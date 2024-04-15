getEvaluationMatrixStructure <- function(outputList) {
  f <- list()
  for (pth in names(outputList)) {
    f[[pth]] <- list()
  }
  return(f)
}

getMixedMatrices <- function(A, B, parameterPaths) {
  U <- list()
  for (pthNumber in seq_along(parameterPaths)) {
    pth <- parameterPaths[[pthNumber]]
    U[[pth]] <- B
    U[[pth]][, pthNumber] <- A[, pthNumber]
  }
  return(U)
}

getSimulationBatches <- function(simulation, parameterPaths, numberParallelThreads) {
  sapply(1:numberParallelThreads, function(row) {
    ospsuite::createSimulationBatch(
      simulation = simulation,
      parametersOrPaths = parameterPaths
    )
  })
}






varcalc <- function(fU_list, outputList) {
  parameterPathsAB <- names(fU_list)
  parameterPaths <- setdiff(parameterPathsAB, c("A", "B"))

  invalidEntries <- c(NaN,Inf,NA)

  f_02 <- getEvaluationMatrixStructure(outputList)
  D_0 <- getEvaluationMatrixStructure(outputList)
  S_1 <- getEvaluationMatrixStructure(outputList)
  S_T <- getEvaluationMatrixStructure(outputList)

  for (outPth in names(outputList)) {
    for(pk in names(fU_list[["A"]][[outPth]])){
      #CHECK
      #for (pk in outputList[[outPth]]$pkParameterList) {
      fA <- fU_list[["A"]][[outPth]][[pk]]
      fB <- fU_list[["B"]][[outPth]][[pk]]

      #Remove Inf values
      infIndices <- (abs(fA) %in% invalidEntries) | (abs(fB) %in% invalidEntries)
      for (parPth in parameterPaths) {
        fMixed <- fU_list[[parPth]][[outPth]][[pk]]
        infIndices <- infIndices | (abs(fMixed) %in% invalidEntries)
      }
      fA <- fA[!infIndices]
      fB <- fB[!infIndices]

      N <- nrow(fA)

      f_02 <- (mean(fA))^2 # Mean squared
      D_0 <- (mean(fA * fA)) - f_02 # Total variance

      S_1[[outPth]][[pk]] <- list()
      S_T[[outPth]][[pk]] <- list()

      for (parPth in parameterPaths) {
        fMixed <- fU_list[[parPth]][[outPth]][[pk]]
        fMixed <- fMixed[!infIndices]
        pD <- mean(fA * fMixed)
        pT <- mean(fB * fMixed)
        D_all <- max(pD - f_02,0)
        T_all <- max(D_0 - (pT - f_02),0)
        S_1[[outPth]][[pk]][[parPth]] <- min(1,D_all/D_0) # Calculate relative first order effects
        S_T[[outPth]][[pk]][[parPth]] <- min(1,max(T_all,D_all)/D_0) # Calculate relative total effects
      }
    }
  }

  sobolResultsList <- list("FirstOrder" = S_1, "Total" = S_T)

  sobolResultsDf <- NULL



  for (outPth in names(outputList)) {
    for(pk in names(fU_list[["A"]][[outPth]])){
      #CHECK
      #for (pk in outputList[[outPth]]$pkParameterList) {
      for (parPth in parameterPaths) {
        for (resType in names(sobolResultsList)) {
          df <- data.frame(
            Output = outPth,
            PK = pk,
            Parameter = parPth,
            Measure = resType,
            Value = sobolResultsList[[resType]][[outPth]][[pk]][[parPth]]
          )
          sobolResultsDf <- rbind.data.frame(sobolResultsDf, df)
        }
      }
    }
  }
  sobolResultsDf$Value[sobolResultsDf$Value < 0] <- 0
  return(sobolResultsDf)
}




#' @export
runSobol <- function(simulation,
                     DDIsimulation = NULL,
                     parameters,
                     outputs,
                     numberOfSamples,
                     runParallel = TRUE,
                     updateProgress = NULL,
                     saveResults = FALSE,
                     saveFolder = NULL,
                     saveFileName = NULL) {

  numberOfParameters <- length(parameters)

  parameterPaths <- sapply(parameters,function(par){par$path})
  names(parameters) <- parameterPaths

  outputPaths <- sapply(outputs, function(op){op$path})
  names(outputs) <- outputPaths

  checkParametersExistInSimulation(simulation = simulation,
                                   parameterPaths = parameterPaths,
                                   simulationName =  "simulation",
                                   stopIfNotFound = TRUE)

  checkOutputsExistInSimulation(simulation = simulation,
                                outputPaths = outputPaths,
                                simulationName =  "simulation",
                                stopIfNotFound = TRUE)

  if(!is.null(DDIsimulation)){
    checkParametersExistInSimulation(simulation = DDIsimulation,
                                     parameterPaths = parameterPaths,
                                     simulationName =  "DDI simulation",
                                     stopIfNotFound = TRUE)

    checkOutputsExistInSimulation(simulation = DDIsimulation,
                                  outputPaths = outputPaths,
                                  simulationName =  "DDI simulation",
                                  stopIfNotFound = TRUE)
  }

  sobolSeq <- sobolSequence.points(dimR=2*numberOfParameters, count=numberOfSamples)
  A <- as.matrix(sobolSeq[,1:numberOfParameters])
  B <- as.matrix(sobolSeq[, (numberOfParameters+1) : (2*numberOfParameters)])

  for (i in seq_along(parameters)) {
    path <- parameterPaths[[i]]

    dimension <- parameters[[path]]$dimension
    unit <- parameters[[path]]$unit

    A[, i] <- parameters[[i]]$distribution$quantilesToSample(quantiles = A[, i])
    B[, i] <- parameters[[i]]$distribution$quantilesToSample(quantiles = B[, i])


    if(!(dimension %in% "Dimensionless")){
      A[, i] <- ospsuite::toBaseUnit(quantityOrDimension = parameters[[path]]$dimension,
                                     values = A[, i],
                                     unit = parameters[[path]]$unit)

      B[, i] <- ospsuite::toBaseUnit(quantityOrDimension = parameters[[path]]$dimension,
                                     values = B[, i],
                                     unit = parameters[[path]]$unit)
    }

  }

  if (any(c("A", "B") %in% parameterPaths)) {
    stop("Parameter paths must not be 'A' or 'B'")
  }

  parameterPathsAB <- c("A", "B", parameterPaths)

  U_list <- c(list("A" = A, "B" = B), getMixedMatrices(A, B, parameterPaths))

  numberParallelThreads <- 1
  if (runParallel) {
    numberParallelThreads <- (parallel::detectCores() - 1)
  }
  sampleBlocks <- split(1:numberOfSamples, ceiling((1:numberOfSamples) / numberParallelThreads))

  simBatchesListMixed <- list()
  simulation$outputSelections$clear()
  ospsuite::addOutputs(quantitiesOrPaths = names(outputs), simulation = simulation)
  for (pth in parameterPathsAB) {
    simBatchesListMixed[[pth]] <- getSimulationBatches(simulation, parameterPaths, numberParallelThreads)
  }

  if(!is.null(DDIsimulation)){
    DDIsimBatchesListMixed <- list()
    DDIsimulation$outputSelections$clear()
    ospsuite::addOutputs(quantitiesOrPaths = names(outputs), simulation = DDIsimulation)
    for (pth in parameterPathsAB) {
      DDIsimBatchesListMixed[[pth]] <- getSimulationBatches(DDIsimulation, parameterPaths, numberParallelThreads)
    }
  }

  fU_list <- lapply(parameterPathsAB, function(pth) {
    getEvaluationMatrixStructure(outputs)
  })
  names(fU_list) <- parameterPathsAB

  for (n in seq_along(sampleBlocks)) {

    if(is.function(updateProgress)){
      progressText <- paste("\nWorking on subset",n,"of",length(sampleBlocks))
      updateProgress(value = n/length(sampleBlocks),detail = progressText)
    }

    print(paste("Simulating block", n, "of", length(sampleBlocks)))

    tictoc::tic()

    rowNumbersToSim <- sampleBlocks[[n]]
    numberOfRowsInSampleBlock <- length(rowNumbersToSim)

    for (r in seq_along(rowNumbersToSim)) {
      rowNumber <- rowNumbersToSim[r]
      for (pth in parameterPathsAB) {
        simBatchesListMixed[[pth]][[r]]$addRunValues(parameterValues = U_list[[pth]][rowNumber, ])

        if(!is.null(DDIsimulation)){
          DDIsimBatchesListMixed[[pth]][[r]]$addRunValues(parameterValues = U_list[[pth]][rowNumber, ])
        }

      }
    }

    resMixed <- list()
    if(!is.null(DDIsimulation)){
      DDIresMixed <- list()
    }
    for (pth in parameterPathsAB) {

      resMixed[[pth]] <- ospsuite::runSimulationBatches(simulationBatches = simBatchesListMixed[[pth]][1:numberOfRowsInSampleBlock])

      if(!is.null(DDIsimulation)){

        DDIresMixed[[pth]] <- ospsuite::runSimulationBatches(simulationBatches = DDIsimBatchesListMixed[[pth]][1:numberOfRowsInSampleBlock])

      }

    }


    for (r in seq_along(rowNumbersToSim)) {
      for (parPth in parameterPathsAB) {

        failed <- FALSE
        res <- resMixed[[parPth]][[r]][[1]]
        if(is.null(res)){
          failed <- TRUE
        }

        if(!is.null(DDIsimulation)){
          DDIres <- DDIresMixed[[parPth]][[r]][[1]]
          if(is.null(DDIres)){
            failed <- TRUE
          }
        }

        if(!failed){
          pkRes <- suppressWarnings(pkAnalysesToDataFrame(ospsuite::calculatePKAnalyses(results = res)))
        }

        if(!is.null(DDIsimulation)){
          if(!failed){
            DDIpkRes <- suppressWarnings(pkAnalysesToDataFrame(ospsuite::calculatePKAnalyses(results = DDIres)))
          }
        }

        for (outPth in names(outputs)) {
          for (pk in outputs[[outPth]]$pkParameterList) {

            newPK <- NA
            if(!failed){
              newPK <- pkRes$Value[pkRes$QuantityPath == outPth & pkRes$Parameter == pk]
            }

            fU_list[[parPth]][[outPth]][[pk]] <- c(  fU_list[[parPth]][[outPth]][[pk]] ,  newPK )

            if(!is.null(DDIsimulation)){
              DDInewPK <- NA
              if(!failed){
                DDInewPK <- DDIpkRes$Value[DDIpkRes$QuantityPath == outPth & DDIpkRes$Parameter == pk]/pkRes$Value[pkRes$QuantityPath == outPth & pkRes$Parameter == pk]
              }
              fU_list[[parPth]][[outPth]][[paste0(pk, "-DDI-ratio")]] <- c( fU_list[[parPth]][[outPth]][[paste0(pk, "-DDI-ratio")]], DDInewPK )
            }
          }
        }
      }

    }
    tictoc::toc()
  }

  sobolResultsDf <- varcalc(fU_list, outputs)
  sobolResultsDf$ParameterDisplayName <- sapply(sobolResultsDf$Parameter,function(path){parameters[[path]]$displayName})
  sobolResultsDf$OutputDisplayName <- sapply(sobolResultsDf$Output,function(path){outputs[[path]]$displayName})

  sobolResults <- list(Results = sobolResultsDf, Settings = buildSettingsCMD(parameters = parameters, outputs = outputs))

  if(saveResults){

    dateTime <- paste0(format(Sys.Date(), "%Y%m%d"), "_", format(Sys.time(), "%H%M%S"))

    if(is.null(saveFileName)){
      saveFileName <- paste0("sobol-summary-", dateTime, ".xlsx" )
    }

    if (is.null(saveFolder)) {
      saveFolder <- getwd()
    }

    writexl::write_xlsx(x = sobolResults,
                        path = file.path(saveFolder,saveFileName))

  }

  print(sobolResultsDf)
  return(sobolResults)
}
