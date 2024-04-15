getUncertaintyAnalysisResults <- function(simulation,
                                          DDIsimulation = NULL,
                                          parameters,
                                          outputs,
                                          numberOfUncertaintyAnalysisSamples,
                                          sensitiveParameterPaths,
                                          runParallel = TRUE,
                                          updateProgress = NULL) {
  print("Running uncertainty analysis")

  parameterPaths <- sapply(parameters, function(x) {
    x$path
  })

  error(
    !all(sensitiveParameterPaths %in% parameterPaths),
    paste0("Parameter paths '", paste0(sensitiveParameterPaths[!(sensitiveParameterPaths %in% parameterPaths)], collapse = "', '"), "' not found in the list 'parameters'.")
  )

  numberOfParameters <- length(parameters)
  U_list <- matrix(data = runif(n = numberOfParameters*numberOfUncertaintyAnalysisSamples), ncol = numberOfParameters)

  for (i in seq_along(parameters)) {
    path <- parameterPaths[[i]]
    dimension <- parameters[[path]]$dimension
    unit <- parameters[[path]]$unit
    U_list[, i] <- parameters[[i]]$distribution$quantilesToSample(quantiles = U_list[, i])
    if (!(dimension %in% "Dimensionless")) {
      U_list[, i] <- ospsuite::toBaseUnit(
        quantityOrDimension = parameters[[path]]$dimension,
        values = U_list[, i],
        unit = parameters[[path]]$unit
      )
    }
  }


  U_list <- as.data.frame(U_list)
  names(U_list) <- parameterPaths


  fU_list <- list()
  for (pth in sensitiveParameterPaths) {
    fU_list[[pth]] <- getEvaluationMatrixStructure(outputs)
  }

  numberParallelThreads <- 1
  if (runParallel) {
    numberParallelThreads <- (parallel::detectCores() - 1)
  }

  simBatchesListMixed <- list()
  for (pth in sensitiveParameterPaths) {
    simBatchesListMixed[[pth]] <- getSimulationBatches(simulation, pth, numberParallelThreads)
  }

  if(!is.null(DDIsimulation)){
    DDIsimBatchesListMixed <- list()
    for (pth in sensitiveParameterPaths) {
      DDIsimBatchesListMixed[[pth]] <- getSimulationBatches(DDIsimulation, pth, numberParallelThreads)
    }
  }

  sampleBlocks <- split(1:numberOfUncertaintyAnalysisSamples, ceiling((1:numberOfUncertaintyAnalysisSamples) / numberParallelThreads))

  counter <- 0

  for (blockNumber in seq_along(sampleBlocks)) {
    tictoc::tic()

    resMixed <- list()

    if(!is.null(DDIsimulation)){
      DDIresMixed <- list()
    }

    rowNumbersToSim <- sampleBlocks[[blockNumber]]
    numberOfRowsInSampleBlock <- length(rowNumbersToSim)

    for (pthNumber in seq_along(sensitiveParameterPaths)) {
      pth <- sensitiveParameterPaths[pthNumber]
      print(paste0("Uncertainty analysis: ", round(100 * pthNumber / length(sensitiveParameterPaths)), "% of block ", blockNumber, " out of ", length(sampleBlocks)))

      counter <- counter + 1
      if (is.function(updateProgress)) {
        progressText <- paste("\nWorking on subset", counter, "of", (length(sampleBlocks) * length(sensitiveParameterPaths)))
        updateProgress(value = counter / (length(sampleBlocks) * length(sensitiveParameterPaths)), detail = progressText)
      }

      for (r in seq_along(rowNumbersToSim)) {
        simBatchesListMixed[[pth]][[r]]$addRunValues(parameterValues = U_list[[pth]][rowNumbersToSim[r]])
      }
      resMixed[[pth]] <- ospsuite::runSimulationBatches(simulationBatches = simBatchesListMixed[[pth]][1:numberOfRowsInSampleBlock])

      if(!is.null(DDIsimulation)){
        for (r in seq_along(rowNumbersToSim)) {
          DDIsimBatchesListMixed[[pth]][[r]]$addRunValues(parameterValues = U_list[[pth]][rowNumbersToSim[r]])
        }
        DDIresMixed[[pth]] <- ospsuite::runSimulationBatches(simulationBatches = DDIsimBatchesListMixed[[pth]][1:numberOfRowsInSampleBlock])
      }

      for (r in seq_along(rowNumbersToSim)) {

        failed <- FALSE
        res <- resMixed[[pth]][[r]][[1]]

        if(is.null(res)){
          failed <- TRUE
        }

        if(!is.null(DDIsimulation)){
          DDIres <- DDIresMixed[[pth]][[r]][[1]]
          if(is.null(DDIres)){
            failed <- TRUE
          }
        }

        if(!failed){
          pkRes <- suppressWarnings(pkAnalysesToDataFrame(ospsuite::calculatePKAnalyses(results = res)))
          for (outPth in names(outputs)) {
            for (pk in outputs[[outPth]]$pkParameterList) {
              newPK <- pkRes$Value[pkRes$QuantityPath == outPth & pkRes$Parameter == pk]
              fU_list[[pth]][[outPth]][[pk]] <- c(
                fU_list[[pth]][[outPth]][[pk]],
                newPK
              )
            }
          }

          if(!is.null(DDIsimulation)){
            DDIpkRes <- suppressWarnings(pkAnalysesToDataFrame(ospsuite::calculatePKAnalyses(results = DDIres)))
            for (outPth in names(outputs)) {
              for (pk in outputs[[outPth]]$pkParameterList) {
                DDInewPK <- DDIpkRes$Value[DDIpkRes$QuantityPath == outPth & DDIpkRes$Parameter == pk] / pkRes$Value[pkRes$QuantityPath == outPth & pkRes$Parameter == pk]
                fU_list[[pth]][[outPth]][[paste0(pk,"-DDI-ratio")]] <- c(
                  fU_list[[pth]][[outPth]][[paste0(pk,"-DDI-ratio")]],
                  DDInewPK
                )
              }
            }
          }
        }

      }
    }

    tictoc::toc()
  }


  uncertaintyResults <- NULL
  for (parPth in names(fU_list)){
    for (outPth in names(fU_list[[parPth]])){
      for (pk in names(fU_list[[parPth]][[outPth]])){

        df <- data.frame(
          parameterPath = parPth,
          outputPath = outPth,
          pkParameter = pk,
          value = fU_list[[parPth]][[outPth]][[pk]]
        )

        df <- cbind(data.frame(parameterValue = U_list[[parPth]]) , df)

        uncertaintyResults <- rbind.data.frame(uncertaintyResults,df)

      }
    }
  }

  return(uncertaintyResults)
}
