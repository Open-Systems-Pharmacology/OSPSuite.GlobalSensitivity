Bmat <- function(k) {
  B <- matrix(rep(0, k), nrow = 1)
  for (n in 1:k) {
    B_ <- B[n, ]
    B_[n] <- 1
    B <- rbind(B, B_)
  }
  return(B %>% unname())
}

Jmat <- function(k) {
  return(matrix(rep(1, k * (k + 1)), ncol = k))
}

J1mat <- function(k) {
  return(matrix(rep(1, k + 1), nrow = k + 1))
}

Dmat <- function(k) {
  y <- sample(x = c(1, -1), size = k, replace = TRUE)
  return(diag(x = y, nrow = k))
}

Pmat <- function(k) {
  I <- diag(rep(1, k))
  orders <- sample(x = seq(k), size = k, replace = FALSE)
  P <- I[, orders]
}

defaultNumberOfGridPartitions <- 8

getTrajectory <- function(numberOfParameters, numberOfGridPartitions = defaultNumberOfGridPartitions) {
  if (!(numberOfGridPartitions %% 2) == 0) {
    numberOfGridPartitions <- numberOfGridPartitions + 1
  }

  k <- numberOfParameters
  p <- numberOfGridPartitions
  delta <- p / (2 * (p - 1))
  pts <- seq(0, 1 - delta, 1 / (p - 1))
  xstar <- matrix(sample(x = pts, size = k, replace = TRUE), ncol = k)
  B <- Bmat(k)
  J <- Jmat(k)
  J1 <- J1mat(k)
  D <- Dmat(k)
  P <- Pmat(k)

  Qmat <- ((delta / 2) * ((((2 * B) - J) %*% D) + J))
  Bstar <- ((J1 %*% xstar) + Qmat) %*% P

  Bstar %>% return()
}

getVaryingParameterIndices <- function(trajectory) {
  varyingParameterIndices <- sapply(2:nrow(trajectory), function(nr) {
    which(!(trajectory[nr, ] - trajectory[nr - 1, ] == 0))
  })
}

summaryFunctions <- list(
  mu = function(x) {
    return(mean(x))
  },
  mustar = function(x) {
    return(mean(abs(x)))
  },
  stdv = function(x) {
    return(sd(x))
  },
  rankingNorm = function(x) {
    (c(mean(abs(x)), sd(x)))^2 %>%
      sum() %>%
      sqrt()
  }
)


#' @export
runMorris <- function(simulation,
                      DDIsimulation = NULL,
                      parameters,
                      outputs,
                      numberOfSamples,
                      runParallel = TRUE,
                      updateProgress = NULL,
                      saveResults = FALSE,
                      saveFolder = NULL,
                      saveFileName = NULL) {
  trajectoryQuantiles <- list()
  A <- list()
  morrisResult <- list()
  elementaryEffects <- NULL
  numberOfParameters <- length(parameters)

  parameterPaths <- sapply(parameters, function(par) {
    par$path
  })
  names(parameters) <- parameterPaths

  outputPaths <- sapply(outputs, function(op) {
    op$path
  })
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

  simulation$outputSelections$clear()
  ospsuite::addOutputs(
    quantitiesOrPaths = outputPaths,
    simulation = simulation
  )

  if(!is.null(DDIsimulation)){
    ospsuite::addOutputs(
      quantitiesOrPaths = outputPaths,
      simulation = DDIsimulation
    )
  }

  simBatches <- getSimulationBatches(
    simulation = simulation,
    parameterPaths = parameterPaths,
    numberParallelThreads = numberOfParameters + 1
  )

  if(!is.null(DDIsimulation)){
    DDIsimBatches <- getSimulationBatches(
      simulation = DDIsimulation,
      parameterPaths = parameterPaths,
      numberParallelThreads = numberOfParameters + 1
    )
  }

  for (runNumber in 1:numberOfSamples) {
    if (is.function(updateProgress)) {
      progressText <- paste("\nWorking on subset", runNumber, "of", numberOfSamples)
      updateProgress(value = runNumber / numberOfSamples, detail = progressText)
    }

    print(paste("Working on Morris run number", runNumber, "of", numberOfSamples))
    trajectoryQuantiles[[runNumber]] <- getTrajectory(numberOfParameters = numberOfParameters)
    A[[runNumber]] <- trajectoryQuantiles[[runNumber]]
    morrisResult[[runNumber]] <- list()

    # Generate quantile trajectory for current run and transform quantiles to samples from distributions
    for (i in seq_along(parameters)) {
      A[[runNumber]][, i] <- parameters[[i]]$distribution$quantilesToSample(quantiles = A[[runNumber]][, i])
      if (!(parameters[[i]]$dimension %in% "Dimensionless")) {
        A[[runNumber]][, i] <- ospsuite::toBaseUnit(
          quantityOrDimension = parameters[[i]]$dimension,
          values = A[[runNumber]][, i],
          unit = parameters[[i]]$unit
        )
      }
    }

    # Update each simulation batch with a set of parameter values corresponding to a step in the trajectory
    for (trajectoryStep in 1:(numberOfParameters + 1)) {
      simBatches[[trajectoryStep]]$addRunValues(parameterValues = A[[runNumber]][trajectoryStep, ])

      if(!is.null(DDIsimulation)){
        DDIsimBatches[[trajectoryStep]]$addRunValues(parameterValues = A[[runNumber]][trajectoryStep, ])
      }

    }

    # Simulate model at each step of trajectory for current run
    runResults <- ospsuite::runSimulationBatches(simulationBatches = simBatches)
    if(any(sapply(runResults,function(x){x[[1]]$count}) == 0)){
      stop()
    }

    if(!is.null(DDIsimulation)){
      DDIrunResults <- ospsuite::runSimulationBatches(simulationBatches = DDIsimBatches)
      if(any(sapply(runResults,function(x){x[[1]]$count}) == 0)){
        stop()
      }
    }

    # Gather results for current run
    for (r in seq_along(runResults)) {
      morrisResult[[runNumber]][[r]] <- list()
      morrisResult[[runNumber]][[r]]$simulationResults <- runResults[[r]][[1]]

      if(!is.null(DDIsimulation)){
        morrisResult[[runNumber]][[r]]$DDIsimulationResults <- DDIrunResults[[r]][[1]]
      }

      morrisResult[[runNumber]][[r]]$inputParameters <- lapply(seq_along(parameterPaths), function(parameterNumber) {
        A[[runNumber]][r, parameterNumber]
      })
      names(morrisResult[[runNumber]][[r]]$inputParameters) <- parameterPaths

      pkRes <- pkAnalysesToDataFrame( ospsuite::calculatePKAnalyses(results = morrisResult[[runNumber]][[r]]$simulationResults) )

      if(!is.null(DDIsimulation)){
        DDIpkRes <- pkAnalysesToDataFrame( ospsuite::calculatePKAnalyses(results = morrisResult[[runNumber]][[r]]$DDIsimulationResults) )
      }

      morrisResult[[runNumber]][[r]]$outputs <- list()
      for (outPth in names(outputs)) {
        morrisResult[[runNumber]][[r]]$outputs[[outPth]] <- list()
        for (pk in outputs[[outPth]]$pkParameterList) {
          morrisResult[[runNumber]][[r]]$outputs[[outPth]][[pk]] <- pkRes$Value[pkRes$QuantityPath == outPth & pkRes$Parameter == pk]
          if(!is.null(DDIsimulation)){
            morrisResult[[runNumber]][[r]]$outputs[[outPth]][[paste0(pk,"-DDI-ratio")]] <- DDIpkRes$Value[DDIpkRes$QuantityPath == outPth & DDIpkRes$Parameter == pk]/pkRes$Value[pkRes$QuantityPath == outPth & pkRes$Parameter == pk]
          }
        }
      }
    }

    for (r in 1:(length(runResults) - 1)) {
      changingInput <- which(trajectoryQuantiles[[runNumber]][r + 1, ] - trajectoryQuantiles[[runNumber]][r, ] != 0)
      currentDelta <- trajectoryQuantiles[[runNumber]][r + 1, changingInput] - trajectoryQuantiles[[runNumber]][r, changingInput]
      changingInputParameterPath <- parameters[[changingInput]]$path
      changingInputParameterDisplayName <- parameters[[changingInput]]$displayName

      for (outPth in names(outputs)) {
        outputDisplayName <- outputs[[outPth]]$displayName
        for (pk in  names(morrisResult[[runNumber]][[r]]$outputs[[outPth]])) {
          #CHECK
          #for (pk in outputs[[outPth]]$pkParameterList) {
          ee <- ((morrisResult[[runNumber]][[r + 1]]$outputs[[outPth]][[pk]] - morrisResult[[runNumber]][[r]]$outputs[[outPth]][[pk]])) / (currentDelta)
          df <- data.frame(
            runNumber = runNumber,
            changingInputParameterPath = changingInputParameterPath,
            changingInputParameterDisplayName = changingInputParameterDisplayName,
            currentDelta = currentDelta,
            outputPath = outPth,
            outputDisplayName = outputDisplayName,
            pkParameter = pk,
            elementaryEffect = ee
          )
          elementaryEffects <- rbind.data.frame(elementaryEffects, df)
        }
      }
    }
  }

  elementaryEffectsSummary <- aggregate(elementaryEffects$elementaryEffect, by = list(
    elementaryEffects$changingInputParameterDisplayName,
    elementaryEffects$outputDisplayName,
    elementaryEffects$pkParameter
  ), FUN = function(x) {
    x
  })
  names(elementaryEffectsSummary) <- c("Parameter", "Output", "PK", "x")

  for (fnName in names(summaryFunctions)) {
    EEX <- as.matrix(elementaryEffectsSummary$x)
    elementaryEffectsSummary[[fnName]] <- sapply(1:nrow(EEX), function(rowNumber) {
      EEX[rowNumber, ] %>%
        summaryFunctions[[fnName]]() %>%
        return()
    })
  }
  elementaryEffectsSummary$x <- NULL
  morrisResults <- list(Results = elementaryEffectsSummary, Settings = buildSettingsCMD(parameters = parameters, outputs = outputs))

  if(saveResults){

    dateTime <- paste0(format(Sys.Date(), "%Y%m%d"), "_", format(Sys.time(), "%H%M%S"))

    if(is.null(saveFileName)){
      saveFileName <- paste0("morris-summary-", dateTime, ".xlsx" )
    }

    if (is.null(saveFolder)) {
      saveFolder <- getwd()
    }

    writexl::write_xlsx(x = morrisResults,
                        path = file.path(saveFolder,saveFileName))

  }

  print(morrisResults)
  return(morrisResults)
}

#' @export
generateMorrisPlot <- function(morrisResults) {
  morrisPlots <- list()
  for (outputPath in unique(morrisResults$Output)) {
    morrisPlots[[outputPath]] <- list()
    for (pk in unique(morrisResults[morrisResults$Output == outputPath, ]$PK)) {
      df <- morrisResults[morrisResults$Output == outputPath & morrisResults$PK == pk, ]

      plt <- ggplot2::ggplot(data = df, mapping = aes(x = mustar, y = stdv, color = Parameter, label = Parameter)) +
        ggplot2::geom_point() +
        ggplot2::labs(x = paste0("\u03bc", "*"), y = "\u03c3", title = "Morris plot", subtitle = paste0("Output ", outputPath, "\nPK: ", pk)) +
        geom_text(hjust = 0, vjust = 0)
      morrisPlots[[outputPath]][[pk]] <- plt
    }
  }
  return(morrisPlots)
}
