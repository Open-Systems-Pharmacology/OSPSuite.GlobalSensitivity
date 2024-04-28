
#' @title generateParameterFrequenciesTotal
#' @description Generate a set of frequencies with each parameter as per Saltelli, Tarantola & Chan, 1999.
#' @param parameters List of `SAParameter` objects.
#' @param parameterNumber An integer pointing to a specific parameter in the list of `parameters`.
#' @return A vector of the frequencies associated with each parameter.
generateParameterFrequenciesTotal <- function(parameters, parameterNumber) {
  M <- 4
  parameterFrequencies <- NULL
  Np <- length(parameters)
  freq <- 1
  for (parNumber in seq_len(Np)) {
    if (parNumber == parameterNumber) {
      parameterFrequencies[parNumber] <- 0
      next
    }
    parameterFrequencies[parNumber] <- freq
    freq <- freq + 2
  }
  parameterFrequencies[parameterNumber] <- 2 * M * max(parameterFrequencies) + 1
  names(parameterFrequencies) <- sapply(parameters, function(par) {
    par$path
  })
  return(parameterFrequencies)
}
#' @title getSampleRate
#' @description Select a sampling rate within the parameter space as per Saltelli, Tarantola & Chan, 1999.
#' @param parameterFrequencies A vector of the frequencies associated with each parameter.
#' @return A maximal rate of sampling that satisfies Nyquist-Shannon with respect to the `parameterFrequencies`.


getSampleRate <- function(parameterFrequencies) {
  # Ensure that the sampling rate is significantly higher than twice the maximum frequency of the parameters to satisfy the Nyquist-Shannon sampling theorem.
  # Eqns 21 and 24 in Saltelli, Tarantola, Chan, 1999
  M <- 4
  wMax <- max(parameterFrequencies)
  return(((2 * (wMax * M)) + 1))
}
#' @title getSamplingHypercube
#' @description Build a hypercube of points at which the model will be simulated.
#' @param parameterFrequencies A vector of the frequencies associated with each parameter.
#' @param samplingRate Maximal rate of sampling that satisfies Nyquist-Shannon with respect to the `parameterFrequencies`.
#' @return A hypercube of points in [0,2*pi].

getSamplingHypercube <- function(parameterFrequencies, samplingRate) {
  # Create vector of sampling points based on sampling frequency, which is much greater than twice the maximum parameter frequency as per Nyquist-Shannon
  samplingPoints <- head(seq(0, 1, 1 / samplingRate), -1)
  # Build parameter samples on the unit hypercube
  X <- lapply(parameterFrequencies, function(w) {
    ((2 * pi * w * samplingPoints))
  })
  X <- as.data.frame(X)
  return(X)
}
#' @title mapHypercubeToParameterSpace
#' @description Use parameter distributions to map points in the sampling hypercube to values within the domain of each parameter's distribution.
#' @param parameters List of `SAParameter` objects.
#' @param hypercube A hypercube of points in the percentile space (0,1) that is to be mapped onto the domain of each parameter's distribution.
#' @return A list of points in the domain of each parameter at which the model is to be evaluated.

mapHypercubeToParameterSpace <- function(parameters, hypercube) {
  X <- hypercube
  for (i in seq_along(parameters)) {
    path <- parameters[[i]]$path
    dimension <- parameters[[path]]$dimension
    unit <- parameters[[path]]$unit
    X[[i]] <- parameters[[i]]$distribution$quantilesToSample(quantiles = X[[i]])
    if (!(dimension %in% "Dimensionless")) {
      X[[i]] <- ospsuite::toBaseUnit(
        quantityOrDimension = parameters[[path]]$dimension,
        values = X[[i]],
        unit = parameters[[path]]$unit
      )
    }
  }
  return(X)
}
#' @title perturbHypercube
#' @description Perturb the points within the sampling hypercube as per Saltelli, Tarantola & Chan, 1999.
#' @param hyperCube Hypercube of points in parameter space at which model will be evaluated.
#' @return A perturbation to the points of the original `hypercube`.


perturbHypercube <- function(hyperCube) {
  for (colnm in names(hyperCube)) {
    hyperCube[[colnm]] <- (hyperCube[[colnm]] + runif(1, 0, 2 * pi))
    hyperCube[[colnm]] <- 0.5 + (asin(sin(hyperCube[[colnm]]))) / pi
  }
  return(hyperCube)
}
#' @title getEFASTResultsDf
#' @description Format the EFAST results into a data frame.
#' @param fftEvaluationsList List of evaluated first order and total effect indices.
#' @param outputs List of `SAOutput` objects.



getEFASTResultsDf <- function(fftEvaluationsList, outputs) {
  eFASTResultsDf <- NULL

  for (outPth in names(outputs)) {
    for (pk in names(fftEvaluationsList[[outPth]])) {
      parPth <- names(fftEvaluationsList[[outPth]][[pk]])

      df1 <- data.frame(
        Output = outPth,
        PK = pk,
        Parameter = parPth,
        Value = fftEvaluationsList[[outPth]][[pk]][[parPth]]$S1,
        Measure = "FirstOrder"
      )

      dft <- data.frame(
        Output = outPth,
        PK = pk,
        Parameter = parPth,
        Value = fftEvaluationsList[[outPth]][[pk]][[parPth]]$St,
        Measure = "Total"
      )

      eFASTResultsDf <- rbind.data.frame(eFASTResultsDf, df1, dft)
    }
  }

  return(eFASTResultsDf)
}
#' @title extractPKParametersFromBatchSimulationResults
#' @description Evaluate the PK parameters corresponding to each output in the simulation results obtained from batch mode simulation.
#' @param batchSimulationResults for the main `simulation`
#' @param DDIbatchSimulationResults for the DDI `simulation`
#' @param outputs List of `SAOutput` objects.

extractPKParametersFromBatchSimulationResults <- function(batchSimulationResults, DDIbatchSimulationResults, outputs, pkEvaluationsList) {
  for (r in seq_along(batchSimulationResults)) {
    failed <- FALSE
    res <- batchSimulationResults[[r]][[1]]
    if (is.null(res)) {
      failed <- TRUE
    } else {
      pkRes <- suppressWarnings(pkAnalysesToDataFrame(ospsuite::calculatePKAnalyses(results = res)))
    }

    if (!is.null(DDIbatchSimulationResults)) {
      DDIfailed <- FALSE
      DDIres <- DDIbatchSimulationResults[[r]][[1]]
      if (is.null(DDIres)) {
        DDIfailed <- TRUE
      } else {
        DDIpkRes <- suppressWarnings(pkAnalysesToDataFrame(ospsuite::calculatePKAnalyses(results = DDIres)))
      }
    }

    for (outPth in names(outputs)) {
      for (pk in outputs[[outPth]]$pkParameterList) {
        newPK <- NA
        if (!failed) {
          newPK <- pkRes$Value[pkRes$QuantityPath == outPth & pkRes$Parameter == pk]
        }
        pkEvaluationsList[[outPth]][[pk]] <- c(
          pkEvaluationsList[[outPth]][[pk]],
          newPK
        )


        if (!is.null(DDIbatchSimulationResults)) {
          DDInewPK <- NA
          if (!DDIfailed) {
            DDInewPK <- DDIpkRes$Value[DDIpkRes$QuantityPath == outPth & DDIpkRes$Parameter == pk] / pkRes$Value[pkRes$QuantityPath == outPth & pkRes$Parameter == pk]
          }
          pkEvaluationsList[[outPth]][[paste0(pk, "-DDI-ratio")]] <- c(
            pkEvaluationsList[[outPth]][[paste0(pk, "-DDI-ratio")]],
            DDInewPK
          )
        }
      }
    }
  }
  return(pkEvaluationsList)
}
#' @title runFFT2
#' @description Run the Fast Fourier Transform and the sensitivity indices.
#' @param outputs List of `SAOutput` objects.
#' @param pkEvaluationsList List of evaluated PK parameters for each output.
#' @param parameters List of `SAParameter` objects.
#' @param allFrequencies A vector of all integer frequencies from 0 up to the sampling rate that was used in generating the hypercube over which the pkEvaluationsList was evaluated.
#' @param parameterFrequencies A vector of the frequencies associated with each parameter.
#' @param addHarmonicsForParameterNumber Parameter number for which first order index is to be calculated
#' @return First order and total effect indices of EFAST evaluation.


runFFT2 <- function(outputs,
                    pkEvaluationsList,
                    parameters,
                    fftStructure,
                    allFrequencies,
                    parameterFrequencies,
                    addHarmonicsForParameterNumber) {
  for (outPth in names(outputs)) {
    for (pk in names(pkEvaluationsList[[outPth]])) {
      fftRes <- fft(pkEvaluationsList[[outPth]][[pk]])


      # Parameter number for which first order index is to be calculated
      parNo <- addHarmonicsForParameterNumber

      # Frequency of parameter for which first order index is to be calculated
      w0 <- parameterFrequencies[parNo]

      # Get multiples of w0 in the frequencies in fft spectrum
      multiplesOfParameterFrequencyInSpectrum <- unique(floor(((allFrequencies) / 2) / (parameterFrequencies[parNo])))

      # Calculate contribution to first order index from frequency at w0
      D1 <- (abs(fftRes[(which.min(abs(allFrequencies - w0)))]))^2

      # Calculate contribution to first order index from frequency at multiples of w0 within spectrum of fft
      for (mlt in multiplesOfParameterFrequencyInSpectrum[multiplesOfParameterFrequencyInSpectrum > 1]) {
        D1 <- D1 + (abs(fftRes[(which.min(abs(allFrequencies - (mlt * w0))))]))^2
      }

      # Calculate total indices
      Dt <- sum((abs(fftRes[allFrequencies > 0 & allFrequencies < w0 / 2]))^2)

      # Calculate total variance
      V <- sum((abs(fftRes[allFrequencies > 0 & allFrequencies < max(allFrequencies) / 2]))^2)

      # Normalize indices
      S1 <- D1 / V
      St <- 1 - (Dt / V)

      fftStructure[[outPth]][[pk]] <- list()
      fftStructure[[outPth]][[pk]][[parameters[[parNo]]$path]] <- list(S1 = S1, St = St)
    }
  }
  return(fftStructure)
}
#' @title runEFAST
#' @description Run the EFAST algorithm.
#' @param simulation PKML simulation object.
#' @param DDIsimulation DDI PKML simulation object.
#' @param parameters List of `SAParameter` objects.
#' @param outputs List of `SAOutput` objects.
#' @param runParallel  Logical value.  EFAST sensitivity computation is run in parallel when `TRUE`.
#' @param numberOfResamples Number of times to run the EFAST algorithm steps with resampling, as per Saltelli, Tarantola & Chan, 1999.
#' @param saveResults Logical value.  Results are saved if `TRUE`.
#' @param saveFolder Folder in which results will be saved if `saveResults` is set to `TRUE`.
#' @param saveFileName File name to which results will be saved if `saveResults` is set to `TRUE`.





#' @export
runEFAST <- function(simulation,
                     DDIsimulation = NULL,
                     parameters,
                     outputs,
                     runParallel = TRUE,
                     numberOfResamples = 1,
                     updateProgress = NULL,
                     saveResults = FALSE,
                     saveFolder = NULL,
                     saveFileName = NULL) {
  numberOfParameters <- length(parameters)

  parameterPaths <- sapply(parameters, function(par) {
    par$path
  })
  names(parameters) <- parameterPaths

  outputPaths <- sapply(outputs, function(op) {
    op$path
  })
  names(outputs) <- outputPaths

  checkParametersExistInSimulation(
    simulation = simulation,
    parameterPaths = parameterPaths,
    simulationName = "simulation",
    stopIfNotFound = TRUE
  )

  checkOutputsExistInSimulation(
    simulation = simulation,
    outputPaths = outputPaths,
    simulationName = "simulation",
    stopIfNotFound = TRUE
  )


  if (!is.null(DDIsimulation)) {
    checkParametersExistInSimulation(
      simulation = DDIsimulation,
      parameterPaths = parameterPaths,
      simulationName = "DDI simulation",
      stopIfNotFound = TRUE
    )

    checkOutputsExistInSimulation(
      simulation = DDIsimulation,
      outputPaths = outputPaths,
      simulationName = "DDI simulation",
      stopIfNotFound = TRUE
    )
  }

  numberParallelThreads <- 1
  if (runParallel) {
    numberParallelThreads <- (parallel::detectCores() - 1)
  }

  simulation$outputSelections$clear()
  ospsuite::addOutputs(quantitiesOrPaths = names(outputs), simulation = simulation)
  simBatchesListMixed <- getSimulationBatches(simulation, parameterPaths, numberParallelThreads)
  eFASTResultsDf <- NULL

  if (!is.null(DDIsimulation)) {
    DDIsimulation$outputSelections$clear()
    ospsuite::addOutputs(quantitiesOrPaths = names(outputs), simulation = DDIsimulation)
    DDIsimBatchesListMixed <- getSimulationBatches(DDIsimulation, parameterPaths, numberParallelThreads)
    DDIeFASTResultsDf <- NULL
  }

  # For each parameter, will have a large freq wi while all other parameter freqs small

  stepsSoFar <- 0
  for (parNo in seq_along(parameters)) {
    parameterFrequencies <- generateParameterFrequenciesTotal(parameters = parameters, parameterNumber = parNo)
    samplingRate <- getSampleRate(parameterFrequencies)
    allFrequencies <- (0:(samplingRate - 1))
    X <- getSamplingHypercube(parameterFrequencies = parameterFrequencies, samplingRate = samplingRate)
    names(X) <- parameterPaths


    sampleBlocks <- split(1:length(allFrequencies), ceiling((1:length(allFrequencies)) / (numberParallelThreads)))
    totalSteps <- numberOfResamples * length(sampleBlocks) * length(parameters)

    # Simulate
    # For each resampling
    for (rsm in seq_len(numberOfResamples)) {
      fU_list <- getEvaluationMatrixStructure(outputs)
      fft_list <- getEvaluationMatrixStructure(outputs)
      XPerturbed <- perturbHypercube(X)
      XPerturbed <- mapHypercubeToParameterSpace(parameters = parameters, hypercube = XPerturbed)

      # For each block of rows in hypercube dataframe
      for (sampleBlockNumber in seq_along(sampleBlocks)) {
        stepsSoFar <- stepsSoFar + 1
        progressText <- paste("Working on subset", stepsSoFar, "of", totalSteps)
        print(progressText)
        if (is.function(updateProgress)) {
          updateProgress(value = stepsSoFar / totalSteps, detail = progressText)
        }

        tictoc::tic()

        rowNumbersToSim <- sampleBlocks[[sampleBlockNumber]]
        numberOfRowsInSampleBlock <- length(rowNumbersToSim)

        for (r in seq_along(rowNumbersToSim)) {
          rowNumber <- rowNumbersToSim[r]
          simBatchesListMixed[[r]]$addRunValues(parameterValues = unlist(XPerturbed[rowNumber, ]))

          if (!is.null(DDIsimulation)) {
            DDIsimBatchesListMixed[[r]]$addRunValues(parameterValues = unlist(XPerturbed[rowNumber, ]))
          }
        }

        resList <- ospsuite::runSimulationBatches(simulationBatches = simBatchesListMixed[1:numberOfRowsInSampleBlock])

        DDIresList <- NULL
        if (!is.null(DDIsimulation)) {
          DDIresList <- ospsuite::runSimulationBatches(simulationBatches = DDIsimBatchesListMixed[1:numberOfRowsInSampleBlock])
        }

        fU_list <- extractPKParametersFromBatchSimulationResults(
          batchSimulationResults = resList,
          DDIbatchSimulationResults = DDIresList,
          outputs = outputs,
          pkEvaluationsList = fU_list
        )

        tictoc::toc()
      }

      # If fewer than 50% of simulation runs failed, then interpolate between PK parameter values.  Else run has failed.
      for (outPth in names(fU_list)) {
        for (pk in names(fU_list[[outPth]])) {
          failedPathValues <- ""
          indicesNA <- which(is.na(fU_list[[outPth]][[pk]]))
          if (length(indicesNA) > 0) {
            for (failedNumber in indicesNA) {
              failedPathValues <- paste0(failedPathValues, "Run number ", failedNumber, "\n")
              for (parPath in names(XPerturbed)) {
                failedPathValues <- paste0(failedPathValues, parPath, " = ", XPerturbed[[parPath]][failedNumber], "\n")
              }
            }
            warning(paste0("Evaluation of PK parameter ", pk, " for output path ", outPth, " with respect to parameter ", parameters[[parNo]]$path, " failed for \n", failedPathValues))
          }
          if (length(indicesNA) < 0.5 * length(fU_list[[outPth]][[pk]])) {
            fU_list[[outPth]][[pk]] <- approx(x = seq_along(fU_list[[outPth]][[pk]]), y = fU_list[[outPth]][[pk]], method = "linear", n = length(fU_list[[outPth]][[pk]]))$y
          } else {
            warning(paste0("EFAST sensitivity of ", pk, " of ", outPth, " with respect to parameter ", parameters[[parNo]]$path, " failed.  More than 50% of runs for PK parameter did not complete successfully."))
          }
        }
      }

      # For current resample, run fast Fourier transform on each output and PK parameter and extract absolute value of Fourier coefficients corresponding to each parameter
      fftEvaluationsList <- runFFT2(
        outputs = outputs,
        pkEvaluationsList = fU_list,
        parameters = parameters,
        fftStructure = fft_list,
        allFrequencies = allFrequencies,
        parameterFrequencies = parameterFrequencies,
        addHarmonicsForParameterNumber = parNo
      )

      runEFASTResultsDf <- getEFASTResultsDf(fftEvaluationsList, outputs)
      runEFASTResultsDf$resampleNumber <- rsm
      runEFASTResultsDf$ParameterDisplayName <- parameters[[parNo]]$displayName
      runEFASTResultsDf$OutputDisplayName <- sapply(runEFASTResultsDf$Output, function(path) {
        outputs[[path]]$displayName
      })
      eFASTResultsDf <- rbind.data.frame(eFASTResultsDf, runEFASTResultsDf)
    }
  }

  # Compute average over resampling runs
  aggByNames <- c("Output", "PK", "Parameter", "Measure", "ParameterDisplayName", "OutputDisplayName")

  eFASTResultsDf <- aggregate(eFASTResultsDf$Value,
    by = lapply(aggByNames, function(colnm) {
      eFASTResultsDf[[colnm]]
    }),
    mean
  )
  names(eFASTResultsDf) <- c(aggByNames, "Value")
  eFASTResultsDf <- eFASTResultsDf[, c("Output", "PK", "Parameter", "Measure", "Value", "ParameterDisplayName", "OutputDisplayName")]
  eFASTResults <- list(Results = eFASTResultsDf, Settings = buildSettingsCMD(parameters = parameters, outputs = outputs))

  if (saveResults) {
    dateTime <- paste0(format(Sys.Date(), "%Y%m%d"), "_", format(Sys.time(), "%H%M%S"))

    if (is.null(saveFileName)) {
      saveFileName <- paste0("efast-summary-", dateTime, ".xlsx")
    }

    if (is.null(saveFolder)) {
      saveFolder <- getwd()
    }

    writexl::write_xlsx(
      x = eFASTResults,
      path = file.path(saveFolder, saveFileName)
    )
  }

  print(eFASTResultsDf)
  return(eFASTResults)
}
