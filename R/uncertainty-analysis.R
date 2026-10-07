#' @title getUncertaintyAnalysisResults
#' @description Function to run uncertainty analysis
#' @param simulation PKML simulation object.
#' @param DDIsimulation DDI PKML simulation object.
#' @param parameters List of `SAParameter` objects.
#' @param outputs List of `SAOutput` objects.
#' @param numberOfUncertaintyAnalysisSamples Number of samples at which to evaluate the simulation for the uncertainty analysis.
#' @param sensitiveParameterPaths Paths of `simulation` parameters that are deemed sensitive.
#' @param runParallel Logical value.  Uncertainty analysis computation is run in parallel when `TRUE`.
#' @param updateProgress Logical value.  Updates shiny app GUI with uncertainty analysis progress when `TRUE`.
#' @return description
#' @keywords internal
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
  U_list <- matrix(data = runif(n = numberOfParameters * numberOfUncertaintyAnalysisSamples), ncol = numberOfParameters)

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

  # Track solver / PK-analysis failures per parameter so that lost Monte Carlo
  # draws are reported at the end rather than silently dropped.
  failureCount <- stats::setNames(rep(0L, length(sensitiveParameterPaths)), sensitiveParameterPaths)
  # Draws whose simulation and PK analysis succeeded but where at least one PK
  # value or DDI ratio could not be extracted or was invalid.
  partialFailureCount <- failureCount

  numberParallelThreads <- 1
  if (runParallel) {
    numberParallelThreads <- (parallel::detectCores() - 1)
  }

  simBatchesListMixed <- list()
  for (pth in sensitiveParameterPaths) {
    simBatchesListMixed[[pth]] <- getSimulationBatches(simulation, pth, numberParallelThreads)
  }

  if (!is.null(DDIsimulation)) {
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

    if (!is.null(DDIsimulation)) {
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
      # A CVODES failure for an individual draw makes that draw's result NULL;
      # a hard failure can also abort the whole batch call.  Neither should stop
      # the analysis, so trap it here and fall through to the NA handling below.
      resMixed[[pth]] <- tryCatch(
        ospsuite::runSimulationBatches(simulationBatches = simBatchesListMixed[[pth]][1:numberOfRowsInSampleBlock]),
        error = function(e) {
          warning(paste0("Uncertainty analysis: simulation batch failed for parameter '", pth, "' in block ", blockNumber, " (", conditionMessage(e), ")."))
          NULL
        }
      )

      if (!is.null(DDIsimulation)) {
        for (r in seq_along(rowNumbersToSim)) {
          DDIsimBatchesListMixed[[pth]][[r]]$addRunValues(parameterValues = U_list[[pth]][rowNumbersToSim[r]])
        }
        DDIresMixed[[pth]] <- tryCatch(
          ospsuite::runSimulationBatches(simulationBatches = DDIsimBatchesListMixed[[pth]][1:numberOfRowsInSampleBlock]),
          error = function(e) {
            warning(paste0("Uncertainty analysis: DDI simulation batch failed for parameter '", pth, "' in block ", blockNumber, " (", conditionMessage(e), ")."))
            NULL
          }
        )
      }

      for (r in seq_along(rowNumbersToSim)) {
        failed <- FALSE

        # runSimulationBatches returns NULL for the whole batch on a hard failure,
        # or a per-simulation result that is NULL when that individual CVODES
        # solve failed.  Guard the indexing so either case resolves to res = NULL.
        res <- tryCatch(resMixed[[pth]][[r]][[1]], error = function(e) NULL)
        if (is.null(res)) {
          failed <- TRUE
        }

        DDIres <- NULL
        if (!is.null(DDIsimulation)) {
          DDIres <- tryCatch(DDIresMixed[[pth]][[r]][[1]], error = function(e) NULL)
          if (is.null(DDIres)) {
            failed <- TRUE
          }
        }

        # PK analysis can itself fail on a marginal profile; treat that as a
        # failed draw rather than letting the exception propagate.
        pkRes <- NULL
        if (!failed) {
          pkRes <- tryCatch(
            suppressWarnings(pkAnalysesToDataFrame(ospsuite::calculatePKAnalyses(results = res))),
            error = function(e) NULL
          )
          if (is.null(pkRes)) {
            failed <- TRUE
          }
        }

        DDIpkRes <- NULL
        if (!failed && !is.null(DDIsimulation)) {
          DDIpkRes <- tryCatch(
            suppressWarnings(pkAnalysesToDataFrame(ospsuite::calculatePKAnalyses(results = DDIres))),
            error = function(e) NULL
          )
          if (is.null(DDIpkRes)) {
            failed <- TRUE
          }
        }

        if (failed) {
          failureCount[[pth]] <- failureCount[[pth]] + 1L
        }

        # Append exactly one value per draw to every PK value and DDI ratio vector
        # (NA when the draw failed or the PK parameter could not be extracted) so
        # that each vector stays the same length as, and aligned 1:1 with, the
        # sampled parameter column U_list[[pth]].  This is what keeps the final
        # assembly from crashing / recycling when a solver failure occurs.
        # pkValueFailed records whether any PK value or DDI ratio of an otherwise
        # successful draw was set to NA, so that the draw is counted once however
        # many of its values failed.
        pkValueFailed <- FALSE
        for (outPth in names(outputs)) {
          for (pk in outputs[[outPth]]$pkParameterList) {
            newPK <- NA_real_
            denom <- NA_real_
            if (!failed) {
              extracted <- pkRes$Value[pkRes$QuantityPath == outPth & pkRes$Parameter == pk]
              # Only accept a single finite value; numeric(0) or NA would break alignment.
              if (length(extracted) == 1L && is.finite(extracted)) {
                newPK <- extracted
                denom <- extracted
              } else {
                pkValueFailed <- TRUE
              }
            }
            fU_list[[pth]][[outPth]][[pk]] <- c(fU_list[[pth]][[outPth]][[pk]], newPK)

            if (!is.null(DDIsimulation)) {
              ratioKey <- paste0(pk, "-DDI-ratio")
              newRatio <- NA_real_
              if (!failed) {
                numer <- DDIpkRes$Value[DDIpkRes$QuantityPath == outPth & DDIpkRes$Parameter == pk]
                if (length(numer) == 1L && is.finite(numer) && is.finite(denom) && denom != 0) {
                  ratio <- numer / denom
                  # Finite, non-zero operands can still overflow to Inf when denom
                  # is very small, so validate the ratio itself.
                  if (is.finite(ratio)) {
                    newRatio <- ratio
                  }
                }
                if (is.na(newRatio)) {
                  pkValueFailed <- TRUE
                }
              }
              fU_list[[pth]][[outPth]][[ratioKey]] <- c(fU_list[[pth]][[outPth]][[ratioKey]], newRatio)
            }
          }
        }

        if (!failed && pkValueFailed) {
          partialFailureCount[[pth]] <- partialFailureCount[[pth]] + 1L
        }
      }
    }

    tictoc::toc()
  }

  # Report how many draws were lost to solver / PK-analysis failures.
  totalFailures <- sum(failureCount)
  if (totalFailures > 0) {
    print(paste0(
      "Uncertainty analysis: ", totalFailures, " of ",
      length(sensitiveParameterPaths) * numberOfUncertaintyAnalysisSamples,
      " draws failed to evaluate and were recorded as NA."
    ))
    for (pth in sensitiveParameterPaths) {
      if (failureCount[[pth]] > 0) {
        print(paste0(
          "  ", pth, ": ", failureCount[[pth]], "/", numberOfUncertaintyAnalysisSamples,
          " (", round(100 * failureCount[[pth]] / numberOfUncertaintyAnalysisSamples, 1), "%)"
        ))
      }
    }
  }

  totalPartialFailures <- sum(partialFailureCount)
  if (totalPartialFailures > 0) {
    print(paste0(
      "Uncertainty analysis: ", totalPartialFailures, " further draws evaluated but had ",
      "at least one PK value or DDI ratio that was missing or invalid (recorded as NA)."
    ))
  }

  # NA counts per parameter / output / PK parameter. Each vector holds exactly
  # one entry per draw, so counting NAs gives the number of draws lost for that
  # output / PK parameter, including whole-draw failures. This shows which
  # metric is responsible for the losses.
  if (totalFailures > 0 || totalPartialFailures > 0) {
    print("Uncertainty analysis: NA count per parameter / output / PK parameter:")
    for (pth in sensitiveParameterPaths) {
      for (outPth in names(fU_list[[pth]])) {
        for (pk in names(fU_list[[pth]][[outPth]])) {
          nNA <- sum(is.na(fU_list[[pth]][[outPth]][[pk]]))
          if (nNA > 0) {
            print(paste0(
              "  ", pth, " / ", outPth, " / ", pk, ": ", nNA, "/", numberOfUncertaintyAnalysisSamples
            ))
          }
        }
      }
    }
  }

  uncertaintyResults <- NULL
  for (parPth in names(fU_list)) {
    for (outPth in names(fU_list[[parPth]])) {
      for (pk in names(fU_list[[parPth]][[outPth]])) {
        values <- fU_list[[parPth]][[outPth]][[pk]]
        # Alignment guard: every vector must hold exactly one value per draw. The NA
        # handling above should guarantee this. If it ever fails, we cannot tell which
        # draws are missing, so padding or trimming could misalign parameter values
        # with PK values. Abort instead.
        if (length(values) != numberOfUncertaintyAnalysisSamples) {
          stop(
            "Uncertainty analysis (internal error): length mismatch for '",
            parPth, "' / '", outPth, "' / '", pk, "' (",
            length(values), " values vs ", numberOfUncertaintyAnalysisSamples, " draws).",
            call. = FALSE
          )
        }

        df <- data.frame(
          parameterValue = U_list[[parPth]],
          parameterPath = parPth,
          outputPath = outPth,
          pkParameter = pk,
          value = values
        )

        uncertaintyResults <- rbind.data.frame(uncertaintyResults, df)
      }
    }
  }

  return(uncertaintyResults)
}
