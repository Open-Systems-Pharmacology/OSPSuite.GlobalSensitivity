#' @title Bmat
#' @description Function to construct the Bmat matrix as per Morris, 'Factorial Sampling Plans for Preliminary Computational Experiments', 1992.
#' @param k The number of parameter paths.
#' @return Bmat matrix as per Morris, 1991.
Bmat <- function(k) {
  B <- matrix(rep(0, k), nrow = 1)
  for (n in 1:k) {
    B_ <- B[n, ]
    B_[n] <- 1
    B <- rbind(B, B_)
  }
  return(B %>% unname())
}

#' @title Jmat
#' @description Function to construct the Jmat matrix as per Morris, 'Factorial Sampling Plans for Preliminary Computational Experiments', 1992.
#' @param k The number of parameter paths.
#' @return Jmat matrix as per Morris, 1991.
Jmat <- function(k) {
  return(matrix(rep(1, k * (k + 1)), ncol = k))
}

#' @title J1mat
#' @description Function to construct the J1mat matrix as per Morris, 'Factorial Sampling Plans for Preliminary Computational Experiments', 1992.
#' @param k The number of parameter paths.
#' @return J1mat matrix as per Morris, 1991.
J1mat <- function(k) {
  return(matrix(rep(1, k + 1), nrow = k + 1))
}

#' @title Dmat
#' @description Function to construct the Dmat matrix as per Morris, 'Factorial Sampling Plans for Preliminary Computational Experiments', 1992.
#' @param k The number of parameter paths.
#' @return Dmat matrix as per Morris, 1991.
Dmat <- function(k) {
  y <- sample(x = c(1, -1), size = k, replace = TRUE)
  return(diag(x = y, nrow = k))
}

#' @title Pmat
#' @description Function to construct the Pmat matrix as per Morris, 'Factorial Sampling Plans for Preliminary Computational Experiments', 1992.
#' @param k The number of parameter paths.
#' @return Pmat matrix as per Morris, 1991.
Pmat <- function(k) {
  I <- diag(rep(1, k))
  orders <- sample(x = seq(k), size = k, replace = FALSE)
  P <- I[, orders]
}

defaultNumberOfGridPartitions <- 8

#' @title getTrajectory
#' @description Function to construct a trajectory for model evaluations through parameter space as per Morris, 'Factorial Sampling Plans for Preliminary Computational Experiments', 1992.
#' @param numberOfParameters The number of parameter paths.
#' @param numberOfGridPartitions The number of grid partitions, equivalent to parameter `p` in Morris, 1991.
#' @return Trajectory for model evaluations through parameter space as per Morris, 1991.
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


#' @title runMorris
#' @description Function to run Morris sensitivity analysis, robust to individual ODE-solver failures.
#' @param simulation PKML simulation object.
#' @param DDIsimulation DDI PKML simulation object.
#' @param parameters List of `SAParameter` objects.
#' @param outputs List of `SAOutput` objects.
#' @param numberOfSamples The number of *valid* Morris trajectories to collect.
#' @param runParallel  Logical value.  Morris computation is run in parallel when `TRUE`.
#' @param updateProgress Logical value.  Updates shiny app GUI with Morris algorithm progress when `TRUE`.
#' @param saveResults  Logical value.  If `TRUE`, the results will be saved.
#' @param saveFolder String indicating the path to the folder in which the results are to be saved.
#' @param saveFileName String indicating the file name to use when saving the results.
#' @param resampleOnFailure Logical.  If `TRUE` (default), a trajectory that fails to
#'   solve (or yields non-finite PK parameters) is discarded and a fresh trajectory is
#'   drawn to replace it, so the final result still contains `numberOfSamples` valid
#'   trajectories.  If `FALSE`, failed trajectories are simply dropped and the effective
#'   sample size is reduced.
#' @param maxRunAttempts Integer.  Hard cap on the total number of trajectories attempted.
#'   Defaults to `3 * numberOfSamples` when `resampleOnFailure = TRUE`.
#' @param maxConsecutiveFailures Integer.  If this many trajectories fail back-to-back,
#'   abort with an informative error (guards against a globally broken setup).
#' @return Morris sensitivity analysis results.
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
                      saveFileName = NULL,
                      resampleOnFailure = TRUE,
                      maxRunAttempts = NULL,
                      maxConsecutiveFailures = 20) {
  elementaryEffects <- NULL
  numberOfParameters <- length(parameters)
  numberOfTrajectorySteps <- numberOfParameters + 1

  parameterPaths <- sapply(parameters, function(par) par$path)
  names(parameters) <- parameterPaths

  outputPaths <- sapply(outputs, function(op) op$path)
  names(outputs) <- outputPaths

  checkParametersExistInSimulation(
    simulation = simulation, parameterPaths = parameterPaths,
    simulationName = "simulation", stopIfNotFound = TRUE
  )
  checkOutputsExistInSimulation(
    simulation = simulation, outputPaths = outputPaths,
    simulationName = "simulation", stopIfNotFound = TRUE
  )

  if (!is.null(DDIsimulation)) {
    checkParametersExistInSimulation(
      simulation = DDIsimulation, parameterPaths = parameterPaths,
      simulationName = "DDI simulation", stopIfNotFound = TRUE
    )
    checkOutputsExistInSimulation(
      simulation = DDIsimulation, outputPaths = outputPaths,
      simulationName = "DDI simulation", stopIfNotFound = TRUE
    )
  }

  simulation$outputSelections$clear()
  ospsuite::addOutputs(quantitiesOrPaths = outputPaths, simulation = simulation)
  if (!is.null(DDIsimulation)) {
    ospsuite::addOutputs(quantitiesOrPaths = outputPaths, simulation = DDIsimulation)
  }

  simBatches <- getSimulationBatches(
    simulation = simulation, parameterPaths = parameterPaths,
    numberParallelThreads = numberOfParameters + 1
  )
  if (!is.null(DDIsimulation)) {
    DDIsimBatches <- getSimulationBatches(
      simulation = DDIsimulation, parameterPaths = parameterPaths,
      numberParallelThreads = numberOfParameters + 1
    )
  }

  # --- helper: NULL-safe check that every batch produced a solved result --------
  allBatchesSolved <- function(res) {
    if (is.null(res) || length(res) == 0) return(FALSE)
    all(vapply(res, function(x) {
      r <- tryCatch(x[[1]], error = function(e) NULL)
      !is.null(r) && !is.null(r$count) && isTRUE(r$count > 0)
    }, logical(1)))
  }

  if (is.null(maxRunAttempts)) {
    maxRunAttempts <- if (isTRUE(resampleOnFailure)) 3 * numberOfSamples else numberOfSamples
  }
  if (!isTRUE(resampleOnFailure)) {
    maxRunAttempts <- numberOfSamples
  }

  successfulRuns    <- 0
  attempt           <- 0
  failedAttempts    <- 0
  consecutiveFails  <- 0

  while (successfulRuns < numberOfSamples && attempt < maxRunAttempts) {
    attempt   <- attempt + 1
    runNumber <- successfulRuns + 1

    if (is.function(updateProgress)) {
      progressText <- paste("\nWorking on subset", runNumber, "of", numberOfSamples)
      updateProgress(value = runNumber / numberOfSamples, detail = progressText)
    }
    print(paste0("Working on Morris run number ", runNumber, " of ", numberOfSamples,
                 " (attempt ", attempt, " of at most ", maxRunAttempts, ")"))

    # --- one trajectory attempt, fully wrapped so nothing here can abort the SA ---
    attemptResult <- tryCatch({
      # 1. draw trajectory in quantile space, transform to sampled parameter values
      traj <- getTrajectory(numberOfParameters = numberOfParameters)
      Arun <- traj
      for (i in seq_along(parameters)) {
        Arun[, i] <- parameters[[i]]$distribution$quantilesToSample(quantiles = Arun[, i])
        if (!(parameters[[i]]$dimension %in% "Dimensionless")) {
          Arun[, i] <- ospsuite::toBaseUnit(
            quantityOrDimension = parameters[[i]]$dimension,
            values = Arun[, i], unit = parameters[[i]]$unit
          )
        }
      }

      # 2. queue run values on each batch (one per trajectory step)
      for (trajectoryStep in 1:numberOfTrajectorySteps) {
        simBatches[[trajectoryStep]]$addRunValues(parameterValues = Arun[trajectoryStep, ])
        if (!is.null(DDIsimulation)) {
          DDIsimBatches[[trajectoryStep]]$addRunValues(parameterValues = Arun[trajectoryStep, ])
        }
      }

      # 3. simulate
      runResults    <- ospsuite::runSimulationBatches(simulationBatches = simBatches)
      DDIrunResults <- NULL
      if (!is.null(DDIsimulation)) {
        DDIrunResults <- ospsuite::runSimulationBatches(simulationBatches = DDIsimBatches)
      }

      # 4. NULL-safe solver-success check (this is where the old code crashed)
      solved <- allBatchesSolved(runResults) &&
        (is.null(DDIsimulation) || allBatchesSolved(DDIrunResults))
      if (!solved) {
        stop("At least one trajectory step failed to integrate (CVODES failure).")
      }

      # 5. gather results + PK parameters for each step of the trajectory
      thisRun <- vector("list", length(runResults))
      for (r in seq_along(runResults)) {
        node <- list()
        node$simulationResults <- runResults[[r]][[1]]
        if (!is.null(DDIsimulation)) node$DDIsimulationResults <- DDIrunResults[[r]][[1]]

        node$inputParameters <- setNames(
          lapply(seq_along(parameterPaths), function(pn) Arun[r, pn]), parameterPaths
        )

        pkRes <- pkAnalysesToDataFrame(
          ospsuite::calculatePKAnalyses(results = node$simulationResults)
        )
        if (!is.null(DDIsimulation)) {
          DDIpkRes <- pkAnalysesToDataFrame(
            ospsuite::calculatePKAnalyses(results = node$DDIsimulationResults)
          )
        }

        node$outputs <- list()
        for (outPth in names(outputs)) {
          node$outputs[[outPth]] <- list()
          for (pk in outputs[[outPth]]$pkParameterList) {
            val <- pkRes$Value[pkRes$QuantityPath == outPth & pkRes$Parameter == pk]
            # a solved ODE can still yield no / non-finite PK row -> treat as failure
            if (length(val) != 1 || !is.finite(val)) {
              stop(paste0("Missing or non-finite PK parameter '", pk,
                          "' for output '", outPth, "'."))
            }
            node$outputs[[outPth]][[pk]] <- val
            if (!is.null(DDIsimulation)) {
              dval <- DDIpkRes$Value[DDIpkRes$QuantityPath == outPth & DDIpkRes$Parameter == pk]
              ratio <- if (length(dval) == 1 && is.finite(dval)) dval / val else NA_real_
              node$outputs[[outPth]][[paste0(pk, "-DDI-ratio")]] <- ratio
            }
          }
        }
        thisRun[[r]] <- node
      }

      # 6. elementary effects for this trajectory
      eeList <- list()
      for (r in 1:(length(runResults) - 1)) {
        changingInput <- which(traj[r + 1, ] - traj[r, ] != 0)
        currentDelta  <- traj[r + 1, changingInput] - traj[r, changingInput]
        changingInputParameterPath        <- parameters[[changingInput]]$path
        changingInputParameterDisplayName <- parameters[[changingInput]]$displayName

        for (outPth in names(outputs)) {
          outputDisplayName <- outputs[[outPth]]$displayName
          for (pk in names(thisRun[[r]]$outputs[[outPth]])) {
            ee <- (thisRun[[r + 1]]$outputs[[outPth]][[pk]] -
                     thisRun[[r]]$outputs[[outPth]][[pk]]) / currentDelta
            eeList[[length(eeList) + 1]] <- data.frame(
              runNumber                         = runNumber,
              changingInputParameterPath        = changingInputParameterPath,
              changingInputParameterDisplayName = changingInputParameterDisplayName,
              currentDelta                      = currentDelta,
              outputPath                        = outPth,
              outputDisplayName                 = outputDisplayName,
              pkParameter                       = pk,
              elementaryEffect                  = ee,
              stringsAsFactors                  = FALSE
            )
          }
        }
      }
      do.call(rbind, eeList)
    },
    error = function(e) {
      structure(list(message = conditionMessage(e)), class = "morrisRunFailure")
    })

    # --- outcome handling ---------------------------------------------------------
    if (inherits(attemptResult, "morrisRunFailure")) {
      failedAttempts   <- failedAttempts + 1
      consecutiveFails <- consecutiveFails + 1
      warning(paste0("Morris trajectory (attempt ", attempt, ") discarded: ",
                     attemptResult$message))
      if (consecutiveFails >= maxConsecutiveFailures) {
        stop(paste0(consecutiveFails, " consecutive Morris trajectories failed. ",
                    "Aborting - check parameter ranges / model stability. ",
                    "Last error: ", attemptResult$message))
      }
      next
    }

    # success
    elementaryEffects <- rbind.data.frame(elementaryEffects, attemptResult)
    successfulRuns    <- successfulRuns + 1
    consecutiveFails  <- 0
  }

  if (successfulRuns == 0 || is.null(elementaryEffects)) {
    stop("Morris analysis produced no valid trajectories - every attempt failed.")
  }
  if (successfulRuns < numberOfSamples) {
    warning(paste0("Requested ", numberOfSamples, " Morris samples but only ",
                   successfulRuns, " valid trajectories were obtained after ",
                   attempt, " attempts (", failedAttempts, " failed)."))
  } else if (failedAttempts > 0) {
    message(paste0("Morris analysis complete: ", successfulRuns,
                   " valid trajectories (", failedAttempts,
                   " failed trajectories were discarded and resampled)."))
  }

  elementaryEffectsSummary <- aggregate(
    elementaryEffects$elementaryEffect,
    by = list(
      elementaryEffects$changingInputParameterDisplayName,
      elementaryEffects$outputDisplayName,
      elementaryEffects$pkParameter
    ),
    FUN = function(x) x
  )
  names(elementaryEffectsSummary) <- c("Parameter", "Output", "PK", "x")

  for (fnName in names(summaryFunctions)) {
    EEX <- as.matrix(elementaryEffectsSummary$x)
    elementaryEffectsSummary[[fnName]] <- sapply(1:nrow(EEX), function(rowNumber) {
      EEX[rowNumber, ] %>% summaryFunctions[[fnName]]() %>% return()
    })
  }
  elementaryEffectsSummary$x <- NULL
  morrisResults <- list(
    Results  = elementaryEffectsSummary,
    Settings = buildSettingsCMD(parameters = parameters, outputs = outputs)
  )

  if (saveResults) {
    dateTime <- paste0(format(Sys.Date(), "%Y%m%d"), "_", format(Sys.time(), "%H%M%S"))
    if (is.null(saveFileName)) saveFileName <- paste0("morris-summary-", dateTime, ".xlsx")
    if (is.null(saveFolder))   saveFolder   <- getwd()
    writexl::write_xlsx(x = morrisResults, path = file.path(saveFolder, saveFileName))
  }

  print(morrisResults)
  return(morrisResults)
}




#' @title generateMorrisPlot
#' @description Function to generate a plot of Morris sensitivity analysis.
#' @param morrisResults Morris sensitivity results returned by `runMorris` function.
#' @param logPlot Logical setting.  The Morris results are plotted on a logarithmic scale if `TRUE`.
#' @return A list of ggplots of Morris sensitivity analysis results, one corresponding to each output path/PK parameter combination.
#' @export
generateMorrisPlot <- function(morrisResults, logPlot = FALSE) {
  pltFn <- function(x){x}
  if(logPlot){
    pltFn <- log10
  }
  morrisPlots <- list()
  for (outputPath in unique(morrisResults$Output)) {
    morrisPlots[[outputPath]] <- list()
    for (pk in unique(morrisResults[morrisResults$Output == outputPath, ]$PK)) {
      df <- morrisResults[morrisResults$Output == outputPath & morrisResults$PK == pk, ]
      df <- df[rev(order(df$rankingNorm)),]
      df$label <- seq_along(df$Parameter)
      df$label <- as.factor(df$label)
      df$legendLabel <- sapply( 1:nrow(df) , function(nn){ paste0( df$label[nn] , ": " , df$Parameter[nn] ) } )
      plt <- ggplot2::ggplot(data = df, mapping = aes(x = pltFn(mustar), y = pltFn(stdv), color = label, label = label)) +
        ggplot2::geom_point(size = 2) +
        ggplot2::labs(x = paste0("\u03bc", "*"), y = "\u03c3", title = "Morris sensitivity", subtitle = paste0("Output: ", outputPath, "\nPK: ", pk)) +
        ggplot2::scale_color_discrete(name = "Parameter" , labels = df$legendLabel) +
        ggplot2::geom_text(hjust = 0, vjust = 0,size = 6,show.legend = FALSE)
      morrisPlots[[outputPath]][[pk]] <- plt
    }
  }
  return(morrisPlots)
}
