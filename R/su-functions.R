#' @title getUncertaintyClass
#' @description Function to classify uncertainty analysis result.
#' @param uncertaintyValue is a non-negative numeric value.
#' @return Uncertainty class.
#' @keywords internal
getUncertaintyClass <- function(uncertaintyValue) {
  if (is.na(uncertaintyValue)) {
    return(NA)
  }

  if (is.nan(uncertaintyValue)) {
    return(NA)
  }

  absVal <- abs(uncertaintyValue)

  if (absVal >= 2) {
    return("High")
  }

  if (absVal >= 0.3) {
    return("Medium")
  }

  return("Low")
}

#' @title getUncertaintyStatistics
#' @description Function to summarize the Monte Carlo draws of one parameter / output / PK parameter combination.
#' Failed draws (`NA`) are excluded.  If fewer than `minimumValidDraws` draws remain, every statistic is `NA`
#' (undefined): missing evidence is never reported as zero uncertainty.
#' @param values Numeric vector with one entry per draw, `NA` for draws that failed.
#' @param minimumValidDraws Minimum number of valid draws required to compute the statistics.
#' @param quantiles Additional quantiles to evaluate.
#' @return A list with `defined`, `ValidDraws`, `Mean`, `Percentile_2.5`, `Median`, `Percentile_97.5`,
#' `UncertaintyRatio` and `quantileValues` (named by `as.character(quantiles)`).
#' @keywords internal
getUncertaintyStatistics <- function(values, minimumValidDraws, quantiles = NULL) {
  validValues <- values[is.finite(values)]
  numberOfValidDraws <- length(validValues)

  quantileValues <- stats::setNames(as.list(rep(NA_real_, length(quantiles))), as.character(quantiles))
  uaStats <- list(
    defined = FALSE,
    ValidDraws = numberOfValidDraws,
    Mean = NA_real_,
    Percentile_2.5 = NA_real_,
    Median = NA_real_,
    Percentile_97.5 = NA_real_,
    UncertaintyRatio = NA_real_,
    quantileValues = quantileValues
  )

  if (numberOfValidDraws < minimumValidDraws || numberOfValidDraws < 2) {
    return(uaStats)
  }

  uaStats$defined <- TRUE
  uaStats$Mean <- mean(validValues)
  uaStats$Percentile_2.5 <- unname(stats::quantile(validValues, 0.025))
  uaStats$Median <- unname(stats::quantile(validValues, 0.5))
  uaStats$Percentile_97.5 <- unname(stats::quantile(validValues, 0.975))
  spread <- uaStats$Percentile_97.5 - uaStats$Percentile_2.5
  # Zero spread among enough valid draws is genuinely zero uncertainty, even when
  # the median is also zero (0 / 0).  A non-zero spread around a zero median is
  # left as +/-Inf, i.e. unbounded relative uncertainty.
  uaStats$UncertaintyRatio <- if (spread == 0) 0 else spread / uaStats$Median
  for (qv in quantiles) {
    uaStats$quantileValues[[as.character(qv)]] <- unname(stats::quantile(validValues, qv))
  }
  return(uaStats)
}

#' @title getSensitivityClass
#' @description Function to classify sensitivity analysis result.
#' @param sensitivityValue is a non-negative numeric value.
#' @return Sensitivity class.
#' @keywords internal
getSensitivityClass <- function(sensitivityValue) {
  if (is.na(sensitivityValue)) {
    return(NA)
  }

  if (is.nan(sensitivityValue)) {
    return(NA)
  }

  absVal <- abs(sensitivityValue)

  if (absVal >= 0.5) {
    return("High")
  }

  if (absVal >= 0.2) {
    return("Medium")
  }

  if (absVal >= 0.1) {
    return("Low")
  }

  return("Negligible")
}


#' @title getParameterListForUncertaintyAnalysis
#' @description This function assigns a log-uniform distribution with a variation range given by `variationRange` for parameters with a previously unspecified distribution.
#' @param parameterPaths Paths of the parameters in the `simulation`.
#' @param simulation The simulation object which contains parameters at paths `parameterPaths`.
#' @param variationRange The variation range used to define the log-uniform distribution of the parameters in `parameterPaths`.
#' @return A list of `SAParameter` objects.
#' @keywords internal
getParameterListForUncertaintyAnalysis <- function(parameterPaths, simulation, variationRange = 0.1) {
  parameters <- list()

  # Get all valid parameters from simulation

  for (path in parameterPaths) {
    ospParameter <- ospsuite::getParameter(path = path, container = simulation)

    if (is.nan(ospParameter$value)) {
      next
    }

    if (ospParameter$value == 0) {
      next
    }

    parameters <- c(parameters, SAParameter$new(
      simulation = simulation,
      path = path,
      defaultVariationRangeForLogUniformDistributions = variationRange
    ))
  }

  # Name parameters list with paths of each parameter
  names(parameters) <- sapply(parameters, function(x) {
    x$path
  })

  return(parameters)
}

#' @title runSU
#' @description Function to run sensitivity analysis and then run uncertainy analysis for parameters deemed sensitive.
#' Main parameters:
#' @param simulation PKML simulation object.
#' @param DDIsimulation DDI PKML simulation object.
#' @param customParameters List of `SAParameter` objects with user-specified distributions.
#' @param outputs List of `SAOutput` objects.
#' @param evaluateForAllParameters Logical value.  If `TRUE`, all model parameters will be evaluated for sensitivity and those with a sensitivity greater than `sensitivityThreshold` will undergo uncertainty analysis.  Any parameters not included in the `customParameters` list will have a log-uniform distribution with bounds set by `variationRange`.
#' Sensitivity analysis settings:
#' @param variationRange The variation range used to define the log-uniform distribution of the parameters in `parameterPaths`.
#' @param numberOfSensitivityAnalysisSteps Positive integer.  Number of steps at which the model is simulated within a parameter's `variationRange` during the sensitivity analysis.
#' @param sensitivityThreshold Positive numeric value.  The threshold sensitivity above which a parameter will undergo uncertainty analysis if `runUncertaintyAnalysis` and `runUncertaintlyOnlyForSensitiveParameters` are both `TRUE`.
#' Uncertainty analysis settings:
#' @param runUncertaintyAnalysis Logical value.  If `TRUE`, uncertainty analysis will run.
#' @param runUncertaintlyOnlyForSensitiveParameters  Logical value.  If `TRUE`, uncertainty analysis will only be run for the parameters for which sensitivity exceeds `sensitivityThreshold`.
#' @param numberOfUncertaintyAnalysisSamples Positive integer value giving the number of Monte Carlo runs for each parameter.
#' @param quantiles  Vector of numerical values between 0 and 1.  The `quantiles` input sets the percentiles of the PK parameters to be evaluated in addition to the 50% and 95% percentiles, which are computed by default.
#' Save settings:
#' @param saveResults  Logical value.  If `TRUE`, the results will be saved.
#' @param saveFolder String indicating the path to the folder in which the results are to be saved.
#' @param saveFileName String indicating the file name to use when saving the results.
#'Parallel run and updating settings
#' @param runParallel  Logical value.  Sensitivity computation is run in parallel when `TRUE`.
#' @param updateProgressSensitivity Logical value.  Updates shiny app GUI with sensitivity analysis progress when `TRUE`.
#' @param updateProgressUncertainty Logical value.  Updates shiny app GUI with uncertainty analysis progress when `TRUE`.
#' @param minimumValidDrawFraction Numeric value in (0, 1].  Minimum fraction of the `numberOfUncertaintyAnalysisSamples` draws that must evaluate successfully for a parameter / output / PK parameter combination.  Below this (or below 2 valid draws), the uncertainty summary statistics and `UncertaintyRatio` are reported as `NA` (undefined) rather than computed from the surviving draws.
#' @return Sensitivity and uncertainty analysis results.
#' @export
runSU <- function(simulation,
                  DDIsimulation = NULL,
                  customParameters = NULL,
                  outputs,
                  evaluateForAllParameters = FALSE,
                  ## Sensitivity analysis settings
                  variationRange = 0.1,
                  numberOfSensitivityAnalysisSteps = 2,
                  sensitivityThreshold = 0.1,
                  ## Uncetainty analysis settings
                  runUncertaintyAnalysis = TRUE,
                  runUncertaintlyOnlyForSensitiveParameters = TRUE,
                  numberOfUncertaintyAnalysisSamples = 100,
                  quantiles = c(0.05, 0.5, 0.95),
                  ## Save settings
                  saveResults = FALSE,
                  saveFolder = NULL,
                  saveFileName = NULL,
                  # Parallel run and updating settings
                  runParallel = TRUE,
                  updateProgressSensitivity = NULL,
                  updateProgressUncertainty = NULL,
                  # Uncertainty draw validity setting (appended to preserve positional argument order)
                  minimumValidDrawFraction = 0.9) {
  print("Loading parameters.")
  error(condition = is.null(outputs), errorMessage = "No model outputs have been selected.")
  error(!(variationRange > 0 & variationRange < 1), paste0("Argument 'variationRange' must lie strictly between 0 and 1."))
  error(!(numberOfSensitivityAnalysisSteps %% 1 == 0), paste0("Argument 'numberOfSensitivityAnalysisSteps' must be an integer."))
  error(!(numberOfSensitivityAnalysisSteps > 0), paste0("Argument 'numberOfSensitivityAnalysisSteps' must be positive."))
  error(any(quantiles > 1), "Elements of the vector 'quantiles' must be less than 1.")
  error(any(quantiles < 0), "Elements of the vector 'quantiles' must be greater than 0.")
  error(!(is.numeric(minimumValidDrawFraction) && length(minimumValidDrawFraction) == 1 && minimumValidDrawFraction > 0 && minimumValidDrawFraction <= 1), "Argument 'minimumValidDrawFraction' must be a single number in (0, 1].")

  # Set nominal parameter values in simulation based on any customParameters
  parameterPaths <- NULL
  customParameterPaths <- NULL
  originalParameterValues <- NULL
  if (!is.null(customParameters)) {
    names(customParameters) <- sapply(customParameters, function(x) {
      x$path
    })
    customParameterPaths <- names(customParameters)

    originalParameterValues <- sapply(customParameterPaths, function(pth) {
      return(ospsuite::getParameter(path = pth, container = simulation)$value)
    })

    ospsuite::setParameterValuesByPath(
      simulation = simulation,
      parameterPaths = customParameterPaths,
      values = sapply(customParameterPaths, function(pth) {
        medianValue <- customParameters[[pth]]$distribution$quantilesToSample(quantiles = 0.5)
        medianValueInBaseUnits <- ospsuite::toBaseUnit(
          quantityOrDimension = customParameters[[pth]]$dimension,
          values = medianValue,
          unit = customParameters[[pth]]$unit
        )
        return(medianValueInBaseUnits)
      })
    )

    if (!is.null(DDIsimulation)) {
      ospsuite::setParameterValuesByPath(
        simulation = DDIsimulation,
        parameterPaths = customParameterPaths,
        values = sapply(customParameterPaths, function(pth) {
          medianValue <- customParameters[[pth]]$distribution$quantilesToSample(quantiles = 0.5)
          medianValueInBaseUnits <- ospsuite::toBaseUnit(
            quantityOrDimension = customParameters[[pth]]$dimension,
            values = medianValue,
            unit = customParameters[[pth]]$unit
          )
          return(medianValueInBaseUnits)
        })
      )
    }
  }

  additionalParameterPaths <- NULL
  if (evaluateForAllParameters) {
    additionalParameterPaths <- sapply(ospsuite::getAllParametersForSensitivityAnalysisMatching(paths = "**", simulation = simulation), function(x) {
      x$path
    })
    additionalParameterPaths <- setdiff(additionalParameterPaths, customParameterPaths)
  }

  # Set outputs
  outputPaths <- sapply(outputs, function(op) {
    op$path
  })
  names(outputs) <- outputPaths


  checkParametersExistInSimulation(
    simulation = simulation,
    parameterPaths = c(additionalParameterPaths, customParameterPaths),
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
      parameterPaths = c(additionalParameterPaths, customParameterPaths),
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

  simulation$outputSelections$clear()
  ospsuite::addOutputs(quantitiesOrPaths = names(outputs), simulation = simulation)


  if (!is.null(DDIsimulation)) {
    DDIsimulation$outputSelections$clear()
    ospsuite::addOutputs(quantitiesOrPaths = names(outputs), simulation = DDIsimulation)
  }

  allParameterPaths <- c(customParameterPaths, additionalParameterPaths)
  sensitivityDataframe <- getLocalSensitivity(simulation,
    DDIsimulation = DDIsimulation,
    parameterPaths = allParameterPaths,
    outputs,
    variationRange = variationRange,
    numberOfSteps = numberOfSensitivityAnalysisSteps,
    runParallel = TRUE,
    updateProgress = updateProgressSensitivity
  )

  sensitiveParameterPaths <- allParameterPaths
  if (runUncertaintlyOnlyForSensitiveParameters) {
    sensitivityDataframe <- na.omit(sensitivityDataframe)
    sensitiveParameterPaths <- as.character(unique(sensitivityDataframe$parameterPath[abs(sensitivityDataframe$sensitivity) > sensitivityThreshold]))
    sensitivityDataframe <- sensitivityDataframe[sensitivityDataframe$parameterPath %in% sensitiveParameterPaths, ]
  }

  parameters <- customParameters
  if (evaluateForAllParameters) {
    parameters <- c(
      parameters,
      getParameterListForUncertaintyAnalysis(additionalParameterPaths, simulation, variationRange = variationRange)
    )
  }
  if (runUncertaintyAnalysis) {
    uncertaintyAnalysisResults <- getUncertaintyAnalysisResults(simulation,
      DDIsimulation = DDIsimulation,
      parameters,
      outputs,
      numberOfUncertaintyAnalysisSamples,
      sensitiveParameterPaths,
      runParallel = TRUE,
      updateProgress = updateProgressUncertainty
    )
  }

  # A summary is only computed from draws that evaluated successfully; below this
  # count the result is reported as undefined rather than from a small survivor set.
  minimumValidDraws <- max(2L, ceiling(minimumValidDrawFraction * numberOfUncertaintyAnalysisSamples))
  undefinedUncertaintyResults <- list()

  # Build summary of sensitivity and uncertainty results
  allSUResults <- NULL
  suResultsList <- list()
  for (outPth in names(outputs)) {
    suResultsList[[outPth]] <- list()

    pkNames <- unique(sensitivityDataframe$pkParameter[sensitivityDataframe$outputPath == outPth])
    if (runUncertaintyAnalysis) {
      pkNames <- unique(c(
        sensitivityDataframe$pkParameter[sensitivityDataframe$outputPath == outPth],
        uncertaintyAnalysisResults$pkParameter[uncertaintyAnalysisResults$outputPath == outPth]
      ))
    }


    for (pk in pkNames) {

      suResultsList[[outPth]][[pk]] <- NULL

      for (pth in sensitiveParameterPaths) {
        sensitivityValue <- sensitivityDataframe$sensitivity[sensitivityDataframe$parameterPath == pth & sensitivityDataframe$outputPath == outPth & sensitivityDataframe$pkParameter == pk]
        if (length(sensitivityValue) == 0) {
          sensitivityValue <- NA
        }

        df <- data.frame(
          Output = outPth,
          OutputDisplayName = outputs[[outPth]]$displayName,
          PK = pk,
          Parameter = pth,
          ParameterDisplayName = parameters[[pth]]$displayName,
          Sensitivity = sensitivityValue,
          SensitivityClass = getSensitivityClass(sensitivityValue)
        )


        if (runUncertaintyAnalysis) {
          uncertaintyVec <- uncertaintyAnalysisResults$value[uncertaintyAnalysisResults$parameterPath == pth & uncertaintyAnalysisResults$outputPath == outPth & uncertaintyAnalysisResults$pkParameter == pk]
          uncertaintyStats <- getUncertaintyStatistics(
            values = uncertaintyVec,
            minimumValidDraws = minimumValidDraws,
            quantiles = quantiles
          )
          if (!uncertaintyStats$defined) {
            undefinedUncertaintyResults[[length(undefinedUncertaintyResults) + 1]] <- paste0(
              pth, " / ", outPth, " / ", pk, ": ", uncertaintyStats$ValidDraws, "/", numberOfUncertaintyAnalysisSamples, " valid draws"
            )
          }

          df[["ValidDraws"]] <- uncertaintyStats$ValidDraws
          df[["Mean"]] <- uncertaintyStats$Mean
          df[["Percentile_2.5"]] <- uncertaintyStats$Percentile_2.5
          df[["Median"]] <- uncertaintyStats$Median
          df[["Percentile_97.5"]] <- uncertaintyStats$Percentile_97.5
          df[["UncertaintyRatio"]] <- uncertaintyStats$UncertaintyRatio
          df[["UncertaintyClass"]] <- getUncertaintyClass(uncertaintyStats$UncertaintyRatio)
          for (qv in quantiles) {
            df[[paste0("Percentile_", 100 * qv)]] <- uncertaintyStats$quantileValues[[as.character(qv)]]
          }
        }
        suResultsList[[outPth]][[pk]] <- rbind.data.frame(suResultsList[[outPth]][[pk]], df)
      }

      if (!is.null(suResultsList[[outPth]][[pk]])) {
        if (runUncertaintyAnalysis) {
          suResultsList[[outPth]][[pk]] <- suResultsList[[outPth]][[pk]][with(suResultsList[[outPth]][[pk]], order(-UncertaintyRatio, -Sensitivity)), ]
          exclusionRows <- (suResultsList[[outPth]][[pk]]$SensitivityClass %in% c(NA)) & (suResultsList[[outPth]][[pk]]$UncertaintyClass %in% NA)
        } else {
          suResultsList[[outPth]][[pk]] <- suResultsList[[outPth]][[pk]][with(suResultsList[[outPth]][[pk]], order(-Sensitivity)), ]
          exclusionRows <- (suResultsList[[outPth]][[pk]]$SensitivityClass %in% c(NA))
        }

        # Exclude rows that have negligible sensitivity or NA sensitivity or NA uncertainty
        suResultsList[[outPth]][[pk]] <- suResultsList[[outPth]][[pk]][!exclusionRows, ]

        allSUResults <- rbind.data.frame(allSUResults, suResultsList[[outPth]][[pk]])
      }
    }
  }

  if (length(undefinedUncertaintyResults) > 0) {
    warning(paste0(
      "Uncertainty analysis: uncertainty is undefined (reported as NA) for ", length(undefinedUncertaintyResults),
      " parameter / output / PK parameter combination(s) with fewer than ", minimumValidDraws,
      " valid draws:\n  ", paste0(unlist(undefinedUncertaintyResults), collapse = "\n  ")
    ))
  }

  summaryTable <- getSUSummaryDf(su = allSUResults)
  settings <- buildSettingsCMD(parameters = parameters, outputs = outputs)
  suResults <- list(Results = allSUResults, Settings = settings)

  suResults <- c(suResults, summaryTable)

  if (saveResults) {
    dateTime <- paste0(format(Sys.Date(), "%Y%m%d"), "_", format(Sys.time(), "%H%M%S"))

    if (is.null(saveFileName)) {
      saveFileName <- paste0("sensitivity-uncertainty-summary-", dateTime, ".xlsx")
    }

    if (is.null(saveFolder)) {
      saveFolder <- getwd()
    }

    writexl::write_xlsx(
      x = suResults,
      path = file.path(saveFolder, saveFileName)
    )
  }

  print(allSUResults)

  if (!is.null(originalParameterValues)) {
    ospsuite::setParameterValuesByPath(parameterPaths = names(originalParameterValues), values = unname(originalParameterValues), simulation = simulation)
  }

  return(suResults)
}

sensitivityLevels <- c("High", "Medium", "Low", "Negligible", NA)


#' @title cleanUpSUDf
#' @description Format a summary table of sensitivity and uncertainty analysis results.
#' @param df Un-formatted table of sensitivity and uncertainty analysis results.
#' @return Formatted table of sensitivity and uncertainty analysis results.
#' @keywords internal
cleanUpSUDf <- function(df) {
  newDf <- NULL
  for (sen in sensitivityLevels) {
    subdf <- df[df$Index %in% sen, ]
    for (unc in c("High", "Medium", "Low", "Undefined", "Parameter")) {
      if (!(unc %in% names(subdf))) {
        next
      }
      subdf[[unc]] <- sort(x = subdf[[unc]], decreasing = TRUE)
    }
    if (nrow(subdf) > 0) {
      subdf$Index <- rep("", nrow(subdf))
    }

    if ("Parameter" %in% names(subdf)) {
      subdf <- rbind(data.frame("Index" = sen, "Parameter" = ""), subdf)
    } else {
      headerRow <- data.frame("Index" = sen, "High" = "", "Medium" = "", "Low" = "")
      if ("Undefined" %in% names(subdf)) {
        headerRow$Undefined <- ""
      }
      subdf <- rbind(headerRow, subdf)
    }

    for (nr in rev(seq_len(nrow(subdf)))) {
      whichRowsEmpty <- sapply(names(subdf), function(x) {
        subdf[nr, x] == ""
      })
      whichRowsEmpty[is.na(whichRowsEmpty)] <- FALSE
      allEmpty <- all(whichRowsEmpty)
      if (allEmpty) {
        subdf <- subdf[-nr, ]
      }
    }
    newDf <- rbind.data.frame(newDf, subdf)
  }
  return(newDf)
}

#' @title renameSUDf
#' @description Rename headings in summary table of sensitivity and uncertainty analysis results.
#' @param df Un-formatted table of sensitivity and uncertainty analysis results.
#' @return Formatted table of sensitivity and uncertainty analysis results.
#' @keywords internal
renameSUDf <- function(df) {
  names(df)[names(df) == "High"] <- "High uncertainty"
  names(df)[names(df) == "Medium"] <- "Medium uncertainty"
  names(df)[names(df) == "Low"] <- "Low uncertainty"
  names(df)[names(df) == "Undefined"] <- "Undefined uncertainty"
  df$Index[df$Index == "High"] <- "High sensitivity"
  df$Index[df$Index == "Medium"] <- "Medium sensitivity"
  df$Index[df$Index == "Low"] <- "Low sensitivity"
  df$Index[df$Index == "Negligible"] <- "Negligible sensitivity"
  df$Index[df$Index %in% NA] <- "Sensitivity NA"
  return(df)
}

#' @title addLabelsSUDF
#' @description Rename headings in summary table of sensitivity and uncertainty analysis results.
#' @param df Table of sensitivity and uncertainty analysis results.
#' @param output String. Output name.
#' @param pkParameter String. PK parameter name.
#' @return Formatted table of sensitivity and uncertainty analysis results.
#' @keywords internal
addLabelsSUDF <- function(df, output, pkParameter) {
  nr <- nrow(df)
  outputCol <- rep("", nr)
  outputCol[1] <- output
  pkCol <- rep("", nr)
  pkCol[1] <- pkParameter
  df <- cbind(data.frame("Output" = outputCol, PKparameter = pkCol), df)
}

#' @title getSUSummaryDf
#' @description Function to summarize and categorize sensitivity and uncertainty analysis results in a table.
#' @param su Sensitivity and uncertainty analysis results
#' @return A list of data frames, one for each output/PK parameter combination, where each table categorizes the sensitivity and uncertainty of each parameter as high/medium/low.
#' @keywords internal
getSUSummaryDf <- function(su) {
  sumlist <- list()
  counter <- 0
  for (op in unique(su$Output)) {
    for (pk in unique(su$PK[su$Output == op])) {
      counter <- counter + 1
      subsu <- su[su$Output == op & su$PK == pk, ]
      df <- NULL
      for (sens in sensitivityLevels) {
        pars <- subsu$Parameter[subsu$SensitivityClass %in% sens]
        if (length(pars) > 0) {
          for (pp in pars) {
            ppDisplayName <- subsu$ParameterDisplayName[subsu$SensitivityClass %in% sens & subsu$Parameter == pp]
            if ("UncertaintyClass" %in% names(subsu)) {
              uncLevel <- subsu$UncertaintyClass[subsu$SensitivityClass %in% sens & subsu$Parameter == pp]
              newRow <- data.frame("Index" = sens, "High" = "", "Medium" = "", "Low" = "", "Undefined" = "")
              # NA uncertainty class: too few valid draws to assess uncertainty
              if (is.na(uncLevel)) {
                uncLevel <- "Undefined"
              }
              newRow[[uncLevel]] <- ppDisplayName
              df <- rbind(df, newRow)
            } else {
              newRow <- data.frame("Index" = sens, "Parameter" = "")
              newRow[["Parameter"]] <- ppDisplayName
              df <- rbind(df, newRow)
            }
          }
        }
      }
      # Only show the 'Undefined' uncertainty column when it is populated
      if ("Undefined" %in% names(df) && all(df$Undefined == "")) {
        df$Undefined <- NULL
      }
      df <- cleanUpSUDf(df)
      df <- renameSUDf(df)
      df <- addLabelsSUDF(df, op, pk)
      summaryName <- paste0(tail(ospsuite::toPathArray(op), 1), "_", pk)
      sumlist[[summaryName]] <- df
    }
  }
  return(sumlist)
}
