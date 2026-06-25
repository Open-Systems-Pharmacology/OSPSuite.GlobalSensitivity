#' @title `%||%`
#' @description Function to select RHS argument in case LHS is NULL.  See purrr::%||%
#' @param lhs argument
#' @param rhs argument
#' @return `lhs` if not null, otherwise `rhs`
`%||%` <- function(lhs, rhs) {
  if (is.null(lhs)) {
    return(rhs)
  }
  return(lhs)
}

#' @title error
#' @description Stops if `condition` is `TRUE`
#' @param condition A logical condition
#' @param errorMessage Message to be printed before stopping in case `condition` is `TRUE`
error <- function(condition, errorMessage = NULL) {
  if (condition) {
    stop(errorMessage)
  }
  return(NULL)
}



#' @title buildSettingsCMD
#' @description Build a summary of all parameter and outputs selected for a sensitivity analysis.
#' @param parameters A list of `SAParameter` objects.
#' @param outputs A list of `SAOutput` objects.
#' @return A summary of all parameter and outputs selected for a sensitivity analysis
buildSettingsCMD <- function(parameters, outputs) {
  resultsSettings <- NULL

  parameterPaths <- sapply(parameters, function(par) {
    par$path
  })
  names(parameters) <- parameterPaths
  outputPaths <- sapply(outputs, function(op) {
    op$path
  })
  names(outputs) <- outputPaths

  for (pth in parameterPaths) {
    pathSetting <- paste("Distribution for", pth, "is", parameters[[pth]]$distribution$type)
    resultsSettings <- c(resultsSettings, pathSetting)

    dist <- parameters[[pth]]$distribution$type
    distValKey <- getDistValsList(pth)
    distParameterNames <- names(distValKey[[dist]])

    for (distParameterName in distParameterNames) {
      if (distParameterName == "unit") {
        unit <- parameters[[pth]]$unit
        valueSetting <- paste("The unit of the random variable of the", dist, "distribution for path", pth, "is", parameters[[pth]]$unit)
        if (unit == "") {
          valueSetting <- paste("The random variable of the", dist, "distribution for path", pth, "is dimensionless")
        }
      } else {
        valueSetting <- paste("Value of", distParameterName, "of the", dist, "distribution for path", pth, "is", parameters[[pth]]$distribution[[distParameterName]])
      }

      resultsSettings <- c(resultsSettings, valueSetting)
    }
  }

  for (pth in names(outputs)) {
    input_string <- pkParameterSelection(pth)
    outputSetting <- paste("PK selections for", pth, "are", paste0(outputs[[pth]]$pkParameterList, collapse = ", "))
    resultsSettings <- c(resultsSettings, outputSetting)
  }

  resultsSettings <- data.frame(Settings = resultsSettings)
  print(resultsSettings)
  return(resultsSettings)
}

#' @title checkParametersExistInSimulation
#' @description Function to verify that a list of paramter paths exists in a simulation.
#' @param simulation A PKML simulation object.
#' @param parameterPaths A vector of strings of parameter paths.
#' @param simulationName A descriptive name for the `simulation`.
#' @param stopIfNotFound Logical value. When `TRUE`, results in an error when a parameter in `parameterPaths` is not found in `simulation`.
#' @return A list of logical values indicating whether each outut path in `parameterPaths` is found in `simulation`.
checkParametersExistInSimulation <- function(simulation, parameterPaths, simulationName = NULL, stopIfNotFound = FALSE) {
  responseFn <- warning
  if (stopIfNotFound) {
    responseFn <- stop
  }

  if (is.null(simulationName)) {
    simulationName <- simulation$sourceFile
  }
  parameterPresent <- NULL
  for (pth in parameterPaths) {
    parameterPresent[[pth]] <- TRUE
    if (is.null(ospsuite::getAllParametersMatching(paths = pth, simulation))) {
      responseFn(paste0("Parameter with path ", pth, " not found in ", simulationName, "."))
      parameterPresent[[pth]] <- FALSE
    }
  }
  return(parameterPresent)
}

#' @title checkOutputsExistInSimulation
#' @description Function to verify that a list of output paths exists in a simulation.
#' @param simulation A PKML simulation object.
#' @param outputPaths A victor of strings of output paths.
#' @param simulationName A descriptive name for the `simulation`.
#' @param stopIfNotFound Logical value. When `TRUE`, results in an error when an output in `outputPaths` is not found in `simulation`.
#' @return A list of logical values indicating whether each outut path in `outputPaths` is found in `simulation`.
checkOutputsExistInSimulation <- function(simulation, outputPaths, simulationName, stopIfNotFound = FALSE) {
  responseFn <- warning
  if (stopIfNotFound) {
    responseFn <- stop
  }

  if (is.null(simulationName)) {
    simulationName <- simulation$sourceFile
  }
  outputPresent <- NULL
  for (pth in outputPaths) {
    outputPresent[[pth]] <- TRUE
    if (is.null(ospsuite::getAllQuantitiesMatching(paths = pth, simulation))) {
      responseFn(paste0("Output with path ", pth, " not found in ", simulationName, "."))
      outputPresent[[pth]] <- FALSE
    }
  }
  return(outputPresent)
}

squareTheCircle <- function(x) {
  x <- sub(pattern = "[)]", x = x, replacement = "]")
  x <- sub(pattern = "[(]", x = x, replacement = "[")
  return(x)
}
