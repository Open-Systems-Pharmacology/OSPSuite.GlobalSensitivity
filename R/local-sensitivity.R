#' @title getLocalSensitivity
#' @description Function to run local sensitivity analysis
#' @param simulation PKML simulation object.
#' @param DDIsimulation DDI PKML simulation object.
#' @param parameterPaths Paths of PKML simulation parameters to be analyzed\.
#' @param outputs List of `SAOutput` objects.
#' @param variationRange Variation range for sensitivity analysis.
#' @param numberOfSteps Number of points to evaluate model within the `variationRange`.
#' @param runParallel  Logical value.  Sensitivity analysis computation is run in parallel when `TRUE`.
#' @param updateProgress Logical value.  Updates shiny app GUI with sensitivity analysis progress when `TRUE`.
#' @return description
getLocalSensitivity <- function(simulation,
                                DDIsimulation = NULL,
                                parameterPaths,
                                outputs,
                                variationRange = 0.1,
                                numberOfSteps = 2,
                                runParallel = TRUE,
                                updateProgress = NULL) {
  print("Running sensitivity analysis")

  if (is.null(parameterPaths)) {
    stop("No model parameters have been selected for local sensitivity analysis.")
  }

  if (is.null(outputs)) {
    stop("No model outputs have been selected for local sensitivity analysis.")
  }

  sa <- ospsuite::SensitivityAnalysis$new(
    simulation = simulation,
    parameterPaths = parameterPaths,
    numberOfSteps = numberOfSteps,
    variationRange = variationRange
  )

  sares <- ospsuite::runSensitivityAnalysis(sensitivityAnalysis = sa)

  if (!is.null(DDIsimulation)) {
    DDIsa <- ospsuite::SensitivityAnalysis$new(
      simulation = DDIsimulation,
      parameterPaths = parameterPaths,
      numberOfSteps = numberOfSteps,
      variationRange = variationRange
    )

    DDIsares <- ospsuite::runSensitivityAnalysis(sensitivityAnalysis = DDIsa)
  }


  if (!is.character(names(outputs))) {
    names(outputs) <- sapply(outputs, function(op) {
      op$path
    })
  }

  sensitivityDataframe <- NULL
  for (parameterPath in parameterPaths) {
    for (outPth in names(outputs)) {
      for (pk in outputs[[outPth]]$pkParameterList) {
        sensitivityValue <- sares$pkParameterSensitivityValueFor(
          pkParameterName = pk,
          parameterPath = parameterPath,
          outputPath = outPth
        )
        if (!is.null(DDIsimulation)) {
          DDIsensitivityValue <- DDIsares$pkParameterSensitivityValueFor(
            pkParameterName = pk,
            parameterPath = parameterPath,
            outputPath = outPth
          )
        }



        sensitivityDataframe <- rbind.data.frame(
          sensitivityDataframe,
          data.frame(
            parameterPath = parameterPath,
            outputPath = outPth,
            pkParameter = pk,
            sensitivity = sensitivityValue,
            SensitivityClass = getSensitivityClass(sensitivityValue)
          )
        )

        if (!is.null(DDIsimulation)) {
          sensitivityDataframe <- rbind.data.frame(
            sensitivityDataframe,
            data.frame(
              parameterPath = parameterPath,
              outputPath = outPth,
              pkParameter = paste0(pk, "-DDI-ratio"),
              sensitivity = DDIsensitivityValue - sensitivityValue,
              SensitivityClass = getSensitivityClass(DDIsensitivityValue - sensitivityValue)
            )
          )
        }
      }
    }
  }

  if (is.function(updateProgress)) {
    updateProgress(value = 1)
  }

  return(sensitivityDataframe)
}
