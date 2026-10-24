test_that("getUncertaintyAnalysisResults returns one value per draw per parameter on a real simulation", {
  skip_if_not_installed("ospsuite")
  skip_on_ci()

  simPath <- system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
  simulation <- ospsuite::loadSimulation(simPath)

  parameterPaths <- c("Aciclovir|Lipophilicity", "Aciclovir|Permeability")
  parameters <- lapply(parameterPaths, function(path) SAParameter$new(simulation = simulation, path = path))
  names(parameters) <- parameterPaths
  outputPath <- "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)"
  output <- SAOutput$new(simulation = simulation, path = outputPath)
  output$addPKParameter(standardPKParameter = "C_max")
  outputs <- list(output)
  names(outputs) <- outputPath
  ospsuite::addOutputs(quantitiesOrPaths = outputPath, simulation = simulation)

  numberOfSamples <- 3
  results <- getUncertaintyAnalysisResults(
    simulation = simulation,
    parameters = parameters,
    outputs = outputs,
    numberOfUncertaintyAnalysisSamples = numberOfSamples,
    sensitiveParameterPaths = parameterPaths,
    runParallel = FALSE
  )

  expect_equal(nrow(results), length(parameterPaths) * numberOfSamples)
  expect_true(all(is.finite(results$value)))
  # Each draw's PK value must come from its own sampled parameter value, so
  # different draws of a sensitive parameter give different C_max values.
  expect_equal(anyDuplicated(results$value[results$parameterPath == parameterPaths[1]]), 0)
})
