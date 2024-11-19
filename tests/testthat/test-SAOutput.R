simPath <- system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
simulation <- loadSimulation(simPath)
outputPath <- "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)"
parameterPath <- c("Aciclovir|Lipophilicity")

test_that("SAOutput object can be created with default settings", {
  expect_no_error(
    SAOutputTest <- SAOutput$new(
      simulation = simulation,
      path = outputPath
    )
  )
  expect_s3_class(SAOutputTest, "R6")
  expect_equal(SAOutputTest$path, outputPath)
  expect_equal(SAOutputTest$displayName, outputPath)
  expect_null(SAOutputTest$pkParameterList)
})
