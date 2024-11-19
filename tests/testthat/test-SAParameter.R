simPath <- system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
simulation <- loadSimulation(simPath)
parameterPath <- c("Aciclovir|Lipophilicity")

test_that("SAParameter object can be created with default settings", {
  expect_no_error(
    SAParameterTest <- SAParameter$new(
      simulation = simulation,
      path = parameterPath
    )
  )
  expect_s3_class(SAParameterTest, "R6")
  expect_equal(SAParameterTest$path, parameterPath)
  expect_equal(SAParameterTest$displayName, parameterPath)
  expect_equal(SAParameterTest$unit, "Log Units")
})
