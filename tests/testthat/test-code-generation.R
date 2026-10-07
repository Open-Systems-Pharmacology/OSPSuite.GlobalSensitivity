test_that("generateParameterCode embeds the path, names, unit and distribution", {
  code <- generateParameterCode(
    path = "Aciclovir|Lipophilicity",
    displayName = "Lipo",
    unit = "Log Units",
    parameterDistribution = "distribution$Uniform(minimum = 1, maximum = 2)"
  )

  expect_match(code, 'parametersList\\[\\["Aciclovir\\|Lipophilicity"\\]\\] <- SAParameter\\$new', perl = TRUE)
  expect_match(code, 'path = "Aciclovir\\|Lipophilicity"', perl = TRUE)
  expect_match(code, 'displayName = "Lipo"', perl = TRUE)
  expect_match(code, 'unit = "Log Units"', perl = TRUE)
  expect_match(code, "distribution\\$Uniform\\(minimum = 1, maximum = 2\\)", perl = TRUE)
})

test_that("generateOutputCode creates the output and one line per PK parameter", {
  code <- generateOutputCode(
    path = "Organism|Plasma",
    displayName = "Plasma",
    pkParameterList = c("C_max", "AUC")
  )

  expect_match(code, 'outputsList\\[\\["Organism\\|Plasma"\\]\\] <- SAOutput\\$new', perl = TRUE)
  expect_match(code, 'standardPKParameter = "C_max"')
  expect_match(code, 'standardPKParameter = "AUC"')
  # one creation line plus one addPKParameter line per PK parameter
  expect_equal(length(gregexpr("addPKParameter", code)[[1]]), 2L)
})

test_that("generateOutputCode emits no addPKParameter calls for an empty PK list", {
  code <- generateOutputCode(
    path = "Organism|Plasma",
    displayName = "Plasma",
    pkParameterList = character(0)
  )
  expect_false(grepl("addPKParameter", code))
})

test_that("distributionStringsFnsList renders each distribution as constructor code", {
  uniformParameter <- list(distribution = list(type = "Uniform", minimum = 1, maximum = 5))
  expect_equal(
    distributionStringsFnsList$Uniform(uniformParameter),
    "distribution$Uniform(minimum = 1, maximum = 5)"
  )

  normalParameter <- list(distribution = list(type = "Normal", mean = 2, stdv = 0.5))
  expect_equal(
    distributionStringsFnsList$Normal(normalParameter),
    "distribution$Normal(mean = 2, stdv = 0.5)"
  )

  logNormalParameter <- list(distribution = list(type = "LogNormal", mean = 2, CV = 0.3))
  expect_equal(
    distributionStringsFnsList$LogNormal(logNormalParameter),
    "distribution$LogNormal(mean = 2, CV = 0.3)"
  )
})

test_that("writeSobolSensitivityFunctionToFile injects the requested sample count", {
  code <- writeSobolSensitivityFunctionToFile(numberOfSamplesGSA = 32)
  expect_true(any(grepl("runSobol", code)))
  expect_true(any(grepl("numberOfSamples = 32", code)))
})

test_that("writeEFASTSensitivityFunctionToFile injects the requested resample count", {
  code <- writeEFASTSensitivityFunctionToFile(numberOfReSamplesEFAST = 3)
  expect_true(any(grepl("runEFAST", code)))
  expect_true(any(grepl("numberOfResamples = 3", code)))
})

test_that("writeMorrisSensitivityFunctionToFile injects the requested sample count", {
  code <- writeMorrisSensitivityFunctionToFile(numberOfSamples = 10)
  expect_true(any(grepl("runMorris", code)))
  expect_true(any(grepl("numberOfSamples = 10", code)))
})
