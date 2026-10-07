# Hematocrit distribution from GitHub issue #20
hematocritMean <- 0.47
hematocritCV <- 0.0646

test_that("getLogNormalSampleVector returns finite, positive values at quantile boundaries 0 and 1", {
  samples <- getLogNormalSampleVector(
    c(0, 1),
    mean = hematocritMean,
    CV = hematocritCV
  )

  expect_true(all(is.finite(samples)))
  expect_true(all(samples > 0))
})

test_that("getLogNormalSampleVector returns the distribution median at quantile 0.5", {
  median <- hematocritMean / sqrt(hematocritCV^2 + 1)

  expect_equal(
    getLogNormalSampleVector(0.5, mean = hematocritMean, CV = hematocritCV),
    median
  )
})

test_that("getUniformSampleVector linearly interpolates between minimum and maximum", {
  expect_equal(
    getUniformSampleVector(c(0, 0.25, 0.5, 0.75, 1), minimum = 2, maximum = 6),
    c(2, 3, 4, 5, 6)
  )
})

test_that("getUniformSampleVector handles a negative range and a single quantile", {
  expect_equal(getUniformSampleVector(0.5, minimum = -4, maximum = 4), 0)
  expect_equal(getUniformSampleVector(c(0, 1), minimum = -10, maximum = -2), c(-10, -2))
})

test_that("getLogUniformSampleVector maps the endpoints to minimum and maximum", {
  samples <- getLogUniformSampleVector(c(0, 1), minimum = 1, maximum = 100)
  expect_equal(samples, c(1, 100))
})

test_that("getLogUniformSampleVector is geometric: the midpoint is the geometric mean", {
  expect_equal(
    getLogUniformSampleVector(0.5, minimum = 1, maximum = 100),
    sqrt(1 * 100)
  )
})

test_that("getLogUniformSampleVector returns zeros when both bounds are zero", {
  expect_equal(
    getLogUniformSampleVector(c(0, 0.5, 1), minimum = 0, maximum = 0),
    rep(0, 3)
  )
})

test_that("getLogUniformSampleVector errors on invalid bounds", {
  expect_error(
    getLogUniformSampleVector(0.5, minimum = 10, maximum = 1),
    "may not exceed"
  )
  expect_error(
    getLogUniformSampleVector(0.5, minimum = -1, maximum = 1),
    "must be of the same sign"
  )
})

test_that("getNormalSampleVector returns the mean at quantile 0.5 regardless of truncation", {
  expect_equal(getNormalSampleVector(0.5, mean = 3, stdv = 2, truncate = FALSE), 3)
  expect_equal(getNormalSampleVector(0.5, mean = 3, stdv = 2, truncate = TRUE), 3)
})

test_that("getNormalSampleVector truncation keeps the endpoints finite", {
  truncated <- getNormalSampleVector(c(0, 1), mean = 0, stdv = 1, truncate = TRUE)
  expect_true(all(is.finite(truncated)))

  untruncated <- getNormalSampleVector(c(0, 1), mean = 0, stdv = 1, truncate = FALSE)
  expect_equal(untruncated, c(-Inf, Inf))
})

test_that("getNormalSampleVector matches qnorm when truncation is disabled", {
  quantiles <- c(0.1, 0.4, 0.9)
  expect_equal(
    getNormalSampleVector(quantiles, mean = 1, stdv = 3, truncate = FALSE),
    qnorm(quantiles, mean = 1, sd = 3)
  )
})

test_that("verifyQuantileVec accepts values within [0, 1] and rejects values outside", {
  expect_null(verifyQuantileVec(c(0, 0.5, 1)))
  expect_error(verifyQuantileVec(c(0.5, 1.2)), "Quantile out of bounds")
  expect_error(verifyQuantileVec(c(-0.1, 0.5)), "Quantile out of bounds")
})

test_that("transformQuantiles dispatches to the distribution-specific transformation", {
  quantiles <- c(0, 0.5, 1)

  expect_equal(
    transformQuantiles(quantiles, "Uniform", list(minimum = 0, maximum = 10)),
    getUniformSampleVector(quantiles, 0, 10)
  )
  expect_equal(
    transformQuantiles(quantiles, "Normal", list(mean = 5, stdv = 1)),
    getNormalSampleVector(quantiles, 5, 1)
  )
})

test_that("getEvaluationMatrixStructure creates one empty list entry per output", {
  structure <- getEvaluationMatrixStructure(list(out1 = NA, out2 = NA))
  expect_named(structure, c("out1", "out2"))
  expect_equal(structure$out1, list())
  expect_equal(structure$out2, list())
})

test_that("distribution name helpers build path-specific identifiers", {
  expect_equal(distSelection("Org|Par"), "distributionSelectionOrg|Par")
  expect_equal(pkParameterSelection("Org|Par"), "pkParameterSelectionOrg|Par")
  expect_equal(distValUniformMinimum("p"), "uniformMin_p")
  expect_equal(distValNormalStdv("p"), "normalStdv_p")
  expect_equal(distValLogNormalStdv("p"), "logNormalCV_p")
})

test_that("getDistValsList returns the field-to-input mapping for each distribution", {
  distValKey <- getDistValsList("p")

  expect_named(distValKey, c("Uniform", "LogUniform", "Normal", "LogNormal", "dimension"))
  expect_named(distValKey$Uniform, c("minimum", "maximum", "unit"))
  expect_named(distValKey$LogNormal, c("mean", "CV", "unit"))
  expect_equal(distValKey$Normal$mean, "normalMean_p")
  expect_equal(distValKey$dimension, "dimension_p")
})

test_that("the *Inputs helpers package the distribution parameters into named lists", {
  expect_equal(uniformInputs(minimum = 0, maximum = 1), list(minimum = 0, maximum = 1))
  expect_equal(logUniformInputs(minimum = 2, maximum = 8), list(minimum = 2, maximum = 8))
  expect_equal(normalInputs(mean = 5, stdv = 2), list(mean = 5, stdv = 2))
  expect_equal(logNormalInputs(mean = 10, CV = 0.5), list(mean = 10, CV = 0.5))
})

test_that("the *QuantilesToSamples wrappers read their inputs from the parameter list", {
  quantiles <- c(0, 0.5, 1)

  expect_equal(
    uniformQuantilesToSamples(quantiles, list(minimum = 0, maximum = 10)),
    getUniformSampleVector(quantiles, 0, 10)
  )
  expect_equal(
    logUniformQuantilesToSamples(quantiles, list(minimum = 1, maximum = 100)),
    getLogUniformSampleVector(quantiles, 1, 100)
  )
  expect_equal(
    normalQuantilesToSamples(quantiles, list(mean = 5, stdv = 1)),
    getNormalSampleVector(quantiles, 5, 1)
  )
  expect_equal(
    logNormalQuantilesToSamples(quantiles, list(mean = 10, CV = 0.5)),
    getLogNormalSampleVector(quantiles, 10, 0.5)
  )
})

test_that("quantileTransformationFunctions exposes one transformation per distribution type", {
  expect_named(quantileTransformationFunctions, c("Uniform", "LogUniform", "Normal", "LogNormal"))

  quantiles <- c(0.1, 0.5, 0.9)
  expect_equal(
    quantileTransformationFunctions$LogUniform(quantiles, list(minimum = 2, maximum = 8)),
    getLogUniformSampleVector(quantiles, 2, 8)
  )
})
