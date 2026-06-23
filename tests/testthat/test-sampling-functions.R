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
