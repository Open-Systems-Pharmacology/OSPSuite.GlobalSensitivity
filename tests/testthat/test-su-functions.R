test_that("getUncertaintyClass categorizes by absolute magnitude", {
  expect_equal(getUncertaintyClass(2), "High")
  expect_equal(getUncertaintyClass(-3), "High")
  expect_equal(getUncertaintyClass(0.3), "Medium")
  expect_equal(getUncertaintyClass(0.5), "Medium")
  expect_equal(getUncertaintyClass(0.1), "Low")
  expect_equal(getUncertaintyClass(0), "Low")
})

test_that("getUncertaintyClass maps NA and NaN to NA", {
  expect_true(is.na(getUncertaintyClass(NA)))
  expect_true(is.na(getUncertaintyClass(NaN)))
})

test_that("getSensitivityClass categorizes by absolute magnitude", {
  expect_equal(getSensitivityClass(0.5), "High")
  expect_equal(getSensitivityClass(-0.9), "High")
  expect_equal(getSensitivityClass(0.2), "Medium")
  expect_equal(getSensitivityClass(0.1), "Low")
  expect_equal(getSensitivityClass(0.05), "Negligible")
})

test_that("getSensitivityClass maps NA and NaN to NA", {
  expect_true(is.na(getSensitivityClass(NA)))
  expect_true(is.na(getSensitivityClass(NaN)))
})

test_that("renameSUDf relabels uncertainty columns and sensitivity index values", {
  df <- data.frame(
    Index = c("High", "Medium", "Low", "Negligible", NA),
    High = "",
    Medium = "",
    Low = "",
    stringsAsFactors = FALSE
  )

  renamed <- renameSUDf(df)

  expect_true(all(c("High uncertainty", "Medium uncertainty", "Low uncertainty") %in% names(renamed)))
  expect_equal(
    renamed$Index,
    c("High sensitivity", "Medium sensitivity", "Low sensitivity", "Negligible sensitivity", "Sensitivity NA")
  )
})

test_that("addLabelsSUDF prepends Output/PKparameter columns labelled only on the first row", {
  df <- data.frame(Index = c("a", "b", "c"), stringsAsFactors = FALSE)

  labelled <- addLabelsSUDF(df, output = "Plasma", pkParameter = "C_max")

  expect_equal(names(labelled)[1:2], c("Output", "PKparameter"))
  expect_equal(labelled$Output, c("Plasma", "", ""))
  expect_equal(labelled$PKparameter, c("C_max", "", ""))
})

test_that("cleanUpSUDf groups parameters under their sensitivity-level header rows", {
  # 'Parameter' variant (sensitivity-only summary, no uncertainty analysis)
  df <- data.frame(
    Index = c("High", "High", "Medium", "Negligible"),
    Parameter = c("a", "b", "c", "d"),
    stringsAsFactors = FALSE
  )

  cleaned <- cleanUpSUDf(df)

  expect_named(cleaned, c("Index", "Parameter"))
  # Every supplied parameter is retained somewhere in the summary
  expect_setequal(intersect(cleaned$Parameter, c("a", "b", "c", "d")), c("a", "b", "c", "d"))
  # Header rows are inserted for the sensitivity levels
  expect_true(any(cleaned$Index == "High"))
  expect_true(any(cleaned$Index == "Medium"))
  expect_true(any(cleaned$Index == "Negligible"))
})

test_that("cleanUpSUDf places parameters into the matching uncertainty column", {
  # Cross-tabulated variant: sensitivity level as rows, uncertainty as columns
  df <- data.frame(
    Index = c("High", "Medium"),
    High = c("a", ""),
    Medium = c("", "b"),
    Low = c("", ""),
    stringsAsFactors = FALSE
  )

  cleaned <- cleanUpSUDf(df)

  expect_named(cleaned, c("Index", "High", "Medium", "Low"))
  expect_true("a" %in% cleaned$High)
  expect_true("b" %in% cleaned$Medium)
})

test_that("getUncertaintyStatistics summarizes valid draws, ignoring failed (NA) draws", {
  values <- c(1:9, NA)
  st <- getUncertaintyStatistics(values, minimumValidDraws = 9, quantiles = c(0.05, 0.5))

  expect_true(st$defined)
  expect_equal(st$ValidDraws, 9)
  expect_equal(st$Mean, 5)
  expect_equal(st$Median, 5)
  expect_equal(st$Percentile_2.5, unname(quantile(1:9, 0.025)))
  expect_equal(st$Percentile_97.5, unname(quantile(1:9, 0.975)))
  expect_equal(st$UncertaintyRatio, (st$Percentile_97.5 - st$Percentile_2.5) / 5)
  expect_equal(st$quantileValues[["0.05"]], unname(quantile(1:9, 0.05)))
})

test_that("getUncertaintyStatistics reports total failure as undefined, not zero uncertainty", {
  st <- getUncertaintyStatistics(rep(NA_real_, 100), minimumValidDraws = 90, quantiles = 0.5)

  expect_false(st$defined)
  expect_equal(st$ValidDraws, 0)
  expect_true(is.na(st$UncertaintyRatio))
  expect_true(is.na(st$Median))
  expect_true(is.na(st$quantileValues[["0.5"]]))
  expect_true(is.na(getUncertaintyClass(st$UncertaintyRatio)))
})

test_that("getUncertaintyStatistics reports near-total failure as undefined", {
  values <- c(1, 2, 3, rep(NA_real_, 97))
  st <- getUncertaintyStatistics(values, minimumValidDraws = 90)

  expect_false(st$defined)
  expect_equal(st$ValidDraws, 3)
  expect_true(is.na(st$UncertaintyRatio))
  expect_true(is.na(st$Mean))
})

test_that("getUncertaintyStatistics treats non-finite values as failed draws", {
  st <- getUncertaintyStatistics(c(1, 2, Inf, NaN), minimumValidDraws = 3)
  expect_false(st$defined)
  expect_equal(st$ValidDraws, 2)
})

test_that("getUncertaintyStatistics gives zero uncertainty only for zero spread among enough valid draws", {
  expect_equal(getUncertaintyStatistics(rep(0, 10), minimumValidDraws = 9)$UncertaintyRatio, 0)
  expect_equal(getUncertaintyStatistics(rep(3, 10), minimumValidDraws = 9)$UncertaintyRatio, 0)
  # A single surviving draw has zero spread but is not evidence of zero uncertainty
  expect_true(is.na(getUncertaintyStatistics(c(3, NA), minimumValidDraws = 1)$UncertaintyRatio))
})

test_that("getSUSummaryDf places parameters with undefined uncertainty in an 'Undefined uncertainty' column", {
  su <- data.frame(
    Output = "Organism|Plasma",
    PK = "C_max",
    Parameter = c("p1", "p2"),
    ParameterDisplayName = c("P1", "P2"),
    SensitivityClass = c("High", "High"),
    UncertaintyClass = c("Low", NA),
    stringsAsFactors = FALSE
  )

  summary <- getSUSummaryDf(su)[[1]]

  expect_true("Undefined uncertainty" %in% names(summary))
  expect_true("P2" %in% summary[["Undefined uncertainty"]])
  expect_true("P1" %in% summary[["Low uncertainty"]])
})

test_that("getSUSummaryDf omits the 'Undefined uncertainty' column when every uncertainty is defined", {
  su <- data.frame(
    Output = "Organism|Plasma",
    PK = "C_max",
    Parameter = c("p1", "p2"),
    ParameterDisplayName = c("P1", "P2"),
    SensitivityClass = c("High", "Medium"),
    UncertaintyClass = c("Low", "High"),
    stringsAsFactors = FALSE
  )

  summary <- getSUSummaryDf(su)[[1]]

  expect_false("Undefined uncertainty" %in% names(summary))
  expect_true("P1" %in% summary[["Low uncertainty"]])
  expect_true("P2" %in% summary[["High uncertainty"]])
})
