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
