test_that("%||% returns the left-hand side when it is not NULL", {
  expect_equal("value" %||% "default", "value")
  expect_equal(0 %||% 99, 0)
  expect_equal(FALSE %||% TRUE, FALSE)
})

test_that("%||% falls back to the right-hand side when the left-hand side is NULL", {
  expect_equal(NULL %||% "default", "default")
})

test_that("error stops with the supplied message when the condition is TRUE", {
  expect_error(error(TRUE, "boom"), "boom")
})

test_that("error returns NULL invisibly when the condition is FALSE", {
  expect_null(error(FALSE, "should not stop"))
})

test_that("squareTheCircle converts round parentheses to square brackets", {
  expect_equal(squareTheCircle("f(x)"), "f[x]")
  expect_equal(squareTheCircle("Plasma (Peripheral)"), "Plasma [Peripheral]")
})

test_that("squareTheCircle leaves strings without parentheses unchanged", {
  expect_equal(squareTheCircle("no brackets here"), "no brackets here")
})

test_that("buildSettingsCMD records the distribution, its parameters and units for each path", {
  # Mock SAParameter / SAOutput objects exposing only the fields used by
  # buildSettingsCMD. The distributions mirror those used in the case-study
  # vignettes (e.g. LogNormal mean = 10, CV = 0.5; Uniform [0, 1]).
  parameters <- list(
    list(path = "P1", unit = "", distribution = UniformDistribution$new(minimum = 0, maximum = 1)),
    list(path = "P2", unit = "\u00b5m", distribution = LogNormalDistribution$new(mean = 10, CV = 0.5))
  )
  outputs <- list(list(path = "Out1", pkParameterList = c("C_max", "AUC")))

  settings <- suppressWarnings(buildSettingsCMD(parameters, outputs))
  text <- settings$Settings

  expect_s3_class(settings, "data.frame")
  expect_true("Distribution for P1 is Uniform" %in% text)
  expect_true("Value of minimum of the Uniform distribution for path P1 is 0" %in% text)
  expect_true("Value of maximum of the Uniform distribution for path P1 is 1" %in% text)
  # A dimensionless parameter is reported as such ...
  expect_true("The random variable of the Uniform distribution for path P1 is dimensionless" %in% text)
  # ... while a parameter with a unit reports that unit
  expect_true("Distribution for P2 is LogNormal" %in% text)
  expect_true("The unit of the random variable of the LogNormal distribution for path P2 is \u00b5m" %in% text)
  # PK parameter selections are listed for each output
  expect_true("PK selections for Out1 are C_max, AUC" %in% text)
})
