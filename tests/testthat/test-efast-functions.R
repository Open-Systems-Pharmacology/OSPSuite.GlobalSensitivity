makeParameter <- function(path, displayName = path) {
  list(path = path, displayName = displayName)
}

test_that("generateParameterFrequenciesTotal assigns the high complementary frequency to the target parameter", {
  parameters <- list(makeParameter("p1"), makeParameter("p2"), makeParameter("p3"))

  frequencies <- generateParameterFrequenciesTotal(parameters, parameterNumber = 1)

  # M = 4, complementary frequencies are 1 and 3, target = 2 * M * max(others) + 1
  expect_equal(unname(frequencies), c(2 * 4 * 3 + 1, 1, 3))
  expect_named(frequencies, c("p1", "p2", "p3"))
})

test_that("getSampleRate satisfies the Nyquist-style oversampling formula", {
  # M = 4, rate = 2 * (wMax * M) + 1
  expect_equal(getSampleRate(c(p1 = 25, p2 = 1, p3 = 3)), 2 * (25 * 4) + 1)
})

test_that("getSamplingHypercube has one column per parameter and one row per sampling point", {
  frequencies <- c(p1 = 25, p2 = 1, p3 = 3)
  samplingRate <- 50
  hypercube <- getSamplingHypercube(frequencies, samplingRate)

  expect_equal(ncol(hypercube), length(frequencies))
  expect_equal(nrow(hypercube), samplingRate)
  # the first sampling point is 0, so the first row is all zeros
  expect_true(all(hypercube[1, ] == 0))
})

test_that("perturbHypercube keeps all sampled values inside the unit interval", {
  set.seed(99)
  hypercube <- getSamplingHypercube(c(p1 = 2, p2 = 1), samplingRate = 20)
  perturbed <- perturbHypercube(hypercube)

  expect_equal(dim(perturbed), dim(hypercube))
  expect_true(all(perturbed >= 0 & perturbed <= 1))
})

test_that("getEFASTResultsDf flattens nested indices into FirstOrder and Total rows", {
  fftEvaluationsList <- list(
    out1 = list(
      AUC = list(p1 = list(S1 = 0.4, St = 0.6))
    )
  )
  outputs <- list(out1 = NA)

  resultsDf <- getEFASTResultsDf(fftEvaluationsList, outputs)

  expect_equal(nrow(resultsDf), 2L)
  expect_setequal(resultsDf$Measure, c("FirstOrder", "Total"))
  expect_equal(resultsDf$Value[resultsDf$Measure == "FirstOrder"], 0.4)
  expect_equal(resultsDf$Value[resultsDf$Measure == "Total"], 0.6)
  expect_true(all(resultsDf$Parameter == "p1"))
})

test_that("summarizeParameterDisplayNames returns a path/displayName data frame", {
  parameters <- list(makeParameter("p1", "Lipo"), makeParameter("p2", "Perm"))
  summaryDf <- summarizeParameterDisplayNames(parameters)

  expect_equal(summaryDf$path, c("p1", "p2"))
  expect_equal(summaryDf$displayName, c("Lipo", "Perm"))
})

test_that("summarizeParameterDisplayNames returns an empty frame for an empty list", {
  summaryDf <- summarizeParameterDisplayNames(list())
  expect_equal(nrow(summaryDf), 0L)
  expect_named(summaryDf, c("path", "displayName"))
})

test_that("summarizeOutputDisplayNames returns a path/displayName data frame", {
  outputs <- list(makeParameter("o1", "Plasma"))
  summaryDf <- summarizeOutputDisplayNames(outputs)

  expect_equal(summaryDf$path, "o1")
  expect_equal(summaryDf$displayName, "Plasma")
})

# Mock SAParameter-like object holding only the fields used by the EFAST
# mapping: a path, a (dimensionless) dimension and an SADistribution.
makeDistParameter <- function(path, distribution) {
  list(path = path, dimension = "Dimensionless", unit = "", distribution = distribution)
}

test_that("mapHypercubeToParameterSpace maps quantiles via each parameter's inverse CDF", {
  # As described in the variance-based-methods vignette (Figure 1), points in
  # the unit hypercube of quantiles are mapped onto parameter space using the
  # inverse cumulative distribution of each parameter.
  parameters <- list(
    makeDistParameter("a", UniformDistribution$new(minimum = 0, maximum = 10)),
    makeDistParameter("b", LogUniformDistribution$new(minimum = 1, maximum = 100))
  )
  names(parameters) <- c("a", "b")

  hypercube <- data.frame(a = c(0, 0.5, 1), b = c(0, 0.5, 1))

  mapped <- mapHypercubeToParameterSpace(parameters, hypercube)

  expect_equal(mapped$a, c(0, 5, 10))
  expect_equal(mapped$b, c(1, 10, 100))
})

test_that("perturbHypercube implements the documented arcsin(sin()) EFAST curve", {
  # Saltelli, Tarantola & Chan (1999): the search curve is
  # 1/2 + (1/pi) * arcsin(sin(omega * theta + phi)) with a random phase phi.
  quantiles <- c(0, 0.25, 0.5, 0.75)

  set.seed(2024)
  perturbed <- perturbHypercube(data.frame(p1 = quantiles))

  set.seed(2024)
  phase <- runif(1, 0, 2 * pi)
  expected <- 0.5 + asin(sin(quantiles + phase)) / pi

  expect_equal(perturbed$p1, expected)
})

test_that("getEFASTResultsDf produces FirstOrder and Total rows for every output and PK parameter", {
  # getEFASTResultsDf is invoked once per parameter inside runEFAST, so each
  # output/PK entry holds a single parameter; multiple outputs and PK parameters
  # are exercised here.
  fftEvaluationsList <- list(
    out1 = list(
      AUC = list(p1 = list(S1 = 0.4, St = 0.6)),
      C_max = list(p1 = list(S1 = 0.2, St = 0.5))
    ),
    out2 = list(
      C_max = list(p1 = list(S1 = 0.7, St = 0.8))
    )
  )
  outputs <- list(out1 = NA, out2 = NA)

  resultsDf <- getEFASTResultsDf(fftEvaluationsList, outputs)

  expect_setequal(resultsDf$Measure, c("FirstOrder", "Total"))
  expect_equal(nrow(resultsDf), 6L)
  expect_setequal(unique(resultsDf$Output), c("out1", "out2"))
  expect_setequal(unique(resultsDf$PK[resultsDf$Output == "out1"]), c("AUC", "C_max"))
  expect_equal(
    resultsDf$Value[resultsDf$Output == "out1" & resultsDf$PK == "C_max" & resultsDf$Measure == "Total"],
    0.5
  )
})
