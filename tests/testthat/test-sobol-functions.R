test_that("getSobolSampleMatrices returns A and B with one column per parameter and one row per sample", {
  numberOfParameters <- 3
  numberOfSamples <- 8
  matrices <- getSobolSampleMatrices(numberOfParameters, numberOfSamples)

  expect_equal(dim(matrices$A), c(numberOfSamples, numberOfParameters))
  expect_equal(dim(matrices$B), c(numberOfSamples, numberOfParameters))
})

test_that("getSobolSampleMatrices keeps n x k shape when numberOfSamples is 1", {
  matrices <- getSobolSampleMatrices(numberOfParameters = 3, numberOfSamples = 1)
  expect_equal(dim(matrices$A), c(1L, 3L))
  expect_equal(dim(matrices$B), c(1L, 3L))
})

test_that("getSobolSampleMatrices produces low-discrepancy points strictly inside the unit interval", {
  matrices <- getSobolSampleMatrices(
    numberOfParameters = 2,
    numberOfSamples = 16
  )
  allValues <- c(matrices$A, matrices$B)
  expect_false(any(allValues <= 0 | allValues >= 1))
})

test_that("getSobolSampleMatrices is deterministic across calls", {
  first <- getSobolSampleMatrices(numberOfParameters = 2, numberOfSamples = 16)
  second <- getSobolSampleMatrices(numberOfParameters = 2, numberOfSamples = 16)
  expect_equal(first, second)
})

test_that("getSobolSampleMatrices splits the 2k-dimensional Sobol sequence into A then B", {
  # The unscrambled Sobol sequence starts at 0.5 in every dimension, and its
  # second point is 0.75 in the first half of dimensions and 0.25 in the second.
  matrices <- getSobolSampleMatrices(
    numberOfParameters = 1,
    numberOfSamples = 2
  )
  expect_equal(dim(matrices$A), c(2L, 1L))
  expect_equal(dim(matrices$B), c(2L, 1L))
  expect_equal(matrices$A[1, 1], 0.5)
  expect_equal(matrices$B[1, 1], 0.5)
  expect_equal(matrices$A[2, 1], 0.75)
  expect_equal(matrices$B[2, 1], 0.25)
})

test_that("getMixedMatrices replaces only the matching column of B with the column of A", {
  A <- matrix(1:6, nrow = 3, ncol = 2)
  B <- matrix(7:12, nrow = 3, ncol = 2)
  parameterPaths <- c("p1", "p2")

  result <- getMixedMatrices(A, B, parameterPaths)

  expected <- list(
    p1 = matrix(c(A[, 1], B[, 2]), nrow = 3, ncol = 2),
    p2 = matrix(c(B[, 1], A[, 2]), nrow = 3, ncol = 2)
  )
  expect_equal(result, expected)
})

test_that("varcalc computes first-order and total Sobol indices from model evaluations", {
  fU_list <- list(
    A = list(out1 = list(AUC = c(2, 4, 6, 8))),
    B = list(out1 = list(AUC = c(4, 8, 2, 6))),
    par1 = list(out1 = list(AUC = c(3, 5, 5, 7)))
  )
  outputList <- list(out1 = NA)

  result <- varcalc(fU_list, outputList)

  expected <- data.frame(
    Output = "out1",
    PK = "AUC",
    Parameter = "par1",
    Measure = c("FirstOrder", "Total"),
    Value = c(0.6, 0.8)
  )
  expect_equal(result, expected)
})

test_that("varcalc drops samples with NA or Inf evaluations before computing indices", {
  withInvalid <- list(
    A = list(out1 = list(AUC = c(2, 4, 6, 8, NA))),
    B = list(out1 = list(AUC = c(4, 8, 2, 6, Inf))),
    par1 = list(out1 = list(AUC = c(3, 5, 5, 7, 100)))
  )
  outputList <- list(out1 = NA)

  result <- varcalc(withInvalid, outputList)

  expected <- data.frame(
    Output = "out1",
    PK = "AUC",
    Parameter = "par1",
    Measure = c("FirstOrder", "Total"),
    Value = c(0.6, 0.8)
  )
  expect_equal(result, expected)
})

test_that("runSobol runs the full workflow with the randtoolbox sequence on a real simulation", {
  skip_if_not_installed("ospsuite")
  skip_on_ci()

  simPath <- system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
  simulation <- ospsuite::loadSimulation(simPath)

  parameters <- list(
    SAParameter$new(simulation = simulation, path = "Aciclovir|Lipophilicity"),
    SAParameter$new(simulation = simulation, path = "Aciclovir|Permeability")
  )
  output <- SAOutput$new(
    simulation = simulation,
    path = "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)"
  )
  output$addPKParameter(standardPKParameter = "C_max")

  sobolResults <- runSobol(
    simulation = simulation,
    parameters = parameters,
    outputs = list(output),
    numberOfSamples = 4,
    runParallel = FALSE
  )

  expect_named(sobolResults, c("Results", "Settings"))
  expect_s3_class(sobolResults$Results, "data.frame")
  expect_setequal(
    unique(sobolResults$Results$Measure),
    c("FirstOrder", "Total")
  )
  expect_true(all(
    sobolResults$Results$Value >= 0 & sobolResults$Results$Value <= 1
  ))
})
