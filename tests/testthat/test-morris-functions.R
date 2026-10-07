test_that("Bmat builds the (k+1) x k stepped lower-triangular sampling matrix", {
  expect_equal(
    Bmat(2),
    matrix(c(0, 1, 1, 0, 0, 1), nrow = 3, ncol = 2)
  )
  expect_equal(dim(Bmat(4)), c(5L, 4L))
})

test_that("Jmat is a (k+1) x k matrix of ones", {
  J <- Jmat(3)
  expect_equal(dim(J), c(4L, 3L))
  expect_true(all(J == 1))
})

test_that("J1mat is a column vector of ones of length k+1", {
  J1 <- J1mat(3)
  expect_equal(dim(J1), c(4L, 1L))
  expect_true(all(J1 == 1))
})

test_that("Dmat is a diagonal matrix whose non-zero entries are +/- 1", {
  set.seed(42)
  D <- Dmat(5)
  expect_equal(dim(D), c(5L, 5L))
  expect_true(all(diag(D) %in% c(1, -1)))
  expect_true(all(D[upper.tri(D)] == 0))
  expect_true(all(D[lower.tri(D)] == 0))
})

test_that("Pmat is a permutation matrix with exactly one 1 per row and column", {
  set.seed(123)
  P <- Pmat(4)
  expect_equal(dim(P), c(4L, 4L))
  expect_true(all(P %in% c(0, 1)))
  expect_true(all(rowSums(P) == 1))
  expect_true(all(colSums(P) == 1))
})

test_that("getTrajectory returns a (k+1) x k matrix of values within the unit hypercube", {
  set.seed(7)
  trajectory <- getTrajectory(numberOfParameters = 3, numberOfGridPartitions = 8)
  expect_equal(dim(trajectory), c(4L, 3L))
  expect_true(all(is.finite(trajectory)))
  expect_true(all(trajectory >= 0 & trajectory <= 1))
})

test_that("getTrajectory rounds an odd number of grid partitions up to an even count", {
  set.seed(7)
  oddPartitions <- getTrajectory(numberOfParameters = 2, numberOfGridPartitions = 7)
  set.seed(7)
  evenPartitions <- getTrajectory(numberOfParameters = 2, numberOfGridPartitions = 8)
  expect_equal(oddPartitions, evenPartitions)
})

test_that("Morris summaryFunctions compute the elementary-effect summaries", {
  effects <- c(-2, 0, 2, 4)

  expect_equal(summaryFunctions$mu(effects), mean(effects))
  expect_equal(summaryFunctions$mustar(effects), mean(abs(effects)))
  expect_equal(summaryFunctions$stdv(effects), sd(effects))
  expect_equal(
    summaryFunctions$rankingNorm(effects),
    sqrt(mean(abs(effects))^2 + sd(effects)^2)
  )
})

# Morris results data frame in the shape returned by runMorris() and consumed by
# generateMorrisPlot(), as illustrated in the "Getting started" vignette which
# plots the elementary-effect summaries (mu*, sigma) for each parameter.
makeMorrisResults <- function(output = "Out1", pk = "C_max") {
  data.frame(
    Output = output,
    PK = pk,
    Parameter = c("p1", "p2", "p3"),
    mustar = c(3, 2, 1),
    stdv = c(0.5, 1, 0.2),
    rankingNorm = c(3, 2, 1),
    stringsAsFactors = FALSE
  )
}

test_that("generateMorrisPlot returns one ggplot per output/PK combination", {
  morrisResults <- rbind(makeMorrisResults(pk = "C_max"), makeMorrisResults(pk = "AUC"))

  morrisPlots <- generateMorrisPlot(morrisResults)

  expect_named(morrisPlots, "Out1")
  expect_named(morrisPlots$Out1, c("C_max", "AUC"))
  expect_s3_class(morrisPlots$Out1$C_max, "ggplot")
})

test_that("generateMorrisPlot labels the mu* and sigma axes of the Morris plane", {
  plt <- generateMorrisPlot(makeMorrisResults())$Out1$C_max

  expect_equal(plt$labels$x, "\u03bc*")
  expect_equal(plt$labels$y, "\u03c3")
  expect_equal(plt$labels$title, "Morris sensitivity")
})

test_that("generateMorrisPlot ranks parameters by descending rankingNorm", {
  plt <- generateMorrisPlot(makeMorrisResults())$Out1$C_max

  # Parameters are reordered by decreasing rankingNorm before labelling 1..n
  expect_equal(as.character(plt$data$Parameter), c("p1", "p2", "p3"))
  expect_equal(as.character(plt$data$label), c("1", "2", "3"))
})

test_that("generateMorrisPlot applies a log10 transform to the axes when requested", {
  morrisResults <- makeMorrisResults()

  linearPlot <- generateMorrisPlot(morrisResults, logPlot = FALSE)$Out1$C_max
  logPlot <- generateMorrisPlot(morrisResults, logPlot = TRUE)$Out1$C_max

  linearX <- ggplot2::ggplot_build(linearPlot)$data[[1]]$x
  logX <- ggplot2::ggplot_build(logPlot)$data[[1]]$x

  expect_equal(sort(logX), sort(log10(morrisResults$mustar)))
  expect_equal(sort(linearX), sort(morrisResults$mustar))
})
