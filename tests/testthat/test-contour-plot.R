# The contour-plot helpers visualise pairwise EFAST response surfaces. These
# tests exercise the input-validation / data-sufficiency branches that do not
# require the optional triangulation back-end (interp), so they can run anywhere.

test_that("getPairwiseGrid returns NULL when the parameters have no variation", {
  # Fewer than five unique values per parameter triggers the variance check and
  # short-circuits before any interpolation is attempted.
  constantData <- data.frame(
    p1 = rep(1, 10),
    p2 = rep(2, 10),
    z = seq_len(10)
  )

  expect_null(getPairwiseGrid(constantData, c("p1", "p2"), "z", jitterSize = 0, gridSize = 10))
})

test_that("getContourPlot returns an empty plot per output/PK combination when data is insufficient", {
  # Fewer than 20 simulation rows per output/PK combination is skipped, leaving
  # the pre-initialised (empty) nested list structure in place.
  efastResults <- list(
    InputOutputDf = data.frame(
      p1 = runif(5),
      p2 = runif(5),
      output = "Out1",
      pk = "C_max",
      outputValues = runif(5),
      stringsAsFactors = FALSE
    ),
    Parameters = data.frame(
      path = c("p1", "p2"),
      displayName = c("P1", "P2"),
      stringsAsFactors = FALSE
    ),
    Results = data.frame(
      Measure = character(0), Value = numeric(0),
      Output = character(0), PK = character(0),
      ParameterDisplayName = character(0)
    ),
    Outputs = data.frame(path = "Out1", displayName = "Plasma", stringsAsFactors = FALSE)
  )

  plotList <- getContourPlot(efastResults)

  expect_named(plotList, "Out1")
  expect_named(plotList$Out1, "C_max")
  expect_length(plotList$Out1$C_max, 0L)
})
