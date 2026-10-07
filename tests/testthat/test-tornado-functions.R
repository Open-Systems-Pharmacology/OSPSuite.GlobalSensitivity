# Local sensitivity / uncertainty results in the shape returned by runSU() and
# consumed by generateTornadoPlot(), as demonstrated in the "Getting started"
# vignette where generateTornadoPlot(sensitivityDataFrame = su$Results, ...) is
# used to visualise one-at-a-time local sensitivities.
makeSensitivityResults <- function() {
  data.frame(
    OutputDisplayName = "Plasma",
    PK = "C_max",
    ParameterDisplayName = c("Lipophilicity", "Permeability", "fu"),
    Sensitivity = c(0.8, -0.4, 0.05),
    stringsAsFactors = FALSE
  )
}

test_that("generateTornadoPlot returns one ggplot per output/PK combination", {
  results <- rbind(
    makeSensitivityResults(),
    transform(makeSensitivityResults(), PK = "AUC")
  )

  plotList <- generateTornadoPlot(results)

  expect_named(plotList, "Plasma")
  expect_named(plotList$Plasma, c("C_max", "AUC"))
  expect_s3_class(plotList$Plasma$C_max, "ggplot")
})

test_that("generateTornadoPlot labels the plot with the output and PK parameter", {
  plotList <- generateTornadoPlot(makeSensitivityResults())
  expect_equal(plotList$Plasma$C_max$labels$title, "Tornado Diagram for Plasma  -  C_max")
  expect_equal(plotList$Plasma$C_max$labels$y, "Sensitivity")
})

test_that("generateTornadoPlot orders the parameters by ascending absolute sensitivity", {
  plt <- generateTornadoPlot(makeSensitivityResults())$Plasma$C_max
  # coord_flip means the smallest magnitude is plotted first (bottom -> top)
  expect_equal(
    levels(plt$data$ParameterDisplayName),
    c("fu", "Permeability", "Lipophilicity")
  )
})

test_that("generateTornadoPlot encodes the sign of the sensitivity for colouring", {
  plt <- generateTornadoPlot(makeSensitivityResults())$Plasma$C_max
  signByParameter <- with(plt$data, tapply(as.character(signColumn), as.character(ParameterDisplayName), unique))

  expect_equal(signByParameter[["Lipophilicity"]], "1")
  expect_equal(signByParameter[["Permeability"]], "-1")
})

test_that("generateTornadoPlot replaces missing sensitivities with zero", {
  results <- makeSensitivityResults()
  results$Sensitivity[3] <- NA

  plt <- generateTornadoPlot(results)$Plasma$C_max
  fuValue <- plt$data$values[plt$data$ParameterDisplayName == "fu"]

  expect_equal(fuValue, 0)
})

test_that("generateTornadoPlot for uncertainty results uses the UncertaintyRatio column", {
  results <- makeSensitivityResults()
  results$UncertaintyRatio <- c(2, 0.5, 0.1)

  plotList <- generateTornadoPlot(results, generateForUncertaintyAnalysis = TRUE)

  expect_s3_class(plotList$Plasma$C_max, "ggplot")
  expect_equal(plotList$Plasma$C_max$labels$y, "Uncertainty")
})

test_that("generateTornadoPlot warns and returns NULL for uncertainty without UncertaintyRatio", {
  expect_warning(
    result <- generateTornadoPlot(makeSensitivityResults(), generateForUncertaintyAnalysis = TRUE),
    "No uncertainty analysis results found"
  )
  expect_null(result)
})
