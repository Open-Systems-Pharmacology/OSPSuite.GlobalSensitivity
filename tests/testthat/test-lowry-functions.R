# Variance-based results data frame mirroring the n = 3 parameter example used
# throughout the "Mathematical overview of variance-based methods" vignette and
# the accompanying publication (Najjar et al., CPT:PSP 2024). The first-order
# indices sum to less than one so that interactions are visible in the Lowry
# plot, as described for the total-effect indices S_i^T >= S_i.
makeGSAResults <- function(firstOrder = c(0.5, 0.3, 0.1),
                           total = c(0.6, 0.35, 0.15),
                           output = "Out1",
                           outputDisplayName = "Plasma",
                           pk = "C_max") {
  parameters <- paste0("p", seq_along(firstOrder))
  rbind(
    data.frame(
      Output = output, OutputDisplayName = outputDisplayName, PK = pk,
      Parameter = parameters, ParameterDisplayName = parameters,
      Measure = "FirstOrder", Value = firstOrder, stringsAsFactors = FALSE
    ),
    data.frame(
      Output = output, OutputDisplayName = outputDisplayName, PK = pk,
      Parameter = parameters, ParameterDisplayName = parameters,
      Measure = "Total", Value = total, stringsAsFactors = FALSE
    )
  )
}

test_that("generateLowryPlot returns one ggplot per output/PK combination", {
  results <- rbind(
    makeGSAResults(pk = "C_max"),
    makeGSAResults(pk = "AUC", firstOrder = c(0.4, 0.4, 0.1), total = c(0.5, 0.45, 0.2))
  )

  plotList <- generateLowryPlot(results)

  expect_named(plotList, "Out1")
  expect_named(plotList$Out1, c("C_max", "AUC"))
  expect_s3_class(plotList$Out1$C_max, "ggplot")
  expect_s3_class(plotList$Out1$AUC, "ggplot")
})

test_that("getLowryPlot handles the single-parameter case without error", {
  singleParameter <- makeGSAResults(firstOrder = 0.9, total = 1)

  expect_no_error(plt <- getLowryPlot(singleParameter, "Plasma", "C_max"))
  expect_s3_class(plt, "ggplot")
})

test_that("getLowryPlot titles the plot with the PK parameter and output display name", {
  plt <- getLowryPlot(makeGSAResults(), "Plasma", "C_max")
  expect_equal(plt$labels$title, "Global sensitivity of C_max of Plasma")
})

test_that("generateSobolBarGraph returns one labelled ggplot per output/PK combination", {
  plotList <- generateSobolBarGraph(makeGSAResults())

  expect_named(plotList, "Out1")
  expect_named(plotList$Out1, "C_max")
  expect_s3_class(plotList$Out1$C_max, "ggplot")
  expect_equal(plotList$Out1$C_max$labels$title, "Sobol sensitivity")
})

test_that("generateEFASTBarGraph returns one labelled ggplot per output/PK combination", {
  plotList <- generateEFASTBarGraph(makeGSAResults())

  expect_s3_class(plotList$Out1$C_max, "ggplot")
  expect_equal(plotList$Out1$C_max$labels$title, "EFAST sensitivity")
})

test_that("getGSABarGraph orders the parameters by descending first-order index", {
  # p3 has the largest first-order index and should therefore be the first level
  results <- makeGSAResults(firstOrder = c(0.1, 0.3, 0.6), total = c(0.2, 0.35, 0.7))

  plt <- getGSABarGraph(results, "Plasma", "C_max")
  parameterLevels <- levels(plt$data$Parameter)

  expect_equal(parameterLevels, c("p3", "p2", "p1"))
})
