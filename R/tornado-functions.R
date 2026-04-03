#' @title generateTornadoPlot
#' @description Function to generate a tornado plot of local sensitivity analysis or uncertainty analysis results.
#' @param sensitivityDataFrame Sensitivity or uncertainty analysis results returned by `runSU` function.
#' @param generateForUncertaintyAnalysis Logical value.  If `TRUE`, the tornado plots will be generated for uncertainty analysis results.
#' @return A list of ggplot tornado plots results, one corresponding to each output path/PK parameter combination.
#' @export
generateTornadoPlot <- function(sensitivityDataFrame, generateForUncertaintyAnalysis = FALSE) {
  valuesColumnName <- "Sensitivity"
  ylabel <- "Sensitivity"

  if (generateForUncertaintyAnalysis) {
    if (!("UncertaintyRatio" %in% names(sensitivityDataFrame))) {
      warning("No uncertainty analysis results found in 'sensitivityDataFrame'.  No tornado plot will be generated.")
      return(NULL)
    }

    valuesColumnName <- "UncertaintyRatio"
    ylabel <- "Uncertainty"
  }

  # Make sure 'Sensitivity' is numeric
  sensitivityDataFrame$values <- as.numeric(sensitivityDataFrame[[valuesColumnName]])
  sensitivityDataFrame$values[is.na(sensitivityDataFrame$values)] <- 0
  sensitivityDataFrame$signColumn <- as.factor(sign(sensitivityDataFrame$values))

  # Create an empty list to store ggplots
  plotList <- list()

  for (op in unique(sensitivityDataFrame$OutputDisplayName)) {
    plotList[[op]] <- list()
    for (pk in unique(sensitivityDataFrame$PK[sensitivityDataFrame$OutputDisplayName == op])) {

      # Subset the data for the current combination
      subsetData <- sensitivityDataFrame[sensitivityDataFrame$OutputDisplayName == op & sensitivityDataFrame$PK == pk, ]

      # Order the data by 'Sensitivity'
      orderedData <- subsetData[order(abs(subsetData$values)), ]

      # Factor the factor and order the factor levels by sensitivity
      orderedData$ParameterDisplayName <- factor(x = orderedData$ParameterDisplayName, levels = unique(orderedData$ParameterDisplayName))

      # Create the tornado diagram
      currentPlot <- ggplot(orderedData, aes(x = ParameterDisplayName, y = values)) +
        geom_bar(
          stat = "identity", aes(fill = signColumn),
          position = "identity", color = "black"
        ) +
        scale_fill_manual(
          values = c("-1" = "#F8766D", "0" = "black", "1" = "#00BFC4")
        ) +
        coord_flip() +
        labs(
          title = paste("Tornado Diagram for", op, " - ", pk),
          x = "Parameter display name", y = ylabel
        ) +
        theme(legend.position = "none")

      # Append the current ggplot to the list
      plotList[[op]][[pk]] <- currentPlot
    }
  }
  return(plotList)
}
