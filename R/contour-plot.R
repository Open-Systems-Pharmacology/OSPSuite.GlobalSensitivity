
# --- Helper to Generate "Long" Interpolated Data ---
getPairwiseGrid <- function(data, paramCols, zVar) {
  # Create a list of all parameter pairs (Square Matrix)
  combos <- expand.grid(xVar = paramCols, yVar = paramCols, stringsAsFactors = FALSE)

  gridList <- list()

  for(i in seq_len(nrow(combos))) {
    xName <- combos$xVar[i]
    yName <- combos$yVar[i]

    # Skip diagonal for interpolation (Can't interpolate X vs X)
    if(xName == yName) next

    # Interpolate
    # Use tryCatch to handle flat data/errors gracefully
    interpRes <- tryCatch({
      xRange <- range(data[[xName]], na.rm = TRUE)
      yRange <- range(data[[yName]], na.rm = TRUE)

      akima::interp(
        x = data[[xName]],
        y = data[[yName]],
        z = data[[zVar]],
        xo = seq(xRange[1], xRange[2], length = 40), # 40x40 grid is enough for small panels
        yo = seq(yRange[1], yRange[2], length = 40),
        duplicate = "mean"
      )
    }, error = function(e) NULL)

    if(!is.null(interpRes)) {
      # Convert to DF
      tmpDf <- expand.grid(x = interpRes$x, y = interpRes$y)
      tmpDf$z <- as.vector(interpRes$z)

      # Add Label Columns for Faceting
      tmpDf$xLab <- xName
      tmpDf$yLab <- yName

      gridList[[length(gridList) + 1]] <- tmpDf
    }
  }

  if(length(gridList) == 0) return(NULL)
  return(do.call(rbind, gridList))
}

getContourPlot <- function(efastResults){

  # Identify Parameters
  metadataCols <- c("output", "pk", "outputValues")
  allCols <- names(efst$InputOutputDf)
  paramCols <- allCols[!allCols %in% metadataCols]
  paramColDisplayNames <- sapply(paramCols,function(pth){efastResults$Parameters$displayName[efastResults$Parameters$path == pth]})
  opPkDf <- unique(efst$InputOutputDf[, c("output", "pk")])

  plotList <- list()

  for (op in unique(opPkDf$output)){
    plotList[[op]] <- list()
    for (pk in unique(opPkDf$pk[opPkDf$output == op])){
      plotList[[op]][[pk]] <- list()
    }
  }

  for(nr in seq_len(nrow(opPkDf))) {

    currRow    <- opPkDf[nr, ]
    currOutput <- currRow$output
    currPk     <- currRow$pk

    subsetData <- subset(efst$InputOutputDf, output == currOutput & pk == currPk)

    if(nrow(subsetData) < 20) {
      #Skipping: Insufficient data.
      next
    }

    # A. Generate the massive long dataframe
    longGridData <- getPairwiseGrid(subsetData, paramCols, "outputValues")

    longGridData$xLab <- sapply(longGridData$xLab,function(pth){efastResults$Parameters$displayName[efastResults$Parameters$path == pth]})
    longGridData$yLab <- sapply(longGridData$yLab,function(pth){efastResults$Parameters$displayName[efastResults$Parameters$path == pth]})
    if(is.null(longGridData)) next

    # B. Ensure Factors are ordered correctly so the diagonal is diagonal
    totalSensitivityResults <- efastResults$Results[efastResults$Results$Measure == "Total", ]
    parameterTotalSensitivityRankOrder <- totalSensitivityResults$ParameterDisplayName[order(-totalSensitivityResults$Value)]

    longGridData$xLab <- factor(longGridData$xLab, levels = parameterTotalSensitivityRankOrder)
    longGridData$yLab <- factor(longGridData$yLab, levels = parameterTotalSensitivityRankOrder)

    # C. Create a separate dataframe for Diagonal Labels
    diagData <- data.frame(
      xLab = factor(parameterTotalSensitivityRankOrder, levels = parameterTotalSensitivityRankOrder),
      yLab = factor(parameterTotalSensitivityRankOrder, levels = parameterTotalSensitivityRankOrder),
      label = parameterTotalSensitivityRankOrder
    )

    # D. Plot
    p <- ggplot() +
      geom_contour_filled(
        data = longGridData,
        aes(x = x, y = y, z = z),
        bins = 10,
        show.legend = TRUE
      ) +
      scale_fill_brewer(palette = "Spectral") +

      geom_text(
        data = diagData,
        aes(x = 0.5, y = 0.5, label = label),
        size = 3.5, fontface = "bold",
        inherit.aes = FALSE
      ) +

      facet_grid(yLab ~ xLab, scales = "free") +

      labs(
        title = paste("Response surface matrix:", currPk),
        subtitle = efastResults$Outputs$displayName[efastResults$Outputs$path == currOutput]
      ) +
      theme_minimal() +
      theme(
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        legend.title = element_blank(),
        panel.spacing = unit(0.1, "lines"),
        strip.text = element_blank(),
        panel.border = element_rect(color = "grey90", fill = NA)
      )

    plotList[[currOutput]][[currPk]] <- p

  }
  return(plotList)
}
