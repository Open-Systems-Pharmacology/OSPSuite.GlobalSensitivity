#' Generate Pairwise Interpolation Grids
#'
#' @description
#' A helper function that takes a dataset and a list of parameter columns,
#' then generates a grid of interpolated values for every pairwise combination
#' of parameters (excluding self-vs-self).
#'
#' @details
#' This function utilizes \code{interp::interp} to create a 40x40 interpolated
#' grid for every combination of \code{paramCols}. It is designed to prepare
#' data for facetted contour plotting.
#'
#' @param data A data frame containing the parameter inputs and the result output.
#' @param paramCols A character vector of column names in \code{data} representing
#'   the input parameters (predictors).
#' @param zVar A string specifying the column name in \code{data} to be used as
#'   the response variable (Z-axis).
#'
#' @return A data frame in long format containing the following columns:
#' \item{x}{Interpolated x-coordinate values.}
#' \item{y}{Interpolated y-coordinate values.}
#' \item{z}{Interpolated response values.}
#' \item{xLab}{Name of the parameter on the x-axis.}
#' \item{yLab}{Name of the parameter on the y-axis.}
#' Returns \code{NULL} if no valid interpolation grids could be generated.
#'
#' @keywords internal
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

      # CHANGED: Using interp::interp instead of akima::interp
      interp::interp(
        x = data[[xName]],
        y = data[[yName]],
        z = data[[zVar]],
        xo = seq(xRange[1], xRange[2], length = 40), # 40x40 grid
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




#' Generate Response Surface Contour Plots for eFAST Results
#'
#' @description
#' Generates a matrix of contour plots visualizing the pairwise response surface
#' of model outputs. The plots are organized by simulation timepoint/ID (`pk`)
#' and output variable.
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Iterates through every unique Output and PK (time point) in the results.
#'   \item Interpolates the response surface for every pair of input parameters using
#'         \code{\link{getPairwiseGrid}}.
#'   \item Orders the parameters in the grid based on the Total Sensitivity index
#'         (most sensitive parameters appear first).
#'   \item Generates a \code{ggplot2} object using \code{geom_contour_filled},
#'         facetted by the parameter pairs.
#' }
#'
#' @param efastResults A list object containing the results of an eFAST sensitivity
#'   analysis. Expected structure:
#'   \itemize{
#'     \item \code{InputOutputDf}: Data frame with inputs, `output`, `pk`, and `outputValues`.
#'     \item \code{Parameters}: Metadata including `path` and `displayName`.
#'     \item \code{Results}: Sensitivity indices including `Measure` and `Value`.
#'     \item \code{Outputs}: Metadata for output variables.
#'   }
#'
#' @return A nested list of \code{ggplot} objects, structured as:
#'   \code{plotList[[outputName]][[pkId]]}.
#'
#' @import ggplot2
#' @importFrom interp interp
#' @export
getContourPlot <- function(efastResults){

  # Identify Parameters
  metadataCols <- c("output", "pk", "outputValues")
  allCols <- names(efastResults$InputOutputDf)
  paramCols <- allCols[!allCols %in% metadataCols]

  paramColDisplayNames <- sapply(paramCols, function(pth){
    efastResults$Parameters$displayName[efastResults$Parameters$path == pth]
  })

  opPkDf <- unique(efastResults$InputOutputDf[, c("output", "pk")])

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

    subsetData <- subset(efastResults$InputOutputDf, output == currOutput & pk == currPk)

    if(nrow(subsetData) < 20) {
      # Skipping: Insufficient data.
      next
    }

    # A. Generate the massive long dataframe
    longGridData <- getPairwiseGrid(subsetData, paramCols, "outputValues")

    if(is.null(longGridData)) next

    longGridData$xLab <- sapply(longGridData$xLab, function(pth){
      efastResults$Parameters$displayName[efastResults$Parameters$path == pth]
    })
    longGridData$yLab <- sapply(longGridData$yLab, function(pth){
      efastResults$Parameters$displayName[efastResults$Parameters$path == pth]
    })

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
