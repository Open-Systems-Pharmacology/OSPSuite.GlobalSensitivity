#' Generate Pairwise Interpolation Grids
#'
#' @description
#' A helper function that takes a subset of simulation data and a list of parameter columns,
#' then generates a grid of interpolated response values for every pairwise combination
#' of parameters.
#'
#' @details
#' This function iterates through all unique pairs of columns specified in \code{paramCols}.
#' For each pair, it uses the \code{interp::interp} function to interpolate the scattered
#' simulation data onto a regular grid (defined by \code{gridSize}).
#'
#' To handle potential collinearity issues common in eFAST trajectories (which can cause
#' triangulation failures in \code{interp}), a small amount of random noise (jitter) is
#' added to the parameter values before interpolation.
#'
#' Pairs where one or both parameters have insufficient variation (fewer than 5 unique values)
#' are skipped to prevent errors.
#'
#' @param data A data frame containing the parameter inputs and the result output values.
#' @param paramCols A character vector of column names in \code{data} representing
#'   the input parameters (predictors) to be paired.
#' @param zVar A string specifying the column name in \code{data} to be used as
#'   the response variable (Z-axis).
#' @param jitterSize A numeric value specifying the magnitude of uniform noise added to
#'   parameters before interpolation to prevent collinearity errors.
#'   inside the function if not specified, though the argument is available for tuning.
#' @param gridSize An integer specifying the resolution of the interpolation grid
#'   (e.g., \code{40} results in a 40x40 grid).
#'
#' @return A data frame in long format containing the stacked results of all valid pairwise
#' interpolations. The data frame includes:
#' \describe{
#'   \item{x}{Interpolated x-coordinate values (parameter 1).}
#'   \item{y}{Interpolated y-coordinate values (parameter 2).}
#'   \item{z}{Interpolated response values.}
#'   \item{xLab}{Name of the parameter on the x-axis.}
#'   \item{yLab}{Name of the parameter on the y-axis.}
#' }
#' Returns \code{NULL} if no valid interpolation grids could be generated (e.g., if all
#' parameters are constant).
#'
#' @keywords internal
getPairwiseGrid <- function(data, paramCols, zVar, jitterSize, gridSize) {

  combos <- expand.grid(xVar = paramCols, yVar = paramCols, stringsAsFactors = FALSE)
  gridList <- list()

  for(i in seq_len(nrow(combos))) {
    xName <- combos$xVar[i]
    yName <- combos$yVar[i]

    if(xName == yName) next

    # Variance Check
    if(length(unique(data[[xName]])) < 5 || length(unique(data[[yName]])) < 5) {
      next
    }

    # interp crashes on perfect lines. Add noise to make triangles possible.
    x_jit <- data[[xName]] + runif(length(data[[xName]]), -jitterSize, jitterSize)
    y_jit <- data[[yName]] + runif(length(data[[yName]]), -jitterSize, jitterSize)
    z_val <- data[[zVar]]

    # Try/Catch block to handle 'shull' errors if they still occur
    interpRes <- tryCatch({
      xRange <- range(x_jit, na.rm = TRUE)
      yRange <- range(y_jit, na.rm = TRUE)

      interp::interp(
        x = x_jit,
        y = y_jit,
        z = z_val,
        xo = seq(xRange[1], xRange[2], length = gridSize), # 40x40 Grid
        yo = seq(yRange[1], yRange[2], length = gridSize),
        duplicate = "mean",
        linear = FALSE # Use spline interpolation (smoother than linear)
      )
    }, error = function(e) NULL)

    if(!is.null(interpRes)) {

      # Process the output matrix into a dataframe
      grid_z <- interpRes$z

      # Safety check for dimensions
      if(length(grid_z) < 4) next

      # Create the coordinate frame
      tmpDf <- expand.grid(x = interpRes$x, y = interpRes$y)
      tmpDf$z <- as.vector(grid_z)

      # Add Labels
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
#' Generates a matrix of contour plots visualizing the pairwise response surfaces
#' of model outputs based on eFAST sensitivity analysis results.
#'
#' @details
#' This function creates a visualization tool to identify parameter interactions and
#' non-linearities. The process involves:
#' \enumerate{
#'   \item Iterating through every unique Output and PK parameter (e.g., "C_max", "AUC") in the results.
#'   \item For each Output/PK combination, extracting the relevant simulation data.
#'   \item Calling \code{\link{getPairwiseGrid}} to generate interpolated surfaces for all parameter pairs.
#'   \item Optionally applying a log-transformation to the response values (Z-axis) to handle wide dynamic ranges common in PK/PD.
#'   \item Ranking parameters based on their Total Sensitivity index ($S_T$) so that the most
#'         influential parameters appear in the top-left of the plot matrix.
#'   \item Constructing a \code{ggplot2} object using \code{geom_contour_filled} and
#'         \code{facet_grid} to display the full matrix of pairwise interactions.
#' }
#'
#' The resulting plot is a scatterplot matrix where:
#' \itemize{
#'   \item Off-diagonal cells display filled contours of the response surface for two parameters.
#'   \item Diagonal cells display the names of the parameters.
#' }
#'
#' @param efastResults A list object containing the complete results of an eFAST sensitivity
#'   analysis. Expected structure:
#'   \describe{
#'     \item{InputOutputDf}{Data frame containing simulation inputs, output paths, PK parameters, and calculated values.}
#'     \item{Parameters}{Data frame or list containing parameter metadata, specifically `path` and `displayName`.}
#'     \item{Results}{Data frame containing calculated sensitivity indices (`Measure`, `Value`, `ParameterDisplayName`).}
#'     \item{Outputs}{Data frame or list containing output metadata (`path`, `displayName`).}
#'   }
#' @param jitterSize A numeric value representing the noise magnitude added during interpolation.
#'   Passed to \code{\link{getPairwiseGrid}}. Defaults to 0 (though \code{getPairwiseGrid} may apply a default if needed).
#' @param gridSize An integer specifying the resolution of the contour grids. Defaults to 40.
#' @param logScale Logical. If \code{TRUE} (default), the response variable (Z-axis) is log-transformed (log10)
#'   before plotting. This is recommended for PK data with large dynamic ranges. If data contains
#'   zeros or negative values, the function automatically falls back to a linear scale with a warning.
#'
#' @return A nested list of \code{ggplot} objects, structured as:
#'   \code{plotList[[outputName]][[pkParameter]]}.
#'   Each element is a complete ggplot object ready for printing or saving.
#'
#' @import ggplot2
#' @importFrom interp interp
#' @export
getContourPlot <- function(efastResults, jitterSize = 0, gridSize = 40, logScale = TRUE){

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
    longGridData <- getPairwiseGrid(subsetData, paramCols, "outputValues", jitterSize = jitterSize, gridSize = gridSize)

    if(is.null(longGridData)) next

    # B. Handle Log Scaling
    fillLabel <- "Value"
    if(logScale) {
      # Check for non-positive values which break log10
      if(any(longGridData$z <= 0, na.rm = TRUE)) {
        warning(paste("Log scale requested for", currPk, "but data contains non-positive values. Reverting to linear scale."))
      } else {
        longGridData$z <- log10(longGridData$z)
        fillLabel <- "Log10(Value)"
      }
    }

    longGridData$xLab <- sapply(longGridData$xLab, function(pth){
      efastResults$Parameters$displayName[efastResults$Parameters$path == pth]
    })
    longGridData$yLab <- sapply(longGridData$yLab, function(pth){
      efastResults$Parameters$displayName[efastResults$Parameters$path == pth]
    })

    # C. Ensure Factors are ordered correctly so the diagonal is diagonal
    totalSensitivityResults <- efastResults$Results[efastResults$Results$Measure == "Total" & efastResults$Results$Output == currOutput & efastResults$Results$PK == currPk, ]
    parameterTotalSensitivityRankOrder <- totalSensitivityResults$ParameterDisplayName[order(-totalSensitivityResults$Value)]

    longGridData$xLab <- factor(longGridData$xLab, levels = parameterTotalSensitivityRankOrder)
    longGridData$yLab <- factor(longGridData$yLab, levels = parameterTotalSensitivityRankOrder)

    # D. Create a separate dataframe for Diagonal Labels
    diagData <- data.frame(
      xLab = factor(parameterTotalSensitivityRankOrder, levels = parameterTotalSensitivityRankOrder),
      yLab = factor(parameterTotalSensitivityRankOrder, levels = parameterTotalSensitivityRankOrder),
      label = parameterTotalSensitivityRankOrder
    )

    # E. Plot
    p <- ggplot() +
      geom_contour_filled(
        data = longGridData,
        aes(x = x, y = y, z = z),
        bins = 10,
        show.legend = TRUE
      ) +
      scale_fill_brewer(palette = "Spectral", name = fillLabel) +

      geom_text(
        data = diagData,
        aes(x = 0.5, y = 0.5, label = label),
        size = 3.5, fontface = "bold",
        inherit.aes = FALSE
      ) +

      facet_grid(yLab ~ xLab, scales = "free") +

      labs(
        title = paste0("Response surface matrix: ",ifelse(test = logScale,yes = "Log10 ",no = ""), currPk),
        subtitle = efastResults$Outputs$displayName[efastResults$Outputs$path == currOutput]
      ) +
      theme_minimal() +
      theme(
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA),
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
