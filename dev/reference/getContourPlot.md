# Generate Response Surface Contour Plots for eFAST Results

Generates a matrix of contour plots visualizing the pairwise response
surfaces of model outputs based on eFAST sensitivity analysis results.

## Usage

``` r
getContourPlot(efastResults, jitterSize = 0, gridSize = 40, logScale = TRUE)
```

## Arguments

- efastResults:

  A list object containing the complete results of an eFAST sensitivity
  analysis. Expected structure:

  InputOutputDf

  :   Data frame containing simulation inputs, output paths, PK
      parameters, and calculated values.

  Parameters

  :   Data frame or list containing parameter metadata, specifically
      `path` and `displayName`.

  Results

  :   Data frame containing calculated sensitivity indices (`Measure`,
      `Value`, `ParameterDisplayName`).

  Outputs

  :   Data frame or list containing output metadata (`path`,
      `displayName`).

- jitterSize:

  A numeric value representing the noise magnitude added during
  interpolation. Passed to
  [`getPairwiseGrid`](https://www.open-systems-pharmacology.org/OSPSuite.GlobalSensitivity/dev/reference/getPairwiseGrid.md).
  Defaults to 0 (though `getPairwiseGrid` may apply a default if
  needed).

- gridSize:

  An integer specifying the resolution of the contour grids. Defaults to
  40.

- logScale:

  Logical. If `TRUE` (default), the response variable (Z-axis) is
  log-transformed (log10) before plotting. This is recommended for PK
  data with large dynamic ranges. If data contains zeros or negative
  values, the function automatically falls back to a linear scale with a
  warning.

## Value

A nested list of `ggplot` objects, structured as:
`plotList[[outputName]][[pkParameter]]`. Each element is a complete
ggplot object ready for printing or saving.

## Details

This function creates a visualization tool to identify parameter
interactions and non-linearities. The process involves:

1.  Iterating through every unique Output and PK parameter (e.g.,
    "C_max", "AUC") in the results.

2.  For each Output/PK combination, extracting the relevant simulation
    data.

3.  Calling
    [`getPairwiseGrid`](https://www.open-systems-pharmacology.org/OSPSuite.GlobalSensitivity/dev/reference/getPairwiseGrid.md)
    to generate interpolated surfaces for all parameter pairs.

4.  Optionally applying a log-transformation to the response values
    (Z-axis) to handle wide dynamic ranges common in PK/PD.

5.  Ranking parameters based on their Total Sensitivity index (\$S_T\$)
    so that the most influential parameters appear in the top-left of
    the plot matrix.

6.  Constructing a `ggplot2` object using `geom_contour_filled` and
    `facet_grid` to display the full matrix of pairwise interactions.

The resulting plot is a scatterplot matrix where:

- Off-diagonal cells display filled contours of the response surface for
  two parameters.

- Diagonal cells display the names of the parameters.
