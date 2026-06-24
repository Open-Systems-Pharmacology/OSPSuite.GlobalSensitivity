# Generate Pairwise Interpolation Grids

A helper function that takes a subset of simulation data and a list of
parameter columns, then generates a grid of interpolated response values
for every pairwise combination of parameters.

## Usage

``` r
getPairwiseGrid(data, paramCols, zVar, jitterSize, gridSize)
```

## Arguments

- data:

  A data frame containing the parameter inputs and the result output
  values.

- paramCols:

  A character vector of column names in `data` representing the input
  parameters (predictors) to be paired.

- zVar:

  A string specifying the column name in `data` to be used as the
  response variable (Z-axis).

- jitterSize:

  A numeric value specifying the magnitude of uniform noise added to
  parameters before interpolation to prevent collinearity errors. inside
  the function if not specified, though the argument is available for
  tuning.

- gridSize:

  An integer specifying the resolution of the interpolation grid (e.g.,
  `40` results in a 40x40 grid).

## Value

A data frame in long format containing the stacked results of all valid
pairwise interpolations. The data frame includes:

- x:

  Interpolated x-coordinate values (parameter 1).

- y:

  Interpolated y-coordinate values (parameter 2).

- z:

  Interpolated response values.

- xLab:

  Name of the parameter on the x-axis.

- yLab:

  Name of the parameter on the y-axis.

Returns `NULL` if no valid interpolation grids could be generated (e.g.,
if all parameters are constant).

## Details

This function iterates through all unique pairs of columns specified in
`paramCols`. For each pair, it uses the
[`interp::interp`](https://rdrr.io/pkg/interp/man/interp.html) function
to interpolate the scattered simulation data onto a regular grid
(defined by `gridSize`).

To handle potential collinearity issues common in eFAST trajectories
(which can cause triangulation failures in `interp`), a small amount of
random noise (jitter) is added to the parameter values before
interpolation.

Pairs where one or both parameters have insufficient variation (fewer
than 5 unique values) are skipped to prevent errors.
