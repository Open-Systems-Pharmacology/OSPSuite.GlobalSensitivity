# generateMorrisPlot

Function to generate a plot of Morris sensitivity analysis.

## Usage

``` r
generateMorrisPlot(morrisResults, logPlot = FALSE)
```

## Arguments

- morrisResults:

  Morris sensitivity results returned by `runMorris` function.

- logPlot:

  Logical setting. The Morris results are plotted on a logarithmic scale
  if `TRUE`.

## Value

A list of ggplots of Morris sensitivity analysis results, one
corresponding to each output path/PK parameter combination.
