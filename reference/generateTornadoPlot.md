# generateTornadoPlot

Function to generate a tornado plot of local sensitivity analysis or
uncertainty analysis results.

## Usage

``` r
generateTornadoPlot(
  sensitivityDataFrame,
  generateForUncertaintyAnalysis = FALSE
)
```

## Arguments

- sensitivityDataFrame:

  Sensitivity or uncertainty analysis results returned by `runSU`
  function.

- generateForUncertaintyAnalysis:

  Logical value. If `TRUE`, the tornado plots will be generated for
  uncertainty analysis results.

## Value

A list of ggplot tornado plots results, one corresponding to each output
path/PK parameter combination.
