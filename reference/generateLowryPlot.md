# generateLowryPlot

Function to generate a Lowry plot of Sobol or EFAST sensitivity analysis
results.

## Usage

``` r
generateLowryPlot(gsaResultsDataframe)
```

## Arguments

- gsaResultsDataframe:

  Sobol or EFAST results returned by the `runSobol` or `runFAST`
  functions, respectively.

## Value

A list of ggplot Lowry plots, one corresponding to each output path/PK
parameter combination.
