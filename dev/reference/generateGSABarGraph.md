# generateGSABarGraph

Function to generate a bar graph of Sobol or EFAST sensitivity analysis
results.

## Usage

``` r
generateGSABarGraph(gsaResultsDataframe, plottingFunction)
```

## Arguments

- gsaResultsDataframe:

  Sobol or EFAST results returned by the `runSobol` or `runEFAST`
  functions, respectively.

- plottingFunction:

  The name of the function to use for plotting. Choose either
  `getSobolBarGraph` for Sobol results or `getEFASTBarGraph` for EFAST
  results.

## Value

A list of ggplot bar graphs, one corresponding to each output path/PK
parameter combination.
