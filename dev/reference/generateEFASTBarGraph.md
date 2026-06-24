# generateEFASTBarGraph

Function to generate a bar graph of EFAST sensitivity analysis results.

## Usage

``` r
generateEFASTBarGraph(gsaResultsDataframe)
```

## Arguments

- gsaResultsDataframe:

  EFAST results returned by the `runEFAST` function.

## Value

A list of ggplot bar graphs, one corresponding to each output path/PK
parameter combination.
