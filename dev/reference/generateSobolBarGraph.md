# generateSobolBarGraph

Function to generate a bar graph of Sobol sensitivity analysis results.

## Usage

``` r
generateSobolBarGraph(gsaResultsDataframe)
```

## Arguments

- gsaResultsDataframe:

  Sobol results returned by the `runSobol` function.

## Value

A list of ggplot bar graphs, one corresponding to each output path/PK
parameter combination.
