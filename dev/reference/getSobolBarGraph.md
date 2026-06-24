# getSobolBarGraph

Function to generate a bar graph of Sobol sensitivity analysis results.

## Usage

``` r
getSobolBarGraph(df, outputDisplayName, pk)
```

## Arguments

- df:

  Sobol results dataframe.

- outputDisplayName:

  String. The display name of the output.

- pk:

  String. The name of the PK parameter

## Value

A list of ggplot bar graph plots, one corresponding to each output
path/PK parameter combination.
