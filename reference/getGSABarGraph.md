# generateBarGraph

Function to generate a bar graph of Sobol or EFAST sensitivity analysis
results.

## Usage

``` r
getGSABarGraph(df, outputDisplayName, pk)
```

## Arguments

- df:

  Sobol or EFAST results dataframe.

- outputDisplayName:

  String. The display name of the output.

- pk:

  String. The name of the PK parameter

## Value

A list of ggplot bar graph plots, one corresponding to each output
path/PK parameter combination.
