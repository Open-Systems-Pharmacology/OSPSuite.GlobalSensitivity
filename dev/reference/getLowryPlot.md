# getLowryPlot

Function to build a Lowry plot in ggplot for a spcific output and PK
parameter.

## Usage

``` r
getLowryPlot(df, outputDisplayName, pk)
```

## Arguments

- df:

  Sobol or EFAST results dataframe.

- outputDisplayName:

  String. The display name of the output.

- pk:

  String. The name of the PK parameter

## Value

A ggplot Lowry plot for a specific output path and PK parameter
combination.
