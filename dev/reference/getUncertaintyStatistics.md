# getUncertaintyStatistics

Function to summarize the Monte Carlo draws of one parameter / output /
PK parameter combination. Failed draws (`NA`) are excluded. If fewer
than `minimumValidDraws` draws remain, every statistic is `NA`
(undefined): missing evidence is never reported as zero uncertainty.

## Usage

``` r
getUncertaintyStatistics(values, minimumValidDraws, quantiles = NULL)
```

## Arguments

- values:

  Numeric vector with one entry per draw, `NA` for draws that failed.

- minimumValidDraws:

  Minimum number of valid draws required to compute the statistics.

- quantiles:

  Additional quantiles to evaluate.

## Value

A list with `defined`, `ValidDraws`, `Mean`, `Percentile_2.5`, `Median`,
`Percentile_97.5`, `UncertaintyRatio` and `quantileValues` (named by
`as.character(quantiles)`).
