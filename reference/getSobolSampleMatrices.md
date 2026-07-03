# getSobolSampleMatrices

Generates the two Sobol sample matrices `A` and `B` used as the base
sample for Sobol sensitivity analysis. A single low-discrepancy sequence
of dimension `2 * numberOfParameters` is generated and split into the
first and second halves of its columns.

## Usage

``` r
getSobolSampleMatrices(numberOfParameters, numberOfSamples)
```

## Arguments

- numberOfParameters:

  Number of parameters in the sensitivity analysis.

- numberOfSamples:

  Number of sample points (rows) to generate.

## Value

A list with matrices `A` and `B`, each with `numberOfSamples` rows and
`numberOfParameters` columns.
