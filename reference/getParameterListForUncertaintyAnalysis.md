# getParameterListForUncertaintyAnalysis

This function assigns a log-uniform distribution with a variation range
given by `variationRange` for parameters with a previously unspecified
distribution.

## Usage

``` r
getParameterListForUncertaintyAnalysis(
  parameterPaths,
  simulation,
  variationRange = 0.1
)
```

## Arguments

- parameterPaths:

  Paths of the parameters in the `simulation`.

- simulation:

  The simulation object which contains parameters at paths
  `parameterPaths`.

- variationRange:

  The variation range used to define the log-uniform distribution of the
  parameters in `parameterPaths`.

## Value

A list of `SAParameter` objects.
