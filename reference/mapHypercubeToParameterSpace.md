# mapHypercubeToParameterSpace

Use parameter distributions to map points in the sampling hypercube to
values within the domain of each parameter's distribution.

## Usage

``` r
mapHypercubeToParameterSpace(parameters, hypercube)
```

## Arguments

- parameters:

  List of `SAParameter` objects.

- hypercube:

  A hypercube of points in the percentile space (0,1) that is to be
  mapped onto the domain of each parameter's distribution.

## Value

A list of points in the domain of each parameter at which the model is
to be evaluated.
