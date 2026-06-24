# generateParameterCode

Generate code for setting up a SAParameter R6 object

## Usage

``` r
generateParameterCode(path, displayName, unit, parameterDistribution)
```

## Arguments

- path:

  The parameter path in the simulation.

- displayName:

  A display name for the parameter in lieu of the parameter path.

- unit:

  The units of the numerical values describing the parameter
  distribution, such as the mean and standard deviation of a normal
  distribution.

- parameterDistribution:

  An SADistribution R6 object describing the distribution of the
  parameter.
