# checkParametersExistInSimulation

Function to verify that a list of paramter paths exists in a simulation.

## Usage

``` r
checkParametersExistInSimulation(
  simulation,
  parameterPaths,
  simulationName = NULL,
  stopIfNotFound = FALSE
)
```

## Arguments

- simulation:

  A PKML simulation object.

- parameterPaths:

  A vector of strings of parameter paths.

- simulationName:

  A descriptive name for the `simulation`.

- stopIfNotFound:

  Logical value. When `TRUE`, results in an error when a parameter in
  `parameterPaths` is not found in `simulation`.

## Value

A list of logical values indicating whether each outut path in
`parameterPaths` is found in `simulation`.
