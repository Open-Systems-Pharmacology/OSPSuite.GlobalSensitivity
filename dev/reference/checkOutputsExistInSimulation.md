# checkOutputsExistInSimulation

Function to verify that a list of output paths exists in a simulation.

## Usage

``` r
checkOutputsExistInSimulation(
  simulation,
  outputPaths,
  simulationName,
  stopIfNotFound = FALSE
)
```

## Arguments

- simulation:

  A PKML simulation object.

- outputPaths:

  A victor of strings of output paths.

- simulationName:

  A descriptive name for the `simulation`.

- stopIfNotFound:

  Logical value. When `TRUE`, results in an error when an output in
  `outputPaths` is not found in `simulation`.

## Value

A list of logical values indicating whether each outut path in
`outputPaths` is found in `simulation`.
