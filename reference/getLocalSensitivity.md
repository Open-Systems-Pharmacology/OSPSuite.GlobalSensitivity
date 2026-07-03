# getLocalSensitivity

Function to run local sensitivity analysis

## Usage

``` r
getLocalSensitivity(
  simulation,
  DDIsimulation = NULL,
  parameterPaths,
  outputs,
  variationRange = 0.1,
  numberOfSteps = 2,
  runParallel = TRUE,
  updateProgress = NULL
)
```

## Arguments

- simulation:

  PKML simulation object.

- DDIsimulation:

  DDI PKML simulation object.

- parameterPaths:

  Paths of PKML simulation parameters to be analyzed\\

- outputs:

  List of `SAOutput` objects.

- variationRange:

  Variation range for sensitivity analysis.

- numberOfSteps:

  Number of points to evaluate model within the `variationRange`.

- runParallel:

  Logical value. Sensitivity analysis computation is run in parallel
  when `TRUE`.

- updateProgress:

  Logical value. Updates shiny app GUI with sensitivity analysis
  progress when `TRUE`.

## Value

description
