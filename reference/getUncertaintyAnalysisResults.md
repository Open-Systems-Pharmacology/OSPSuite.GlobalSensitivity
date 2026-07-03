# getUncertaintyAnalysisResults

Function to run uncertainty analysis

## Usage

``` r
getUncertaintyAnalysisResults(
  simulation,
  DDIsimulation = NULL,
  parameters,
  outputs,
  numberOfUncertaintyAnalysisSamples,
  sensitiveParameterPaths,
  runParallel = TRUE,
  updateProgress = NULL
)
```

## Arguments

- simulation:

  PKML simulation object.

- DDIsimulation:

  DDI PKML simulation object.

- parameters:

  List of `SAParameter` objects.

- outputs:

  List of `SAOutput` objects.

- numberOfUncertaintyAnalysisSamples:

  Number of samples at which to evaluate the simulation for the
  uncertainty analysis.

- sensitiveParameterPaths:

  Paths of `simulation` parameters that are deemed sensitive.

- runParallel:

  Logical value. Uncertainty analysis computation is run in parallel
  when `TRUE`.

- updateProgress:

  Logical value. Updates shiny app GUI with uncertainty analysis
  progress when `TRUE`.

## Value

description
