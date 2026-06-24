# runSU

Function to run sensitivity analysis and then run uncertainy analysis
for parameters deemed sensitive. Main parameters:

## Usage

``` r
runSU(
  simulation,
  DDIsimulation = NULL,
  customParameters = NULL,
  outputs,
  evaluateForAllParameters = FALSE,
  variationRange = 0.1,
  numberOfSensitivityAnalysisSteps = 2,
  sensitivityThreshold = 0.1,
  runUncertaintyAnalysis = TRUE,
  runUncertaintlyOnlyForSensitiveParameters = TRUE,
  numberOfUncertaintyAnalysisSamples = 100,
  quantiles = c(0.05, 0.5, 0.95),
  saveResults = FALSE,
  saveFolder = NULL,
  saveFileName = NULL,
  runParallel = TRUE,
  updateProgressSensitivity = NULL,
  updateProgressUncertainty = NULL
)
```

## Arguments

- simulation:

  PKML simulation object.

- DDIsimulation:

  DDI PKML simulation object.

- customParameters:

  List of `SAParameter` objects with user-specified distributions.

- outputs:

  List of `SAOutput` objects.

- evaluateForAllParameters:

  Logical value. If `TRUE`, all model parameters will be evaluated for
  sensitivity and those with a sensitivity greater than
  `sensitivityThreshold` will undergo uncertainty analysis. Any
  parameters not included in the `customParameters` list will have a
  log-uniform distribution with bounds set by `variationRange`.
  Sensitivity analysis settings:

- variationRange:

  The variation range used to define the log-uniform distribution of the
  parameters in `parameterPaths`.

- numberOfSensitivityAnalysisSteps:

  Positive integer. Number of steps at which the model is simulated
  within a parameter's `variationRange` during the sensitivity analysis.

- sensitivityThreshold:

  Positive numeric value. The threshold sensitivity above which a
  parameter will undergo uncertainty analysis if
  `runUncertaintyAnalysis` and
  `runUncertaintlyOnlyForSensitiveParameters` are both `TRUE`.
  Uncertainty analysis settings:

- runUncertaintyAnalysis:

  Logical value. If `TRUE`, uncertainty analysis will run.

- runUncertaintlyOnlyForSensitiveParameters:

  Logical value. If `TRUE`, uncertainty analysis will only be run for
  the parameters for which sensitivity exceeds `sensitivityThreshold`.

- numberOfUncertaintyAnalysisSamples:

  Positive integer value giving the number of Monte Carlo runs for each
  parameter.

- quantiles:

  Vector of numerical values between 0 and 1. The `quantiles` input sets
  the percentiles of the PK parameters to be evaluated in addition to
  the 50% and 95% percentiles, which are computed by default. Save
  settings:

- saveResults:

  Logical value. If `TRUE`, the results will be saved.

- saveFolder:

  String indicating the path to the folder in which the results are to
  be saved.

- saveFileName:

  String indicating the file name to use when saving the results.
  Parallel run and updating settings

- runParallel:

  Logical value. Sensitivity computation is run in parallel when `TRUE`.

- updateProgressSensitivity:

  Logical value. Updates shiny app GUI with sensitivity analysis
  progress when `TRUE`.

- updateProgressUncertainty:

  Logical value. Updates shiny app GUI with uncertainty analysis
  progress when `TRUE`.

## Value

Sensitivity and uncertainty analysis results.
