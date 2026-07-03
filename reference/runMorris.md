# runMorris

Function to run Morris sensitivity analysis.

## Usage

``` r
runMorris(
  simulation,
  DDIsimulation = NULL,
  parameters,
  outputs,
  numberOfSamples,
  runParallel = TRUE,
  updateProgress = NULL,
  saveResults = FALSE,
  saveFolder = NULL,
  saveFileName = NULL
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

- numberOfSamples:

  The number of runs of the Morris algorithm to perform.

- runParallel:

  Logical value. Morris computation is run in parallel when `TRUE`.

- updateProgress:

  Logical value. Updates shiny app GUI with Morris algorithm progress
  when `TRUE`.

- saveResults:

  Logical value. If `TRUE`, the results will be saved.

- saveFolder:

  String indicating the path to the folder in which the results are to
  be saved.

- saveFileName:

  String indicating the file name to use when saving the results.

## Value

Morris sensitivity analysis results.
