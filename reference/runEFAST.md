# runEFAST

Run the EFAST algorithm.

## Usage

``` r
runEFAST(
  simulation,
  DDIsimulation = NULL,
  parameters,
  outputs,
  runParallel = TRUE,
  numberOfResamples = 1,
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

- runParallel:

  Logical value. EFAST sensitivity computation is run in parallel when
  `TRUE`.

- numberOfResamples:

  Number of times to run the EFAST algorithm steps with resampling, as
  per Saltelli, Tarantola & Chan, 1999.

- updateProgress:

  Logical value. Updates shiny app GUI with EFAST progress when `TRUE`.

- saveResults:

  Logical value. Results are saved if `TRUE`.

- saveFolder:

  Folder in which results will be saved if `saveResults` is set to
  `TRUE`.

- saveFileName:

  File name to which results will be saved if `saveResults` is set to
  `TRUE`.

## Value

Results of EFAST evaluation.
