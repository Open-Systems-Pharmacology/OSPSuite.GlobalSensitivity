# writeSUFunctionToFile

Write to file code for generating a local sensitivity and uncertainty
analyses.

## Usage

``` r
writeSUFunctionToFile(
  runUncertaintyCheckbox,
  runSensitivityAnalysisForAllParameters,
  sensitivityThreshold,
  variationRangeNumericInput,
  numberOfStepsNumericInput,
  quantilesTestInput,
  numberOfUncertaintySamples,
  runParallel
)
```

## Arguments

- runUncertaintyCheckbox:

  Logical parameter setting whether to run uncertainty analysis.

- runSensitivityAnalysisForAllParameters:

  Logical parameter setting whether to run sensitivity analysis for all
  constant parameters in the model.

- sensitivityThreshold:

  Minimum sensitivity threshold needed for a parameter sensitivity to be
  included in results.

- variationRangeNumericInput:

  Range over which the sensitivity is to be assessed.

- numberOfStepsNumericInput:

  Number of steps within the variation range at which to assess
  sensitivity.

- quantilesTestInput:

  Quantiles at which to assess percentiles of PK parameters in the
  uncertainty analysis.

- numberOfUncertaintySamples:

  Number of points at which to simulate the model in the uncertainty
  analysis

- runParallel:

  Logical parameter setting whether to run the local sensitivity and
  uncertainty analyses in parallel.
