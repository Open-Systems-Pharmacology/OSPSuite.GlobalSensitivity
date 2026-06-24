# extractPKParametersFromBatchSimulationResults

Evaluate the PK parameters corresponding to each output in the
simulation results obtained from batch mode simulation.

## Usage

``` r
extractPKParametersFromBatchSimulationResults(
  batchSimulationResults,
  DDIbatchSimulationResults,
  outputs,
  pkEvaluationsList
)
```

## Arguments

- batchSimulationResults:

  for the main `simulation`

- DDIbatchSimulationResults:

  for the DDI `simulation`

- outputs:

  List of `SAOutput` objects.

- pkEvaluationsList:

  An empty structured list to be updated with the PK parameter
  evaluations.

## Value

Evaluation of PK parameters from batch simulation results.
