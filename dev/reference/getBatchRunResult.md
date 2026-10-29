# getBatchRunResult

Look up the result of one set of run values in the output of
[`ospsuite::runSimulationBatches`](https://www.open-systems-pharmacology.org/OSPSuite-R/reference/runSimulationBatches.html).
Results are looked up by run values id rather than position, so results
of any other run values that were still queued on the batch (e.g. left
over from a run that threw) can never be attributed to these run values.

## Usage

``` r
getBatchRunResult(batchResults, batchIndex, runValuesId)
```

## Arguments

- batchResults:

  Output of
  [`ospsuite::runSimulationBatches`](https://www.open-systems-pharmacology.org/OSPSuite-R/reference/runSimulationBatches.html),
  or `NULL` if the run failed.

- batchIndex:

  Position of the batch in the list passed to
  [`ospsuite::runSimulationBatches`](https://www.open-systems-pharmacology.org/OSPSuite-R/reference/runSimulationBatches.html).

- runValuesId:

  Id returned by `SimulationBatch$addRunValues` for the run values.

## Value

The `SimulationResults` for `runValuesId`, or `NULL` if the run failed
or no result exists for it.
