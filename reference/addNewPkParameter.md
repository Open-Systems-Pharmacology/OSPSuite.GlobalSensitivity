# addNewPkParameter

Add a new PK parameter to the managed list of OSP PK parameters.

## Usage

``` r
addNewPkParameter(pkParameterName, standardPKParameter, startTime, endTime)
```

## Arguments

- pkParameterName:

  The name of the PK parameter to be added.

- standardPKParameter:

  The standard PK parameter on which the PK parameter to be added is
  based, selected from among the list of strings in
  [`ospsuite::allPKParameterNames()`](https://www.open-systems-pharmacology.org/OSPSuite-R/reference/allPKParameterNames.html).

- startTime:

  Start time within the simulation run for calculation of the PK
  parameter.

- endTime:

  End time within the simulation run for calculation of the PK
  parameter.
