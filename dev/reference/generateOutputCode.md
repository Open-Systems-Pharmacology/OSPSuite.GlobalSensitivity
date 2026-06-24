# generateOutputCode

Generate code for setting up a SAOutput R6 object

## Usage

``` r
generateOutputCode(path, displayName, pkParameterList)
```

## Arguments

- path:

  The parameter path in the simulation.

- displayName:

  A display name for the parameter in lieu of the parameter path.

- pkParameterList:

  A list of PK parameter names as strings, selected from among
  [`ospsuite::allPKParameterNames()`](https://www.open-systems-pharmacology.org/OSPSuite-R/reference/allPKParameterNames.html)
