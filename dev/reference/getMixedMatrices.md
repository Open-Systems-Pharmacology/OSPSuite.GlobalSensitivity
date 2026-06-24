# runSobol

Function to generate points in parameter space at which simulation will
be run in Sobol sensitivity analysis.

## Usage

``` r
getMixedMatrices(A, B, parameterPaths)
```

## Arguments

- A:

  A matrix consisting of a Sobol sequence with a number of columns equal
  to the number of parameter paths and a number of rows equal to the
  sample size.

- B:

  A matrix consisting of a Sobol sequence with a number of columns equal
  to the number of parameter paths and a number of rows equal to the
  sample size.

- parameterPaths:

  A vector of strings of parameter names.

## Value

A list of matrices, one for each parameter path in `parameterPaths`. The
i'th matrix consists of matrix `B` with its i'th column replaced with
the corresponding column from matrix `A`.
