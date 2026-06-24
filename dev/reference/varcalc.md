# varcalc

Function to calculate Sobol indices from simulated model evaluations.

## Usage

``` r
varcalc(fU_list, outputList)
```

## Arguments

- fU_list:

  A list of model evaluations for different Sobol sequence matrices.
  Evaluations correspond to each output/PK parameter combination.

- outputList:

  A vector of strings of parameter names.

## Value

First order and total effect Sobol indices for each parameter
path/output path/PK parameter combination.
