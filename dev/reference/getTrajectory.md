# getTrajectory

Function to construct a trajectory for model evaluations through
parameter space as per Morris, 'Factorial Sampling Plans for Preliminary
Computational Experiments', 1992.

## Usage

``` r
getTrajectory(
  numberOfParameters,
  numberOfGridPartitions = defaultNumberOfGridPartitions
)
```

## Arguments

- numberOfParameters:

  The number of parameter paths.

- numberOfGridPartitions:

  The number of grid partitions, equivalent to parameter `p` in Morris,
  1991.

## Value

Trajectory for model evaluations through parameter space as per Morris,
1991.
