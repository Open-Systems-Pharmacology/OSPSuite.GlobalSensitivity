# getSamplingHypercube

Build a hypercube of points at which the model will be simulated.

## Usage

``` r
getSamplingHypercube(parameterFrequencies, samplingRate)
```

## Arguments

- parameterFrequencies:

  A vector of the frequencies associated with each parameter.

- samplingRate:

  Maximal rate of sampling that satisfies Nyquist-Shannon with respect
  to the `parameterFrequencies`.

## Value

A hypercube of points in \[0, 2\*pi\].
