# getSampleRate

Select a sampling rate within the parameter space as per Saltelli,
Tarantola & Chan, 1999.

## Usage

``` r
getSampleRate(parameterFrequencies)
```

## Arguments

- parameterFrequencies:

  A vector of the frequencies associated with each parameter.

## Value

A maximal rate of sampling that satisfies Nyquist-Shannon with respect
to the `parameterFrequencies`.
