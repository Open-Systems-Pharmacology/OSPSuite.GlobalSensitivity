# runFFT2

Run the Fast Fourier Transform and the sensitivity indices.

## Usage

``` r
runFFT2(
  outputs,
  pkEvaluationsList,
  parameters,
  fftStructure,
  allFrequencies,
  parameterFrequencies,
  addHarmonicsForParameterNumber
)
```

## Arguments

- outputs:

  List of `SAOutput` objects.

- pkEvaluationsList:

  List of evaluated PK parameters for each output.

- parameters:

  List of `SAParameter` objects.

- fftStructure:

  An empty structured list to be updated with the first order and total
  effects resulting from the FFT evaluations.

- allFrequencies:

  A vector of all integer frequencies from 0 up to the sampling rate
  that was used in generating the hypercube over which the
  pkEvaluationsList was evaluated.

- parameterFrequencies:

  A vector of the frequencies associated with each parameter.

- addHarmonicsForParameterNumber:

  Parameter number for which first order index is to be calculated

## Value

First order and total effect indices of EFAST evaluation.
