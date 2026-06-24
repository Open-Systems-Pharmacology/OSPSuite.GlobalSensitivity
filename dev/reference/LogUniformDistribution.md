# LogUniformDistribution

R6 class, child class of `SADistribution`, defining a statistical
loguniform distribution object

## Super class

[`ospsuite.globalsensitivity::SADistribution`](https://www.open-systems-pharmacology.org/OSPSuite.GlobalSensitivity/dev/reference/SADistribution.md)
-\> `LogUniformDistribution`

## Active bindings

- `type`:

  is the type of the probability distribution.

- `minimum`:

  The maximum of the loguniform distribution.

- `maximum`:

  The maximum of the loguniform distribution.

## Methods

### Public methods

- [`LogUniformDistribution$new()`](#method-LogUniformDistribution-new)

- [`LogUniformDistribution$quantilesToSample()`](#method-LogUniformDistribution-quantilesToSample)

- [`LogUniformDistribution$clone()`](#method-LogUniformDistribution-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new `LogUniformDistribution` object.

#### Usage

    LogUniformDistribution$new(minimum, maximum)

#### Arguments

- `minimum`:

  The minimum of the loguniform distribution.

- `maximum`:

  The maximum of the loguniform distribution.

#### Returns

An instance of the `LogUniformDistribution` class.

------------------------------------------------------------------------

### Method `quantilesToSample()`

Maps a vector of `quantiles` to corresponding values from the loguniform
distribution.

#### Usage

    LogUniformDistribution$quantilesToSample(quantiles)

#### Arguments

- `quantiles`:

  Vector of quantiles.

#### Returns

A vector of values from the loguniform distribution corresponding to the
input `quantiles`.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    LogUniformDistribution$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
