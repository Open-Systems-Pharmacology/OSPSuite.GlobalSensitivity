# LogNormalDistribution

R6 class, child class of `SADistribution`, defining a statistical
lognormal distribution object

## Super class

[`ospsuite.globalsensitivity::SADistribution`](https://www.open-systems-pharmacology.org/OSPSuite.GlobalSensitivity/dev/reference/SADistribution.md)
-\> `LogNormalDistribution`

## Active bindings

- `type`:

  is the type of the probability distribution.

- `mean`:

  The mean of the lognormal distribution.

- `CV`:

  The coefficient of variation of the lognormal distribution.

## Methods

### Public methods

- [`LogNormalDistribution$new()`](#method-LogNormalDistribution-new)

- [`LogNormalDistribution$quantilesToSample()`](#method-LogNormalDistribution-quantilesToSample)

- [`LogNormalDistribution$clone()`](#method-LogNormalDistribution-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new `LogNormalDistribution` object.

#### Usage

    LogNormalDistribution$new(mean, CV)

#### Arguments

- `mean`:

  The mean of the lognormal distribution.

- `CV`:

  The coefficient of variation of the lognormal distribution.

#### Returns

An instance of the `LogNormalDistribution` class.

------------------------------------------------------------------------

### Method `quantilesToSample()`

Maps a vector of `quantiles` to corresponding values from the lognormal
distribution.

#### Usage

    LogNormalDistribution$quantilesToSample(quantiles)

#### Arguments

- `quantiles`:

  Vector of quantiles.

#### Returns

A vector of values from the lognormal distribution corresponding to the
input `quantiles`.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    LogNormalDistribution$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
