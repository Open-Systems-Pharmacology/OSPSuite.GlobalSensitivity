# NormalDistribution

R6 class, child class of `SADistribution`, defining a statistical normal
distribution object

## Super class

[`ospsuite.globalsensitivity::SADistribution`](https://www.open-systems-pharmacology.org/OSPSuite.GlobalSensitivity/dev/reference/SADistribution.md)
-\> `NormalDistribution`

## Active bindings

- `type`:

  is the type of the probability distribution.

- `mean`:

  The mean of the normal distribution.

- `stdv`:

  The standard deviation of the normal distribution.

## Methods

### Public methods

- [`NormalDistribution$new()`](#method-NormalDistribution-new)

- [`NormalDistribution$quantilesToSample()`](#method-NormalDistribution-quantilesToSample)

- [`NormalDistribution$clone()`](#method-NormalDistribution-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new `NormalDistribution` object.

#### Usage

    NormalDistribution$new(mean, stdv)

#### Arguments

- `mean`:

  The mean of the normal distribution.

- `stdv`:

  The standard deviation of the normal distribution.

#### Returns

An instance of the `NormalDistribution` class.

------------------------------------------------------------------------

### Method `quantilesToSample()`

Maps a vector of `quantiles` to corresponding values from the normal
distribution.

#### Usage

    NormalDistribution$quantilesToSample(quantiles)

#### Arguments

- `quantiles`:

  Vector of quantiles.

#### Returns

A vector of values from the normal distribution corresponding to the
input `quantiles`.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    NormalDistribution$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
