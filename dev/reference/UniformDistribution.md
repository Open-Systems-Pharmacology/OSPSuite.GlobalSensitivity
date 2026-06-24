# UniformDistribution

R6 class, child class of `SADistribution`, defining a statistical
uniform distribution object

## Super class

[`ospsuite.globalsensitivity::SADistribution`](https://www.open-systems-pharmacology.org/OSPSuite.GlobalSensitivity/dev/reference/SADistribution.md)
-\> `UniformDistribution`

## Active bindings

- `type`:

  is the type of the probability distribution.

- `minimum`:

  The minimum of the uniform distribution.

- `maximum`:

  The maximum of the uniform distribution.

## Methods

### Public methods

- [`UniformDistribution$new()`](#method-UniformDistribution-new)

- [`UniformDistribution$quantilesToSample()`](#method-UniformDistribution-quantilesToSample)

- [`UniformDistribution$clone()`](#method-UniformDistribution-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new `UniformDistribution` object.

#### Usage

    UniformDistribution$new(minimum, maximum)

#### Arguments

- `minimum`:

  The minimum of the uniform distribution.

- `maximum`:

  The maximum of the uniform distribution.

#### Returns

An instance of the `UniformDistribution` class.

------------------------------------------------------------------------

### Method `quantilesToSample()`

Maps a vector of `quantiles` to corresponding values from the uniform
distribution.

#### Usage

    UniformDistribution$quantilesToSample(quantiles)

#### Arguments

- `quantiles`:

  Vector of quantiles.

#### Returns

A vector of values from the uniform distribution corresponding to the
input `quantiles`.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    UniformDistribution$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
