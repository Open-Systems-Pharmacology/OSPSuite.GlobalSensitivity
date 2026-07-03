# SAParameter

R6 class defining a parameter object

## Active bindings

- `path`:

  is the path to the parameter within the `simulation` PKML simulation.

- `displayName`:

  is the display name for the parameter in lieu of the parameter path in
  the `simulation` PKML simulation.

- `dimension`:

  is the dimension of the parameter in the `simulation` PKML simulation.

- `unit`:

  is the unit of the parameter in the `simulation` PKML simulation.

- `distribution`:

  is the distribution of the parameter

## Methods

### Public methods

- [`SAParameter$new()`](#method-SAParameter-new)

- [`SAParameter$clone()`](#method-SAParameter-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new `SAParameter` object.

#### Usage

    SAParameter$new(
      simulation,
      path,
      displayName = NULL,
      unit = NULL,
      parameterDistribution = NULL,
      defaultVariationRangeForLogUniformDistributions = 0.1
    )

#### Arguments

- `simulation`:

  simulation A PKML simulation in which the parameter exists.

- `path`:

  A PKML simulation in which the parameter exists.

- `displayName`:

  A shorthand string for the parameter that substitutes for the path for
  display purposes.

- `unit`:

  A valid OSP unit used to interpret the numerical values that are input
  into the `parameterDistribution` object, such as the mean and standard
  deviation of a normal distribution.

- `parameterDistribution`:

  A `SADistribution` object specifying the probability distribution of
  the parameter.

- `defaultVariationRangeForLogUniformDistributions`:

  When no `parameterDistribution` is specified, a loguniform
  distribution is assumed with multiplicative variation range given by
  `defaultVariationRangeForLogUniformDistributions`.

#### Returns

An instance of the `SAParameter` class.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    SAParameter$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
