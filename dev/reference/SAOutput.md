# SAOutput

R6 class defining a model output object

## Active bindings

- `path`:

  is the path to the output within the `simulation` PKML simulation.

- `displayName`:

  is the display name for the output in lieu of the output path in the
  `simulation` PKML simulation.

- `pkParameterList`:

  The list of PK parameters for this instance of the `SAOutput`.

- `dimension`:

  The dimension of this instance of the `SAOutput`.

- `unit`:

  The display unit of this instance of the `SAOutput`.

## Methods

### Public methods

- [`SAOutput$new()`](#method-SAOutput-new)

- [`SAOutput$addPKParameter()`](#method-SAOutput-addPKParameter)

- [`SAOutput$clone()`](#method-SAOutput-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new `SAOutput` object.

#### Usage

    SAOutput$new(simulation, path, displayName = NULL, unit = NULL)

#### Arguments

- `simulation`:

  simulation A PKML simulation in which the parameter exists.

- `path`:

  A PKML simulation in which the output exists.

- `displayName`:

  A shorthand string for the output that substitutes for the path for
  display purposes.

- `unit`:

  A valid OSP unit that will be the display unit for this instance of
  the `SAOutput` object.

#### Returns

An instance of the `SAOutput` class.

------------------------------------------------------------------------

### Method `addPKParameter()`

Add PK parameter to be evaluated for the `SAOutput` instance.

#### Usage

    SAOutput$addPKParameter(
      standardPKParameter,
      pkParameterDisplayName = NULL,
      startTime = NULL,
      endTime = NULL
    )

#### Arguments

- `standardPKParameter`:

  A standard PK parameter selected from among the list of strings in
  [`ospsuite::allPKParameterNames()`](https://www.open-systems-pharmacology.org/OSPSuite-R/reference/allPKParameterNames.html).

- `pkParameterDisplayName`:

  A shorthand string for the parameter that substitutes for the path for
  display purposes.

- `startTime`:

  Start time within the simulation run for calculation of the PK
  parameter.

- `endTime`:

  End time within the simulation run for calculation of the PK
  parameter.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    SAOutput$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
