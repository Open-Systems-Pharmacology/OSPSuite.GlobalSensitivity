#' @title SAParameter
#' @description R6 class defining a parameter object
#' @export
SAParameter <- R6::R6Class(
  classname = "SAParameter",
  public = list(
    #' @description Create a new `SAParameter` object.
    #' @param simulation simulation A PKML simulation in which the parameter exists.
    #' @param path A PKML simulation in which the parameter exists.
    #' @param displayName A shorthand string for the parameter that substitutes for the path for display purposes.
    #' @param unit A valid OSP unit used to interpret the numerical values that are input into the `parameterDistribution` object, such as the mean and standard deviation of a normal distribution.
    #' @param parameterDistribution A `SADistribution` object specifying the probability distribution of the parameter.
    #' @param defaultVariationRangeForLogUniformDistributions When no `parameterDistribution` is specified, a loguniform distribution is assumed with multiplicative variation range given by `defaultVariationRangeForLogUniformDistributions`.
    #' @return An instance of the `SAParameter` class.
    initialize = function(simulation,
                          path,
                          displayName = NULL,
                          unit = NULL,
                          parameterDistribution = NULL,
                          defaultVariationRangeForLogUniformDistributions = 0.1) {
      self$path <- path
      self$displayName <- displayName %||% path
      ospParameter <- ospsuite::getParameter(
        path = self$path,
        container = simulation
      )

      error(
        condition = is.nan(ospParameter$value),
        errorMessage = paste0("Value of parameter with path ", self$path, " is invalid (NaN).")
      )

      self$dimension <- ospParameter$dimension

      self$unit <- unit %||% ospsuite::getBaseUnit(dimension = self$dimension)

      if (self$unit == "Unitless") {
        self$unit <- ""
      }

      unitsForDimension <- ospsuite::getUnitsForDimension(self$dimension)
      error(
        condition = !(self$unit %in% unitsForDimension),
        errorMessage = paste0(
          "Units for parameter ",
          path,
          " must be one of: ",
          paste0(unitsForDimension, collapse = ", ")
        )
      )
      nominalParameterValue <- ospsuite::toUnit(
        quantityOrDimension = self$dimension,
        values = ospParameter$value,
        targetUnit = self$unit
      )
      boundaryValues <- c(nominalParameterValue / (1 + defaultVariationRangeForLogUniformDistributions), nominalParameterValue * (1 + defaultVariationRangeForLogUniformDistributions))
      self$distribution <- parameterDistribution %||% distribution$LogUniform(
        minimum = min(boundaryValues),
        maximum = max(boundaryValues)
      )
    }
  ),
  private = list(
    .path = NULL,
    .displayName = NULL,
    .dimension = NULL,
    .unit = NULL,
    .distribution = NULL
  ),
  active = list(
    #' @field path is the path to the parameter within the `simulation` PKML simulation.
    path = function(value) {
      if (missing(value)) {
        return(private$.path)
      }
      error(
        condition = !is.character(value),
        errorMessage = "Parameter 'path' must be of type 'character'."
      )
      private$.path <- value
    },
    #' @field displayName is the display name for the parameter in lieu of the parameter path in the `simulation` PKML simulation.
    displayName = function(value) {
      if (missing(value)) {
        return(private$.displayName)
      }
      error(
        condition = !is.character(value),
        errorMessage = "Parameter 'displayName' must be of type 'character'."
      )
      private$.displayName <- value
    },
    #' @field dimension is the dimension of the parameter in the `simulation` PKML simulation.
    dimension = function(value) {
      if (missing(value)) {
        return(private$.dimension)
      }
      error(
        condition = !is.character(value),
        errorMessage = "Parameter 'dimension' must be of type 'character'."
      )
      private$.dimension <- value
    },
    #' @field unit is the unit of the parameter in the `simulation` PKML simulation.
    unit = function(value) {
      if (missing(value)) {
        return(private$.unit)
      }
      error(
        condition = !is.character(value),
        errorMessage = "Parameter 'unit' must be of type 'character'."
      )
      private$.unit <- value
    },
    #' @field distribution is the distribution of the parameter
    distribution = function(value) {
      if (missing(value)) {
        return(private$.distribution)
      }
      error(
        condition = !("SADistribution" %in% class(value)),
        errorMessage = "Parameter 'distribution' must be of class type 'SADistribution'."
      )
      private$.distribution <- value
    }
  )
)
