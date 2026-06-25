#' @title SAOutput
#' @description R6 class defining a model output object
#' @export
SAOutput <- R6::R6Class(
  classname = "SAOutput",
  public = list(
    #' @description Create a new `SAOutput` object.
    #' @param simulation simulation A PKML simulation in which the parameter exists.
    #' @param path A PKML simulation in which the output exists.
    #' @param displayName A shorthand string for the output that substitutes for the path for display purposes.
    #' @param unit A valid OSP unit that will be the display unit for this instance of the `SAOutput` object.
    #' @return An instance of the `SAOutput` class.
    initialize = function(simulation,
                          path,
                          displayName = NULL,
                          unit = NULL) {
      self$path <- path
      self$displayName <- displayName %||% path
      ospQuantity <- ospsuite::getQuantity(
        path = self$path,
        container = simulation
      )

      self$dimension <- ospQuantity$dimension

      self$unit <- unit %||% ospsuite::getBaseUnit(self$dimension)

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
    },
    #' @description Add PK parameter to be evaluated for the `SAOutput` instance.
    #' @param standardPKParameter A standard PK parameter selected from among the list of strings in `ospsuite::allPKParameterNames()`.
    #' @param pkParameterDisplayName A shorthand string for the parameter that substitutes for the path for display purposes.
    #' @param startTime Start time within the simulation run for calculation of the PK parameter.
    #' @param endTime End time within the simulation run for calculation of the PK parameter.
    addPKParameter = function(standardPKParameter,
                              pkParameterDisplayName = NULL,
                              startTime = NULL,
                              endTime = NULL) {
      pkParameterDisplayName <- pkParameterDisplayName %||% paste0(
        standardPKParameter,
        ifelse(startTime, paste0("_", startTime), NULL),
        ifelse(endTime, paste0("_", endTime), NULL)
      )

      addNewPkParameter(
        pkParameterName = pkParameterDisplayName,
        standardPKParameter = standardPKParameter,
        startTime = startTime,
        endTime = endTime
      )

      private$.pkParameterList <- c(private$.pkParameterList, pkParameterDisplayName)
    }
  ),
  private = list(
    .path = NULL,
    .displayName = NULL,
    .pkParameterList = list(),
    .dimension = NULL,
    .unit = NULL
  ),
  active = list(
    #' @field path is the path to the output within the `simulation` PKML simulation.
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
    #' @field displayName is the display name for the output in lieu of the output path in the `simulation` PKML simulation.
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
    #' @field pkParameterList The list of PK parameters for this instance of the `SAOutput`.
    pkParameterList = function(value) {
      if (missing(value)) {
        return(private$.pkParameterList %>% unlist())
      }
    },
    #' @field dimension The dimension of this instance of the `SAOutput`.
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
    #' @field unit The display unit of this instance of the `SAOutput`.
    unit = function(value) {
      if (missing(value)) {
        return(private$.unit)
      }
      error(
        condition = !is.character(value),
        errorMessage = "Parameter 'unit' must be of type 'character'."
      )
      private$.unit <- value
    }
  )
)


#' @title addNewPkParameter
#' @description Add a new PK parameter to the managed list of OSP PK parameters.
#' @param pkParameterName The name of the PK parameter to be added.
#' @param standardPKParameter The standard PK parameter on which the PK parameter to be added is based, selected from among the list of strings in `ospsuite::allPKParameterNames()`.
#' @param startTime Start time within the simulation run for calculation of the PK parameter.
#' @param endTime End time within the simulation run for calculation of the PK parameter.
#' @keywords internal
addNewPkParameter <- function(pkParameterName, standardPKParameter, startTime, endTime) {
  if (pkParameterName %in% ospsuite::allPKParameterNames()) {
    return()
  }

  newPKParameter <- ospsuite::addUserDefinedPKParameter(
    name = pkParameterName,
    standardPKParameter = ospsuite::StandardPKParameter[[standardPKParameter]],
    displayName = pkParameterName
  )

  if (!is.null(startTime)) {
    newPKParameter$startTime <- startTime
  }

  if (!is.null(endTime)) {
    newPKParameter$endTime <- endTime
  }

  return()
}
