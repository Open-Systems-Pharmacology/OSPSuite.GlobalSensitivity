#' @export
SAOutput <- R6::R6Class(classname = "SAOutput",
                        public = list(
                          initialize = function(simulation,
                                                path,
                                                displayName = NULL,
                                                unit = NULL){
                            self$path <- path
                            self$displayName <- displayName %||% path
                            ospQuantity <- ospsuite::getQuantity(path = self$path,
                                                                 container = simulation)

                            self$dimension <- ospQuantity$dimension

                            self$unit <- unit %||% ospsuite::getBaseUnit(dimension = self$dimension)

                            if(self$unit == "Unitless"){
                              self$unit <- ""
                            }

                            unitsForDimension <- ospsuite::getUnitsForDimension(self$dimension)
                            error(condition = !(self$unit %in% unitsForDimension),
                                  errorMessage = paste0("Units for parameter ",
                                                        path,
                                                        " must be one of: ",
                                                        paste0(unitsForDimension, collapse = ", " ))
                            )


                          },

                          addPKParameter = function(standardPKParameter,
                                                    pkParameterDisplayName = NULL,
                                                    startTime = NULL,
                                                    endTime = NULL){

                            pkParameterDisplayName <- pkParameterDisplayName %||% paste0(standardPKParameter,
                                                                                         ifelse(startTime,paste0("_",startTime),NULL),
                                                                                         ifelse(endTime,paste0("_",endTime),NULL))

                            addNewPkParameter(pkParameterName = pkParameterDisplayName,
                                              standardPKParameter =  standardPKParameter,
                                              startTime = startTime,
                                              endTime = endTime)

                            private$.pkParameterList <- c(private$.pkParameterList,pkParameterDisplayName)
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
                          path = function(value){
                            if(missing(value)){
                              return(private$.path)
                            }
                            error(condition = !is.character(value),
                                  errorMessage = "Parameter 'path' must be of type 'character'.")
                            private$.path <- value
                          },

                          displayName = function(value){
                            if(missing(value)){
                              return(private$.displayName)
                            }
                            error(condition = !is.character(value),
                                  errorMessage = "Parameter 'displayName' must be of type 'character'.")
                            private$.displayName <- value
                          },

                          pkParameterList = function(value){
                            if(missing(value)){
                              return(private$.pkParameterList %>% unlist())
                            }
                          },

                          dimension = function(value){
                            if(missing(value)){
                              return(private$.dimension)
                            }
                            error(condition = !is.character(value),
                                  errorMessage = "Parameter 'dimension' must be of type 'character'.")
                            private$.dimension <- value
                          },

                          unit = function(value){
                            if(missing(value)){
                              return(private$.unit)
                            }
                            error(condition = !is.character(value),
                                  errorMessage = "Parameter 'unit' must be of type 'character'.")
                            private$.unit <- value
                          }

                        )
)




addNewPkParameter <- function(pkParameterName, standardPKParameter, startTime, endTime) {

  if (pkParameterName %in% ospsuite::allPKParameterNames()) {
    return()
  }

  newPKParameter <- ospsuite::addUserDefinedPKParameter(
    name = pkParameterName,
    standardPKParameter = StandardPKParameter[[standardPKParameter]],
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
