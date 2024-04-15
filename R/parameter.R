#' @export
SAParameter <- R6::R6Class(classname = "SAParameter",
                           public = list(
                             initialize = function(simulation,
                                                   path,
                                                   displayName = NULL,
                                                   unit = NULL,
                                                   defaultVariationRangeForLogUniformDistributions = 0.1,
                                                   parameterDistribution = NULL){
                               self$path <- path
                               self$displayName <- displayName %||% path
                               ospParameter <- ospsuite::getParameter(path = self$path,
                                                                      container = simulation)

                               error(condition = is.nan(ospParameter$value),
                                     errorMessage = paste0("Value of parameter with path ",self$path," is invalid (NaN)."))

                               self$dimension <- ospParameter$dimension

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
                               nominalParameterValue <- ospsuite::toUnit(quantityOrDimension = self$dimension,
                                                                         values = ospParameter$value,
                                                                         targetUnit = self$unit)
                               boundaryValues <- c(nominalParameterValue/(1+defaultVariationRangeForLogUniformDistributions),nominalParameterValue*(1 + defaultVariationRangeForLogUniformDistributions))
                               self$distribution <- parameterDistribution %||% distribution$LogUniform(minimum = min(boundaryValues),
                                                                                                       maximum = max(boundaryValues))
                             }
                           ),
                           private = list(
                             .path = NULL,
                             .displayName = NULL,
                             .dimension=NULL,
                             .unit = NULL,
                             .distribution = NULL
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
                             },

                             distribution = function(value){
                               if(missing(value)){
                                 return(private$.distribution)
                               }
                               error(condition = !("SADistribution"  %in% class(value) ),
                                     errorMessage = "Parameter 'distribution' must be of class type 'SADistribution'.")
                               private$.distribution <- value
                             }

                           )
)
