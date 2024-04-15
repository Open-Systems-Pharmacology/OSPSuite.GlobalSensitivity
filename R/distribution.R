#' @export
SADistribution <- R6::R6Class(classname = "SADistribution")

#' @export
UniformDistribution <- R6::R6Class(classname = "UniformDistribution",
                                   inherit = SADistribution,
                                   public = list(
                                     initialize= function(minimum,
                                                          maximum){
                                       self$minimum <- minimum
                                       self$maximum <- maximum
                                     },
                                     quantilesToSample = function(quantiles){
                                       getUniformSampleVector(quantileVec = quantiles,
                                                              minimum = self$minimum,
                                                              maximum = self$maximum) %>% return()
                                     }
                                   ),
                                   private = list(
                                     .type = "Uniform",
                                     .minimum = NULL,
                                     .maximum = NULL
                                   ),
                                   active = list(
                                     type = function(value){
                                       if(missing(value)){
                                         return(private$.type)
                                       }
                                     },
                                     minimum = function(value){
                                       if(missing(value)){
                                         return(private$.minimum)
                                       }
                                       error(condition = !is.numeric(value),
                                             errorMessage = "Parameter 'minimum' must be of type 'numeric'.")
                                       private$.minimum <- value
                                     },
                                     maximum = function(value){
                                       if(missing(value)){
                                         return(private$.maximum)
                                       }
                                       error(condition = !is.numeric(value),
                                             errorMessage = "Parameter 'maximum' must be of type 'numeric'.")
                                       private$.maximum <- value
                                     }
                                   )
)

#' @export
LogUniformDistribution <- R6::R6Class(classname = "LogUniformDistribution",
                                      inherit = SADistribution,
                                      public = list(
                                        initialize= function(minimum,
                                                             maximum){
                                          self$minimum <- minimum
                                          self$maximum <- maximum
                                        },
                                        quantilesToSample = function(quantiles){
                                          getLogUniformSampleVector(quantileVec = quantiles,
                                                                    minimum = self$minimum,
                                                                    maximum = self$maximum) %>% return()
                                        }
                                      ),
                                      private = list(
                                        .type = "LogUniform",
                                        .minimum = NULL,
                                        .maximum = NULL
                                      ),
                                      active = list(
                                        type = function(value){
                                          if(missing(value)){
                                            return(private$.type)
                                          }
                                        },
                                        minimum = function(value){
                                          if(missing(value)){
                                            return(private$.minimum)
                                          }
                                          error(condition = !is.numeric(value),
                                                errorMessage = "Parameter 'minimum' must be of type 'numeric'.")
                                          private$.minimum <- value
                                        },
                                        maximum = function(value){
                                          if(missing(value)){
                                            return(private$.maximum)
                                          }
                                          error(condition = !is.numeric(value),
                                                errorMessage = "Parameter 'maximum' must be of type 'numeric'.")
                                          private$.maximum <- value
                                        }
                                      )
)


#' @export
NormalDistribution <- R6::R6Class(classname = "NormalDistribution",
                                  inherit = SADistribution,
                                  public = list(
                                    initialize= function(mean,
                                                         stdv){
                                      self$mean <- mean
                                      self$stdv <- stdv
                                    },
                                    quantilesToSample = function(quantiles){
                                      getNormalSampleVector(quantileVec = quantiles,
                                                            mean = self$mean,
                                                            stdv = self$stdv) %>% return()
                                    }
                                  ),
                                  private = list(
                                    .type = "Normal",
                                    .mean = NULL,
                                    .stdv = NULL
                                  ),
                                  active = list(
                                    type = function(value){
                                      if(missing(value)){
                                        return(private$.type)
                                      }
                                    },
                                    mean = function(value){
                                      if(missing(value)){
                                        return(private$.mean)
                                      }
                                      error(condition = !is.numeric(value),
                                            errorMessage = "Parameter 'mean' must be of type 'numeric'.")
                                      private$.mean <- value
                                    },
                                    stdv = function(value){
                                      if(missing(value)){
                                        return(private$.stdv)
                                      }
                                      error(condition = !is.numeric(value),
                                            errorMessage = "Parameter 'stdv' must be of type 'numeric'.")
                                      error(condition = (value < 0),
                                            errorMessage = "Parameter 'stdv' must be positive.")
                                      private$.stdv <- value
                                    }
                                  )
)


#' @export
LogNormalDistribution <- R6::R6Class(classname = "LogNormalDistribution",
                                     inherit = SADistribution,
                                     public = list(
                                       initialize= function(mean,
                                                            CV){
                                         self$mean <- mean
                                         self$CV <- CV
                                       },
                                       quantilesToSample = function(quantiles){
                                         getLogNormalSampleVector(quantileVec = quantiles,
                                                                  mean = self$mean,
                                                                  CV = self$CV) %>% return()
                                       }
                                     ),
                                     private = list(
                                       .type = "LogNormal",
                                       .mean = NULL,
                                       .CV = NULL
                                     ),
                                     active = list(
                                       type = function(value){
                                         if(missing(value)){
                                           return(private$.type)
                                         }
                                       },
                                       mean = function(value){
                                         if(missing(value)){
                                           return(private$.mean)
                                         }
                                         error(condition = !is.numeric(value),
                                               errorMessage = "Parameter 'mean' must be of type 'numeric'.")
                                         private$.mean <- value
                                       },
                                       CV = function(value){
                                         if(missing(value)){
                                           return(private$.CV)
                                         }
                                         error(condition = !is.numeric(value),
                                               errorMessage = "Parameter 'CV' must be of type 'numeric'.")
                                         error(condition = (value < 0),
                                               errorMessage = "Parameter 'CV' must be non-negative")
                                         private$.CV <- value
                                       }
                                     )
)
