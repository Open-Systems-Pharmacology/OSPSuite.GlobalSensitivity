#' @title SADistribution
#' @description R6 class defining a statistical distribution object
#' @keywords internal
SADistribution <- R6::R6Class(classname = "SADistribution")

#' @title UniformDistribution
#' @description R6 class, child class of `SADistribution`, defining a statistical uniform distribution object
#' @export
UniformDistribution <- R6::R6Class(
  classname = "UniformDistribution",
  inherit = SADistribution,
  public = list(
    #' @description Create a new `UniformDistribution` object.
    #' @param minimum The minimum of the uniform distribution.
    #' @param maximum The maximum of the uniform distribution.
    #' @return An instance of the `UniformDistribution` class.
    initialize = function(minimum,
                          maximum) {
      self$minimum <- minimum
      self$maximum <- maximum
    },
    #' @description Maps a vector of `quantiles` to corresponding values from the uniform distribution.
    #' @param quantiles Vector of quantiles.
    #' @return A vector of values from the uniform distribution corresponding to the input `quantiles`.
    quantilesToSample = function(quantiles) {
      getUniformSampleVector(
        quantileVec = quantiles,
        minimum = self$minimum,
        maximum = self$maximum
      ) %>% return()
    }
  ),
  private = list(
    .type = "Uniform",
    .minimum = NULL,
    .maximum = NULL
  ),
  active = list(
    #' @field type is the type of the probability distribution.
    type = function(value) {
      if (missing(value)) {
        return(private$.type)
      }
    },
    #' @field minimum The minimum of the uniform distribution.
    minimum = function(value) {
      if (missing(value)) {
        return(private$.minimum)
      }
      error(
        condition = !is.numeric(value),
        errorMessage = "Parameter 'minimum' must be of type 'numeric'."
      )
      private$.minimum <- value
    },
    #' @field maximum The maximum of the uniform distribution.
    maximum = function(value) {
      if (missing(value)) {
        return(private$.maximum)
      }
      error(
        condition = !is.numeric(value),
        errorMessage = "Parameter 'maximum' must be of type 'numeric'."
      )
      private$.maximum <- value
    }
  )
)

#' @title LogUniformDistribution
#' @description R6 class, child class of `SADistribution`, defining a statistical loguniform distribution object
#' @export
LogUniformDistribution <- R6::R6Class(
  classname = "LogUniformDistribution",
  inherit = SADistribution,
  public = list(
    #' @description Create a new `LogUniformDistribution` object.
    #' @param minimum The minimum of the loguniform distribution.
    #' @param maximum The maximum of the loguniform distribution.
    #' @return An instance of the `LogUniformDistribution` class.
    initialize = function(minimum,
                          maximum) {
      self$minimum <- minimum
      self$maximum <- maximum
    },
    #' @description Maps a vector of `quantiles` to corresponding values from the loguniform distribution.
    #' @param quantiles Vector of quantiles.
    #' @return A vector of values from the loguniform distribution corresponding to the input `quantiles`.
    quantilesToSample = function(quantiles) {
      getLogUniformSampleVector(
        quantileVec = quantiles,
        minimum = self$minimum,
        maximum = self$maximum
      ) %>% return()
    }
  ),
  private = list(
    .type = "LogUniform",
    .minimum = NULL,
    .maximum = NULL
  ),
  active = list(
    #' @field type is the type of the probability distribution.
    type = function(value) {
      if (missing(value)) {
        return(private$.type)
      }
    },
    #' @field minimum The maximum of the loguniform distribution.
    minimum = function(value) {
      if (missing(value)) {
        return(private$.minimum)
      }
      error(
        condition = !is.numeric(value),
        errorMessage = "Parameter 'minimum' must be of type 'numeric'."
      )
      private$.minimum <- value
    },
    #' @field maximum The maximum of the loguniform distribution.
    maximum = function(value) {
      if (missing(value)) {
        return(private$.maximum)
      }
      error(
        condition = !is.numeric(value),
        errorMessage = "Parameter 'maximum' must be of type 'numeric'."
      )
      private$.maximum <- value
    }
  )
)


#' @title NormalDistribution
#' @description R6 class, child class of `SADistribution`, defining a statistical normal distribution object
#' @export
NormalDistribution <- R6::R6Class(
  classname = "NormalDistribution",
  inherit = SADistribution,
  public = list(
    #' @description Create a new `NormalDistribution` object.
    #' @param mean The mean of the normal distribution.
    #' @param stdv The standard deviation of the normal distribution.
    #' @return An instance of the `NormalDistribution` class.
    initialize = function(mean,
                          stdv) {
      self$mean <- mean
      self$stdv <- stdv
    },
    #' @description Maps a vector of `quantiles` to corresponding values from the normal distribution.
    #' @param quantiles Vector of quantiles.
    #' @return A vector of values from the normal distribution corresponding to the input `quantiles`.
    quantilesToSample = function(quantiles) {
      getNormalSampleVector(
        quantileVec = quantiles,
        mean = self$mean,
        stdv = self$stdv
      ) %>% return()
    }
  ),
  private = list(
    .type = "Normal",
    .mean = NULL,
    .stdv = NULL
  ),
  active = list(
    #' @field type is the type of the probability distribution.
    type = function(value) {
      if (missing(value)) {
        return(private$.type)
      }
    },
    #' @field mean The mean of the normal distribution.
    mean = function(value) {
      if (missing(value)) {
        return(private$.mean)
      }
      error(
        condition = !is.numeric(value),
        errorMessage = "Parameter 'mean' must be of type 'numeric'."
      )
      private$.mean <- value
    },
    #' @field stdv The standard deviation of the normal distribution.
    stdv = function(value) {
      if (missing(value)) {
        return(private$.stdv)
      }
      error(
        condition = !is.numeric(value),
        errorMessage = "Parameter 'stdv' must be of type 'numeric'."
      )
      error(
        condition = (value < 0),
        errorMessage = "Parameter 'stdv' must be positive."
      )
      private$.stdv <- value
    }
  )
)


#' @title LogNormalDistribution
#' @description R6 class, child class of `SADistribution`, defining a statistical lognormal distribution object
#' @export
LogNormalDistribution <- R6::R6Class(
  classname = "LogNormalDistribution",
  inherit = SADistribution,
  public = list(
    #' @description Create a new `LogNormalDistribution` object.
    #' @param mean The mean of the lognormal distribution.
    #' @param CV The coefficient of variation of the lognormal distribution.
    #' @return An instance of the `LogNormalDistribution` class.
    initialize = function(mean,
                          CV) {
      self$mean <- mean
      self$CV <- CV
    },
    #' @description Maps a vector of `quantiles` to corresponding values from the lognormal distribution.
    #' @param quantiles Vector of quantiles.
    #' @return A vector of values from the lognormal distribution corresponding to the input `quantiles`.
    quantilesToSample = function(quantiles) {
      getLogNormalSampleVector(
        quantileVec = quantiles,
        mean = self$mean,
        CV = self$CV
      ) %>% return()
    }
  ),
  private = list(
    .type = "LogNormal",
    .mean = NULL,
    .CV = NULL
  ),
  active = list(
    #' @field type is the type of the probability distribution.
    type = function(value) {
      if (missing(value)) {
        return(private$.type)
      }
    },
    #' @field mean The mean of the lognormal distribution.
    mean = function(value) {
      if (missing(value)) {
        return(private$.mean)
      }
      error(
        condition = !is.numeric(value),
        errorMessage = "Parameter 'mean' must be of type 'numeric'."
      )
      private$.mean <- value
    },
    #' @field CV The coefficient of variation of the lognormal distribution.
    CV = function(value) {
      if (missing(value)) {
        return(private$.CV)
      }
      error(
        condition = !is.numeric(value),
        errorMessage = "Parameter 'CV' must be of type 'numeric'."
      )
      error(
        condition = (value < 0),
        errorMessage = "Parameter 'CV' must be non-negative"
      )
      private$.CV <- value
    }
  )
)
