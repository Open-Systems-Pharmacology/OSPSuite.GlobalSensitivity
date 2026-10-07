getUniformSampleVector <- function(quantileVec, minimum, maximum) {
  verifyQuantileVec(quantileVec)
  return(sapply(quantileVec, function(q) {
    ((1 - q) * minimum) + (q * maximum)
  }))
}

getLogUniformSampleVector <- function(quantileVec, minimum, maximum) {
  verifyQuantileVec(quantileVec)
  error(minimum > maximum, "Parameter 'minimum' for a logUniform distritubed parameter may not exceed parameter 'maximum'.")
  error(sign(minimum) != sign(maximum), "Parameters 'minimum' and 'maximum' for a logUniform distritubed parameter must be of the same sign.")

  if (minimum == 0 & maximum == 0) {
    return(rep(0, length(quantileVec)))
  }

  error(minimum == 0 | maximum == 0, "Parameters 'minimum' and 'maximum' for a logUniform distritubed parameter must be non-zero.")

  logMinimum <- 0
  logMaximum <- log(maximum / minimum)

  logSpaceQuantileVec <- sapply(quantileVec, function(q) {
    ((1 - q) * logMinimum) + (q * logMaximum)
  })
  logUniformQuantileVec <- minimum * exp(logSpaceQuantileVec)
  return(logUniformQuantileVec)
}

normalDistributionTruncationQuantile <- 0.025

getNormalSampleVector <- function(quantileVec, mean, stdv, truncate = TRUE) {
  verifyQuantileVec(quantileVec)
  if (truncate) {
    quantileVec <- (normalDistributionTruncationQuantile * (1 - quantileVec)) + ((1 - normalDistributionTruncationQuantile) * quantileVec)
  }
  return(qnorm(p = quantileVec, mean = mean, sd = stdv))
}

getLogNormalSampleVector <- function(quantileVec, mean, CV) {
  sigmaSquared <- log((CV^2) + 1)
  mu <- log(mean) - (sigmaSquared / 2)
  sigma <- sqrt(sigmaSquared)
  logSpaceQuantileVec <- getNormalSampleVector(quantileVec, mean = mu, stdv = sigma, truncate = TRUE)
  return(exp(logSpaceQuantileVec))
}

uniformQuantilesToSamples <- function(quantileVec, distributionParametersList) {
  minimum <- distributionParametersList$minimum
  maximum <- distributionParametersList$maximum
  return(getUniformSampleVector(quantileVec, minimum, maximum))
}

logUniformQuantilesToSamples <- function(quantileVec, distributionParametersList) {
  minimum <- distributionParametersList$minimum
  maximum <- distributionParametersList$maximum
  return(getLogUniformSampleVector(quantileVec, minimum, maximum))
}

normalQuantilesToSamples <- function(quantileVec, distributionParametersList) {
  mean <- distributionParametersList$mean
  stdv <- distributionParametersList$stdv
  return(getNormalSampleVector(quantileVec, mean, stdv))
}

logNormalQuantilesToSamples <- function(quantileVec, distributionParametersList) {
  mean <- distributionParametersList$mean
  CV <- distributionParametersList$CV
  return(getLogNormalSampleVector(quantileVec, mean, CV))
}

quantileTransformationFunctions <- list(
  "Uniform" = uniformQuantilesToSamples,
  "LogUniform" = logUniformQuantilesToSamples,
  "Normal" = normalQuantilesToSamples,
  "LogNormal" = logNormalQuantilesToSamples
)


uniformInputs <- function(minimum, maximum) {
  return(list(minimum = minimum, maximum = maximum))
}

logUniformInputs <- function(minimum, maximum) {
  return(list(minimum = minimum, maximum = maximum))
}

normalInputs <- function(mean, stdv) {
  return(list(mean = mean, stdv = stdv))
}

logNormalInputs <- function(mean, CV) {
  return(list(mean = mean, CV = CV))
}

#' @title distribution
#' @description A named list of distribution constructors used to create
#'   distribution objects for global sensitivity analyses. Each element is a
#'   constructor function returning a distribution object: `Uniform`,
#'   `LogUniform`, `Normal` and `LogNormal`.
#' @format A named `list` of distribution constructor functions.
#' @export
distribution <- list(
  "Uniform" = UniformDistribution$new,
  "LogUniform" = LogUniformDistribution$new,
  "Normal" = NormalDistribution$new,
  "LogNormal" = LogNormalDistribution$new
)

transformQuantiles <- function(quantileVec, selectedDistributionForPath, parameterListForSelectedDistribution) {
  transformedVec <- quantileTransformationFunctions[[selectedDistributionForPath]](quantileVec = quantileVec,
    distributionParametersList = parameterListForSelectedDistribution)
  return(transformedVec)
}

getSimulationBatches <- function(simulation, parameterPaths, numberParallelThreads) {
  sapply(1:numberParallelThreads, function(row) {
    ospsuite::createSimulationBatch(
      simulation = simulation,
      parametersOrPaths = parameterPaths
    )
  })
}

verifyQuantileVec <- function(quantileVec) {
  if (any(quantileVec < 0) | any(quantileVec > 1)) {
    stop("Quantile out of bounds")
  }
}

getEvaluationMatrixStructure <- function(outputList) {
  f <- list()
  for (pth in names(outputList)) {
    f[[pth]] <- list()
  }
  return(f)
}


distSelection <- function(path) {
  paste0("distributionSelection", path, collapse = "_")
}
distParameterForm <- function(path) {
  paste0("miniform", path, collapse = "_")
}
distValParameterDisplayName <- function(path) {
  paste0("parameterDisplayName_", path)
}
distValUniformMinimum <- function(path) {
  paste0("uniformMin_", path)
}
distValUniformMaximum <- function(path) {
  paste0("uniformMax_", path)
}
distValUniformUnit <- function(path) {
  paste0("uniformUnit_", path)
}
distValLogUniformMinimum <- function(path) {
  paste0("logUniformMin_", path)
}
distValLogUniformMaximum <- function(path) {
  paste0("logUniformMax_", path)
}
distValLogUniformUnit <- function(path) {
  paste0("logUniformUnit_", path)
}
distValNormalMean <- function(path) {
  paste0("normalMean_", path)
}
distValNormalStdv <- function(path) {
  paste0("normalStdv_", path)
}
distValNormalUnit <- function(path) {
  paste0("normalUnit_", path)
}
distValLogNormalMean <- function(path) {
  paste0("logNormalMu_", path)
}
distValLogNormalStdv <- function(path) {
  paste0("logNormalCV_", path)
}
distValLogNormalUnit <- function(path) {
  paste0("logNormalUnit_", path)
}
distValDimension <- function(path) {
  paste0("dimension_", path)
}
pkParameterSelection <- function(path) {
  paste0("pkParameterSelection", path, collapse = "_")
}
getDistValsList <- function(path) {
  distValKey <- list(
    "Uniform" = list(
      "minimum" = distValUniformMinimum(path),
      "maximum" = distValUniformMaximum(path),
      "unit" = distValUniformUnit(path)
    ),
    "LogUniform" = list(
      "minimum" = distValLogUniformMinimum(path),
      "maximum" = distValLogUniformMaximum(path),
      "unit" = distValLogUniformUnit(path)
    ),
    "Normal" = list(
      "mean" = distValNormalMean(path),
      "stdv" = distValNormalStdv(path),
      "unit" = distValNormalUnit(path)
    ),
    "LogNormal" = list(
      "mean" = distValLogNormalMean(path),
      "CV" = distValLogNormalStdv(path),
      "unit" = distValLogNormalUnit(path)
    ),
    "dimension" = distValDimension(path)
  )
  return(distValKey)
}
