getUniformSampleVector <- function(quantileVec,minimum,maximum){
  verifyQuantileVec(quantileVec)
  return(sapply(quantileVec, function(q) {
    ((1 - q) * minimum) + (q * maximum)
  }))
}

getLogUniformSampleVector <- function(quantileVec,minimum,maximum){


  verifyQuantileVec(quantileVec)
  error(minimum > maximum, "Parameter 'minimum' for a logUniform distritubed parameter may not exceed parameter 'maximum'.")
  error(sign(minimum) != sign(maximum), "Parameters 'minimum' and 'maximum' for a logUniform distritubed parameter must be of the same sign.")

  if( minimum == 0 & maximum == 0 ){
    return(rep(0,length(quantileVec)))
  }

  error( minimum == 0 | maximum == 0, "Parameters 'minimum' and 'maximum' for a logUniform distritubed parameter must be non-zero.")

  logMinimum <- 0
  logMaximum <- log(maximum/minimum)

  logSpaceQuantileVec <- sapply(quantileVec, function(q) {
    ((1 - q) * logMinimum) + (q * logMaximum)
  })
  logUniformQuantileVec <- minimum*exp(logSpaceQuantileVec)
  return(logUniformQuantileVec)

}

normalDistributionTruncationQuantile <- 0.025

getNormalSampleVector <- function(quantileVec, mean, stdv, truncate = TRUE) {
  verifyQuantileVec(quantileVec)
  if(truncate){
    quantileVec <- (normalDistributionTruncationQuantile*(1-quantileVec)) + ((1-normalDistributionTruncationQuantile)*quantileVec)
  }
  return(qnorm(p = quantileVec, mean = mean, sd = stdv))
}

getLogNormalSampleVector <- function(quantileVec, mean, CV){
  sigmaSquared <- log((CV^2) + 1)
  mu <- log(mean) - (sigmaSquared/2)
  sigma <- sqrt(sigmaSquared)
  logSpaceQuantileVec <- getNormalSampleVector(quantileVec, mean = mu, stdv = sigma, truncate = FALSE)
  return(exp(logSpaceQuantileVec))
}

uniformQuantilesToSamples <- function(quantileVec, distributionParametersList) {
  minimum <- distributionParametersList$minimum
  maximum <- distributionParametersList$maximum
  return(getUniformSampleVector(quantileVec,minimum,maximum))
}

logUniformQuantilesToSamples <- function(quantileVec, distributionParametersList) {
  minimum <- distributionParametersList$minimum
  maximum <- distributionParametersList$maximum
  return(getLogUniformSampleVector(quantileVec,minimum,maximum))
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


uniformInputs <- function(minimum,maximum){
  return(list(minimum = minimum,maximum = maximum))
}

logUniformInputs <- function(minimum,maximum){
  return(list(minimum = minimum,maximum = maximum))
}

normalInputs <- function(mean, stdv) {
  return(list(mean = mean, stdv = stdv))
}

logNormalInputs <- function(mean, CV){
  return(list(mean = mean, CV = CV))
}

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
