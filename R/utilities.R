`%||%` <- function(lhs,rhs){
  if(is.null(lhs)){
    return(rhs)
  }
  return(lhs)
}


error <- function(condition,errorMessage = NULL){
  if(condition){
    stop(errorMessage)
  }
  return(NULL)
}




buildSettingsCMD <- function(parameters,outputs){

  resultsSettings <- NULL

  parameterPaths <- sapply(parameters,function(par){par$path})
  names(parameters) <- parameterPaths
  outputPaths <- sapply(outputs,function(op){op$path})
  names(outputs) <- outputPaths

  for (pth in parameterPaths) {
    pathSetting <- paste("Distribution for", pth, "is", parameters[[pth]]$distribution$type)
    resultsSettings <- c(resultsSettings,pathSetting)

    dist <- parameters[[pth]]$distribution$type
    distValKey <- getDistValsList(pth)
    distParameterNames <- names(distValKey[[dist]])

    for (distParameterName in distParameterNames) {

      if(distParameterName == "unit"){
        unit <- parameters[[pth]]$unit
        valueSetting <- paste("The unit of the random variable of the", dist, "distribution for path", pth, "is", parameters[[pth]]$unit)
        if (unit == ""){
          valueSetting <- paste("The random variable of the", dist, "distribution for path", pth, "is dimensionless")
        }
      } else {
        valueSetting <- paste("Value of", distParameterName, "of the", dist, "distribution for path", pth, "is", parameters[[pth]]$distribution[[distParameterName]])
      }

      resultsSettings <- c(resultsSettings,valueSetting)
    }

  }

  for (pth in names(outputs)) {
    input_string <- pkParameterSelection(pth)
    outputSetting <- paste("PK selections for", pth, "are", paste0(outputs[[pth]]$pkParameterList, collapse = ", "))
    resultsSettings <- c(resultsSettings,outputSetting)
  }

  resultsSettings <- data.frame(Settings = resultsSettings)
  print(resultsSettings)
  return(resultsSettings)

}


checkParametersExistInSimulation <- function(simulation,parameterPaths,simulationName = NULL,stopIfNotFound = FALSE){

  responseFn <- warning
  if(stopIfNotFound){
    responseFn <- stop
  }

  if(is.null(simulationName)){
    simulationName <- simulation$sourceFile
  }
  parameterPresent <- NULL
  for (pth in parameterPaths){
    parameterPresent[[pth]] <- TRUE
    if(is.null(ospsuite::getAllParametersMatching(paths = pth, simulation))){
      responseFn(paste0("Parameter with path ", pth ," not found in " , simulationName, "."))
      parameterPresent[[pth]] <- FALSE
    }
  }
  return(parameterPresent)
}

checkOutputsExistInSimulation <- function(simulation,outputPaths,simulationName = NULL,stopIfNotFound = FALSE){

  responseFn <- warning
  if(stopIfNotFound){
    responseFn <- stop
  }

  if(is.null(simulationName)){
    simulationName <- simulation$sourceFile
  }
  outputPresent <- NULL
  for (pth in outputPaths){
    outputPresent[[pth]] <- TRUE
    if(is.null(ospsuite::getAllQuantitiesMatching(paths = pth, simulation))){
      responseFn(paste0("Output with path ", pth ," not found in " , simulationName, "."))
      outputPresent[[pth]] <- FALSE
    }
  }
  return(outputPresent)
}
