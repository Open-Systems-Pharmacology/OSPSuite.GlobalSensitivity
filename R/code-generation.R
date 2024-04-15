distributionStringsFnsList <- list(Uniform = function(parameter){ paste0("distribution$Uniform(minimum = ",parameter$distribution$minimum,", maximum = ",parameter$distribution$maximum, ")") },
                                   LogUniform = function(parameter){ paste0("distribution$LogUniform(minimum = ",parameter$distribution$minimum,", maximum = ",parameter$distribution$maximum, ")") },
                                   Normal = function(parameter){ paste0("distribution$Normal(mean = ",parameter$distribution$mean,", stdv = ",parameter$distribution$stdv, ")") },
                                   LogNormal = function(parameter){ paste0("distribution$LogNormal(mean = ",parameter$distribution$mean,", CV = ",parameter$distribution$CV, ")") })

generateParameterCode <- function(path, displayName, unit, parameterDistribution) {
  code <- sprintf(
    "parametersList[[\"%s\"]] <- SAParameter$new(simulation = simulation, path = \"%s\", displayName = \"%s\", unit = \"%s\", parameterDistribution = %s)",
    path , path, displayName, unit, parameterDistribution
  )
  return(code)
}

generateOutputCode <- function(path, displayName,pkParameterList) {
  code <- sprintf(
    "outputsList[[\"%s\"]] <- SAOutput$new(simulation = simulation, path = \"%s\", displayName = \"%s\")\n",
    path, path, displayName
  )
  for (pk in pkParameterList){
    code <- paste0(code,
                   sprintf("outputsList[[\"%s\"]]$addPKParameter(standardPKParameter = \"%s\",pkParameterDisplayName = NULL, startTime = NULL, endTime = NULL)\n",path,pk) )
  }
  return(code)
}



writeParametersAndOutputsToFile <- function(parametersList,outputsList) {
  code <- NULL
  #PREAMBLE
  code <- append(x = code, values = "rm(list = ls())")
  code <- append(x = code, values = "library(ospsuite.globalsensitivity)")
  code <- append(x = code, values = "\n")
  code <- append(x = code, values = "### REPLACE BELOW WITH CORRECT PKML FILE PATHS")
  code <- append(x = code, values = "simulationFilePath = 'PATH/TO/PKML/SIMULATION.pkml'")
  code <- append(x = code, values =  "simulation <- loadSimulation(simulationFilePath)")
  code <- append(x = code, values =  "DDIsimulationFilePath <- NULL #'PATH/TO/PKML/DDI/SIMULATION.pkml'")
  code <- append(x = code, values = "DDIsimulation <- NULL")
  code <- append(x = code, values = "if(!is.null(DDIsimulationFilePath)){DDIsimulation <- loadSimulation(DDIsimulationFilePath)}")
  code <- append(x = code, values = "\n")

  #PARAMETERS
  code <- append(x =  code, values = "### Set parameters")
  code <- append(x =  code, values = "parametersList <- list()")
  parametersCode <- list()
  for (n in seq_along(parametersList)){
    code <- append(x = code, values = paste0("# ",parametersList[[n]]$path))
    parametersCode <- generateParameterCode(path = parametersList[[n]]$path,
                                            displayName = parametersList[[n]]$displayName,
                                            unit = parametersList[[n]]$unit,
                                            parameterDistribution = distributionStringsFnsList[[parametersList[[n]]$distribution$type]](parametersList[[n]]))
    code <- append(x =  code, values = parametersCode)
  }
  code <- append(x = code, values =  "\n")

  #OUTPUTS
  code <- append(x =  code, values = "### Set outputs")
  code <- append(x =  code, values = "outputsList <- list()")
  outputsCode <- list()
  for (n in seq_along(outputsList)){
    code <- append(x =  code, values = paste0("# ",outputsList[[n]]$path))
    outputsCode <- generateOutputCode(path = outputsList[[n]]$path,
                                      displayName = outputsList[[n]]$displayName,
                                      pkParameterList = outputsList[[n]]$pkParameterList)
    code <- append(x =  code, values = outputsCode)
  }
  code <- append(x = code, values =  "\n")
  return(code)
}

writeSUFunctionToFile <- function(runUncertaintyCheckbox,
                                  runSensitivityAnalysisForAllParameters,
                                  sensitivityThreshold,
                                  variationRangeNumericInput,
                                  numberOfStepsNumericInput,
                                  quantilesTestInput,
                                  numberOfUncertaintySamples,
                                  runParallel){
  code <- NULL
  code <- append(x =  code, values = "### Run Sensitivity and uncertainty analysis")
  code <- append(x =  code, values = paste0("suResults <- runSU(simulation = simulation,
                   DDIsimulation = DDIsimulation,
                   customParameters = parametersList,
                   outputs = outputsList,
                   runUncertaintyAnalysis = ",runUncertaintyCheckbox,",
                   evaluateForAllParameters = ",runSensitivityAnalysisForAllParameters,",
                   sensitivityThreshold = ",sensitivityThreshold,",
                   variationRange = ",variationRangeNumericInput,",
                   numberOfSensitivityAnalysisSteps = ",numberOfStepsNumericInput,",
                   quantiles = ", paste0("c(", quantilesTestInput ,")") ,",
                   saveFolder = getwd(),
                   numberOfUncertaintyAnalysisSamples = ",numberOfUncertaintySamples,",
                   runParallel = TRUE)"))
  return(code)
}

writeSobolSensitivityFunctionToFile <- function(numberOfSamplesGSA){
  code <- NULL
  code <- append(x =  code, values = "### Run Sobol sensitivity")
  code <- append(x =  code, values = paste0("sobolResults <- runSobol(simulation = simulation, DDIsimulation = DDIsimulation, parameters = parametersList, outputs = outputsList, numberOfSamples = ",numberOfSamplesGSA,")"))
  return(code)
}

writeEFASTSensitivityFunctionToFile <- function(numberOfReSamplesEFAST){
  code <- NULL
  code <- append(x =  code, values = "### Run EFAST sensitivity")
  code <- append(x =  code, values = paste0("EFASTResults <- runEFAST(simulation = simulation, DDIsimulation = DDIsimulation, parameters = parametersList, outputs = outputsList, numberOfResamples = ",numberOfReSamplesEFAST,")"))
  return(code)
}

writeMorrisSensitivityFunctionToFile <- function(numberOfSamples){
  code <- NULL
  code <- append(x =  code, values = "### Run Morris sensitivity")
  code <- append(x =  code, values = paste0("morrisResults <- runMorris(simulation = simulation, DDIsimulation = DDIsimulation, parameters = parametersList, outputs = outputsList, numberOfSamples = ",numberOfSamples,")"))
  return(code)
}


