rm(list = ls())
graphics.off()
library(ospsuite.globalsensitivity)

simulationFilePath = system.file("extdata", "Itraconazole 100 mg IV SD.pkml", package = "ospsuite.globalsensitivity")
simulation <- loadSimulation(simulationFilePath)
tree <- getSimulationTree(simulation)


### Create a vector of paths of compound-specific parameters in the Itraconazole PK-Sim model that are not defined by a formula
constantParameterPaths <- sapply(ospsuite::getAllParametersForSensitivityAnalysisMatching(paths = "**", simulation = simulation),
                                 function(x){x$path})
targetPaths <- c("Fraction unbound","Lipophilicity","kcat","2004|Ki","Km")
sensitivityAnalysisPaths <- unlist(lapply(targetPaths, function(parName) {
  constantParameterPaths[grep(pattern = parName, x = constantParameterPaths, fixed = TRUE)]
}))

### Append to the vector of `constantParameterPaths` the paths to the `Permeability` and `Blood/Plasma concentration ratio` parameters in the simulation
sensitivityAnalysisPaths <- c(sensitivityAnalysisPaths,
                              tree$Itraconazole$Permeability$path,
                              tree$`Hydroxy-Itraconazole`$Permeability$path,
                              tree$`Keto-Itraconazole`$Permeability$path,
                              tree$`N-desalkyl-Itraconazole`$Permeability$path,
                              tree$Itraconazole$`Blood/Plasma concentration ratio`$path,
                              tree$`Hydroxy-Itraconazole`$`Blood/Plasma concentration ratio`$path,
                              tree$`Keto-Itraconazole`$`Blood/Plasma concentration ratio`$path,
                              tree$`N-desalkyl-Itraconazole`$`Blood/Plasma concentration ratio`$path)


### Create list of SAParameter objects corresponding to the paths in `sensitivityAnalysisPaths`
parametersList <- list()
for (pth in sensitivityAnalysisPaths){
  parametersList[[pth]] <- SAParameter$new(simulation = simulation,
                                           path = pth)
}


### Create list of SAOutput objects
outputsList <- list()
# Add output corresponding to the path Organism|PeripheralVenousBlood|Itraconazole|Plasma (Peripheral Venous Blood) in the Itraconazole simulation
outputsList[["Organism|PeripheralVenousBlood|Itraconazole|Plasma (Peripheral Venous Blood)"]] <- SAOutput$new(simulation = simulation, path = "Organism|PeripheralVenousBlood|Itraconazole|Plasma (Peripheral Venous Blood)", displayName = "PeripheralVenousBlood")

# Specify that the PK parameter to be analyzed for sensitivity is the Cmax of Organism|PeripheralVenousBlood|Itraconazole|Plasma (Peripheral Venous Blood)
outputsList[["Organism|PeripheralVenousBlood|Itraconazole|Plasma (Peripheral Venous Blood)"]]$addPKParameter(standardPKParameter = "C_max",pkParameterDisplayName = NULL, startTime = NULL, endTime = NULL)



### Run Morris sensitivity
morrisResults <- runMorris(simulation = simulation, parameters = parametersList, outputs = outputsList, numberOfSamples = 10)
morrisPlot <- generateMorrisPlot(morrisResults = morrisResults$Results,logPlot = FALSE)

### Select the five most influential parameters as per the Morris sensitivity analysis results
sensitiveParameters <- morrisResults$Results[order(-morrisResults$Results$rankingNorm)[1:5],]$Parameter


### Run EFAST sensitivity for the five most influential parameters as per the Morris sensitivity analysis results
efastResults <- runEFAST(simulation = simulation,
                         parameters = parametersList[sensitiveParameters],
                         outputs = outputsList,numberOfResamples = 1)
efastGSABarsGraph <- generateEFASTBarGraph(gsaResultsDataframe = efastResults$Results)

