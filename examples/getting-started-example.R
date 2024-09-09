rm(list = ls())
library(ospsuite.globalsensitivity)


### REPLACE BELOW WITH CORRECT PKML FILE PATHS
simulationFilePath = system.file("extdata", "getting-started-example.pkml", package = "ospsuite.globalsensitivity")
simulation <- loadSimulation(simulationFilePath)
DDIsimulationFilePath <- NULL #'PATH/TO/PKML/DDI/SIMULATION.pkml'
DDIsimulation <- NULL
if(!is.null(DDIsimulationFilePath)){DDIsimulation <- loadSimulation(DDIsimulationFilePath)}


### Set parameters
parametersList <- list()
# A|X1
parametersList[["A|X1"]] <- SAParameter$new(simulation = simulation, path = "A|X1", displayName = "p1", unit = "", parameterDistribution = distribution$Uniform(minimum = 0, maximum = 1))
# A|X2
parametersList[["A|X2"]] <- SAParameter$new(simulation = simulation, path = "A|X2", displayName = "p2", unit = "", parameterDistribution = distribution$Uniform(minimum = 0, maximum = 1))
# A|X3
parametersList[["A|X3"]] <- SAParameter$new(simulation = simulation, path = "A|X3", displayName = "p3", unit = "", parameterDistribution = distribution$Uniform(minimum = 0, maximum = 1))
# A|X4
parametersList[["A|X4"]] <- SAParameter$new(simulation = simulation, path = "A|X4", displayName = "p4", unit = "", parameterDistribution = distribution$Uniform(minimum = 0, maximum = 1))


### Set outputs
outputsList <- list()
# Organism|A|Z
outputsList[["Organism|A|Z"]] <- SAOutput$new(simulation = simulation, path = "Organism|A|Z", displayName = "z")
outputsList[["Organism|A|Z"]]$addPKParameter(standardPKParameter = "C_max",pkParameterDisplayName = NULL, startTime = NULL, endTime = NULL)


### Run Morris sensitivity
morrisResults <- runMorris(simulation = simulation, DDIsimulation = DDIsimulation, parameters = parametersList, outputs = outputsList, numberOfSamples = 1000)
morrisPlot <- generateMorrisPlot(morrisResults = morrisResults$Results)

### Run Sobol sensitivity
sobolResults <- runSobol(simulation = simulation, DDIsimulation = DDIsimulation, parameters = parametersList, outputs = outputsList, numberOfSamples = 1000)
sobolPlot <- generateSobolBarGraph(gsaResultsDataframe = sobolResults$Results)

### Run EFAST sensitivity
EFASTResults <- runEFAST(simulation = simulation, DDIsimulation = DDIsimulation, parameters = parametersList, outputs = outputsList, numberOfResamples = 10)
efastPlot <- generateEFASTBarGraph(gsaResultsDataframe = EFASTResults$Results)
