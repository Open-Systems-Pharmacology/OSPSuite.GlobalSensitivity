rm(list = ls())
library(ospsuite.globalsensitivity)

simulation <- loadSimulation(filePath = system.file("extdata", "standard_skin_simulation_v12.pkml", package = "ospsuite.globalsensitivity"))
skin <- getSimulationTree(simulation)

SC <- SAOutput$new(simulation = simulation,
                   path = skin$DERMAL_APPLICATION_AREA$permeant$Stratum_corneum_observer$path,
                   displayName = "SC")
SC$addPKParameter(standardPKParameter = "C_max")
outputs <- list(SC)

for(op in outputs){
  ospsuite::addOutputs(quantitiesOrPaths = op$path ,simulation = simulation)
}

localSensitivity <- runSU(simulation = simulation,
                          evaluateForAllParameters = TRUE,
                          outputs = outputs)

localSensitivityTable <- getSUSummaryDf(localSensitivity$Results)
