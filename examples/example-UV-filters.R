rm(list = ls())
#Load the OSP Global Sensitivity R package
library(ospsuite.globalsensitivity)

#Load octocrylene model simulation
simFilePath <- system.file("extdata", "octocrylene-gsa.pkml", package = "ospsuite.globalsensitivity")
sim <- loadSimulation(simFilePath)

#OR

# #Load oxybenzone model simulation
# simFilePath <- system.file("extdata", "oxybenzone-gsa.pkml", package = "ospsuite.globalsensitivity")
# sim <- loadSimulation(simFilePath)


#Create a list of Parameter objects corresponding to parameters that exist in the Octocrylene model.
parametersList <- list(
  SAParameter$new(simulation = sim,
                  path = "DERMAL_APPLICATION_AREA|vehicle|scale_factor_vehicle_evaporation",
                  displayName = "scale_factor_vehicle_evaporation",
                  unit = "",
                  parameterDistribution = LogUniformDistribution$new (minimum = 1e-2, maximum = 1e2)
  ),
  SAParameter$new(simulation = sim,
                  path = "DERMAL_APPLICATION_AREA|vehicle|delta_trans",
                  displayName = "delta_trans",
                  unit = ospUnits$Dimensionless$Unitless,
                  parameterDistribution = LogUniformDistribution$new(minimum = 1e-1, maximum = 1e1)
  ),
  SAParameter$new(simulation = sim,
                  path = "DERMAL_APPLICATION_AREA|vehicle|fraction_vehicle_volume_non_volatile",
                  displayName = "fraction_vehicle_volume_non_volatile",
                  unit = "",
                  parameterDistribution = UniformDistribution$new(minimum = 0, maximum = 1)
  ),
  SAParameter$new(simulation = sim,
                  path = "DERMAL_APPLICATION_AREA|vehicle|beta",
                  displayName = "beta"
  )
)


#Create an Output object corresponding to a simulated quantity that exists in the Octocrylene model.
Y <- SAOutput$new(simulation = sim,
                  path = "DERMAL_APPLICATION_AREA|in_vivo_sink|permeant|whole_body_concentration",
                  displayName = "whole_body_concentration")
#Create an output object corresponding to a simulated quantity that exists in the Midazolam and DDI models
Y$addPKParameter(standardPKParameter = "AUC_tEnd")
outputList <- list(Y)


#Run local sensitivity analysis and uncertainty analysis
su <- runSU(simulation = sim,
            customParameters = parametersList,
            outputs = outputList,
            evaluateForAllParameters = FALSE,
            #Sensitivity analysis parameters:
            variationRange = 0.2, #
            numberOfSensitivityAnalysisSteps = 2,
            sensitivityThreshold = 0,
            #Uncertainty analysis parameters:
            runUncertaintyAnalysis = TRUE,
            numberOfUncertaintyAnalysisSamples = 100,
            quantiles = c(0.05,0.25,0.5,0.75,0.95),
            saveResults = TRUE, saveFolder = "folder/path",saveFileName = "SU-UVFilter.xlsx")
#Specify saveFolder before running script ^^^^^^^^^^^^^^^^

#Run Morris sensitivity analysis
morrisResults <- runMorris(simulation = sim,
                           parameters = parametersList,
                           outputs = outputList,
                           numberOfSamples = 100,
                           saveResults = TRUE, saveFolder = "folder/path",saveFileName = "morris-UVFilter.xlsx")
#Specify saveFolder before running script ^^^^^^^^^^^^^^^^

#Run Sobol sensitivity analysis
sobolResults <- runSobol(simulation = sim,
                         parameters = parametersList,
                         outputs = outputList,
                         numberOfSamples = 2000,
                         saveResults = TRUE, saveFolder = "folder/path",saveFileName = "sobol-UVFilter.xlsx")
#Specify saveFolder before running script ^^^^^^^^^^^^^^^^

#Run EFAST sensitivity analysis
efastResults <- runEFAST(simulation = sim,
                         parameters = parametersList,
                         outputs = outputList,
                         numberOfResamples = 1,
                         saveResults = TRUE, saveFolder = "folder/path",saveFileName = "efast-UVFilter.xlsx")
#Specify saveFolder before running script ^^^^^^^^^^^^^^^^



