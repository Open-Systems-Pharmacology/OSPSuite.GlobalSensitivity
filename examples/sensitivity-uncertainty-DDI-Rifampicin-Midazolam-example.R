rm(list = ls())
library(ospsuite.globalsensitivity)


simFilePath <- system.file("extdata", "DDI Control - Midazolam - Backman 1996 - with Rif.pkml", package = "ospsuite.globalsensitivity")
DDIsimFilePath <- system.file("extdata", "DDI Treatment - Rifampicin_Midazolam - Backman 1996.pkml", package = "ospsuite.globalsensitivity")
sim <- loadSimulation(simFilePath)
DDIsim <- loadSimulation(DDIsimFilePath)
tree <- getSimulationTree(sim)

parametersList <- list(
  SAParameter$new(simulation = sim,
                  path = tree$`Midazolam-CYP3A4-Optimized`$Km$path,
                  displayName = "Km",
                  unit = ospUnits$`Concentration [molar]`$`µmol/l`,
                  parameterDistribution = distribution$LogUniform(minimum = 2,maximum = 8)
  ),
  SAParameter$new(simulation = sim,
                  path = tree$`Midazolam-CYP3A4-Optimized`$kcat$path,
                  displayName = "kcat",
                  unit = ospUnits$`Inversed time`$`1/min`,
                  parameterDistribution = distribution$LogUniform(minimum = 8.76/2,maximum = 8.76*2)
  ),
  SAParameter$new(simulation = sim,
                  path = tree$Midazolam$Lipophilicity$path,
                  displayName = "Lipophilicity",
                  unit = ospUnits$`Log Units`$`Log Units`,
                  parameterDistribution = distribution$Uniform(minimum = 2,maximum = 4)
  ),
  SAParameter$new(simulation = sim,
                  path = tree$Midazolam$`Fraction unbound (plasma, reference value)`$path,
                  displayName = "fu",
                  unit = ospUnits$Dimensionless$Unitless,
                  parameterDistribution = distribution$Uniform(minimum = 0.01,maximum = 0.05)
  ),
  SAParameter$new(simulation = sim,
                  path = tree$Rifampicin$`CYP3A4-Templeton 2011`$EC50$path,
                  displayName = "EC50",
                  unit = ospUnits$`Concentration [molar]`$`µmol/l`,
                  parameterDistribution = distribution$LogUniform(minimum = 0.34*0.5,maximum = 0.34/0.5)
  ),
  SAParameter$new(simulation = sim,
                  path = tree$Rifampicin$`CYP3A4-Templeton 2011`$Emax$path,
                  displayName = "Emax",
                  unit = ospUnits$Dimensionless$Unitless,
                  parameterDistribution = distribution$LogUniform(minimum = 9*0.5,maximum = 9/0.5)
  )
)

Y <- SAOutput$new(simulation = sim,
                  path = tree$Organism$PeripheralVenousBlood$Midazolam$`Plasma (Peripheral Venous Blood)`$path,
                  displayName = "Midazolam-PeripheralVenousBlood")
Y$addPKParameter(standardPKParameter = "C_max")
Y$addPKParameter(standardPKParameter = "AUC_inf")
outputList <- list(Y)

numberOfSamples <- 100

#****** RUN THE SENSITIVITY AND UNCERTAINTY ANALYSIS.
su <- runSU(simulation = sim,
            DDIsimulation = DDIsim,
            customParameters = parametersList,
            outputs = outputList,
            evaluateForAllParameters = FALSE,
            #Sensitivity analysis parameters:
            variationRange = 0.2, #
            numberOfSensitivityAnalysisSteps = 2,
            sensitivityThreshold = 0,
            #Uncertainty analysis parameters:
            runUncertaintyAnalysis = TRUE,
            numberOfUncertaintyAnalysisSamples = numberOfSamples,
            quantiles = c(0.05,0.25,0.5,0.75,0.95))

generateAndSaveSUSummaryDf(suResults = su,savePath = "data/su-DDI-summary-DF.xlsx")
