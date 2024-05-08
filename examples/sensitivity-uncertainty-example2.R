rm(list = ls())
graphics.off()
library(ospsuite.globalsensitivity)
simFilePath <- system.file("extdata", "standard_skin_simulation_v12.pkml", package = "ospsuite.globalsensitivity")

sim <- loadSimulation(simFilePath)
skin <- getSimulationTree(sim)

#****** CREATE A LIST OF PARAMETERS THAT YOU WANT TO ANALYZE
#****** FOR THE UNCERTAINTY ANALYSIS, IF YOU WANT TO SPECIFY A DISTRIBUTION FOR SOME PARAMETERS THAT ARE NOT LOGUNIFORM +/- 10%, YOU CAN SPECIFY THESE DISTRIBUTIONS IN THE FOLLOWING LIST
parametersList <- list(
  SAParameter$new(simulation = sim,
                  path = skin$DERMAL_APPLICATION_AREA$skin_compartment$SC_skin_sublayer$SC_total_thickness$path,
                  displayName = "SC_thickness",
                  unit = ospUnits$Length$µm,
                  parameterDistribution = distribution$Uniform(minimum = 10,maximum = 40)  #******Maximum SC thickness = 40µm, minimum = 10µm
  ),
  SAParameter$new(simulation = sim,
                  path = skin$DERMAL_APPLICATION_AREA$skin_compartment$`Hydrated SC`$path,
                  displayName = "SC_hydration",
                  parameterDistribution = distribution$Uniform(minimum = 0,maximum = 1) #******  0 = part hydrated, 1 =fully hydrated
  ),
  SAParameter$new(simulation = sim,
                  path = skin$permeant$D_sc$path,
                  displayName = "D_sc" #****** No distribution specified so default will be logUniform with max and min = +/- 10% of default value in pkml file
  ),
  SAParameter$new(simulation = sim,
                  path = skin$permeant$K_sc$path,
                  displayName = "K_sc"
  ),
  SAParameter$new(simulation = sim,
                  path = skin$permeant$D_ed$path,
                  displayName = "D_ed"
  ),
  SAParameter$new(simulation = sim,
                  path = skin$permeant$K_ed$path,
                  displayName = "K_ed"
  ),
  SAParameter$new(simulation = sim,
                  path = skin$permeant$D_de$path,
                  displayName = "D_de"
  ),
  SAParameter$new(simulation = sim,
                  path = skin$permeant$K_de$path,
                  displayName = "K_de"
  )
)

#****** SPECIFY THE OUTPUTS AND THEIR PK PARAMETERS
SC <- SAOutput$new(simulation = sim,
                   path = skin$DERMAL_APPLICATION_AREA$permeant$Stratum_corneum_observer$path,
                   displayName = "SC")
SC$addPKParameter(standardPKParameter = "C_max")
SC$addPKParameter(standardPKParameter = "AUC_tEnd")

ED <- SAOutput$new(simulation = sim,
                   path = skin$DERMAL_APPLICATION_AREA$permeant$Epidermis_observer$path,
                   displayName = "ED")
ED$addPKParameter(standardPKParameter = "C_max")


outputList <- list(SC,ED)

numberOfSamples <- 5

#****** RUN THE SENSITIVITY AND UNCERTAINTY ANALYSIS.
su <- runSU(simulation = sim,
            evaluateForAllParameters = TRUE, #****** IF FALSE, ONLY THE PARAMETERS IN parametersList WILL BE ANALYZED
            customParameters = parametersList,
            variationRange = 0.2, #
            numberOfSensitivityAnalysisSteps = 2,
            quantiles = c(0.05,0.25,0.5,0.75,0.95),
            outputs = outputList,
            saveFolder = "data", #****** FOLDER NAME AND PATH WHERE RESULTS WILL BE SAVED.  IF NOT PROVIDED, RESULTS WILL NOT BE SAVED.
            numberOfUncertaintyAnalysisSamples = numberOfSamples, #****** FOR THE UNCERTAINTY ANALYSIS ONLY, THESE SAMPLES WILL BE TAKEN FROM LOGUNIFORM +/- 10% BY DEFAULT, UNLESS A DIFFERENT DISTRIBUTION WAS SPECIFIED IN parametersList
            runParallel = TRUE)

suSummary <- getSUSummaryDf(su$Results)
