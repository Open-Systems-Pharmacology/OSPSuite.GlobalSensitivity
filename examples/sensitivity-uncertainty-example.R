rm(list = ls())
library(ospsuite.globalsensitivity)
graphics.off()

simFilePath <- system.file("extdata", "standard_skin_simulation_v12.pkml", package = "ospsuite.globalsensitivity")

sim <- loadSimulation(simFilePath)
skin <- getSimulationTree(sim)


setParameterValuesByPath(parameterPaths = skin$Applications$skin_formulation_application$active$path,
                         values = 1,simulation = sim)
setParameterValuesByPath(parameterPaths = skin$Applications$skin_formulation_application$api_application$Mass_dose_per_area$path,
                         values = toBaseUnit(quantityOrDimension = ospDimensions$`Mass per area`,
                                             values = 100,
                                             unit = ospUnits$`Mass per area`$`µg/cm²`),
                         simulation = sim)

setParameterValuesByPath(parameterPaths = skin$permeant$D_sc$path,
                         values = toBaseUnit(quantityOrDimension = ospDimensions$`Diffusion coefficient`,
                                             values = 1e-8,
                                             unit = ospUnits$`Diffusion coefficient`$`cm²/s`),
                         simulation = sim)


parametersList <- list(
  SAParameter$new(simulation = sim,
                  path = skin$DERMAL_APPLICATION_AREA$skin_compartment$SC_skin_sublayer$SC_total_thickness$path,
                  displayName = "SC_thickness",
                  unit = ospUnits$Length$µm,
                  parameterDistribution = UniformDistribution$new(minimum = 13,maximum = 40)
  ),
  SAParameter$new(simulation = sim,
                  path = skin$DERMAL_APPLICATION_AREA$skin_compartment$`Hydrated SC`$path,
                  displayName = "SC_hydration",
                  unit = ospUnits$Dimensionless$Unitless,
                  parameterDistribution = UniformDistribution$new(minimum = 0,maximum = 1)
  ),
  SAParameter$new(simulation = sim,
                  path = skin$permeant$D_sc$path,
                  displayName = "D_sc",
                  unit = ospUnits$`Diffusion coefficient`$`dm²/min`,
                  parameterDistribution = LogUniformDistribution$new(minimum = 1e-12,maximum = 1e-8)
  ),
  SAParameter$new(simulation = sim,
                  path = skin$permeant$K_sc$path,
                  displayName = "K_sc",
                  unit = ospUnits$Dimensionless$Unitless,
                  parameterDistribution = LogUniformDistribution$new(minimum = 1,maximum = 100)
  ),
  SAParameter$new(simulation = sim,
                  path = skin$permeant$D_ed$path,
                  displayName = "D_ed",
                  unit = ospUnits$`Diffusion coefficient`$`dm²/min`,
                  parameterDistribution = LogUniformDistribution$new(minimum = 1e-12,maximum = 1e-8)
  ),
  SAParameter$new(simulation = sim,
                  path = skin$permeant$K_ed$path,
                  displayName = "K_ed",
                  unit = ospUnits$Dimensionless$Unitless,
                  parameterDistribution = LogUniformDistribution$new(minimum = 1,maximum = 100)
  ),
  SAParameter$new(simulation = sim,
                  path = skin$permeant$D_de$path,
                  displayName = "D_de",
                  unit = ospUnits$`Diffusion coefficient`$`dm²/min`,
                  parameterDistribution = LogUniformDistribution$new(minimum = 1e-12,maximum = 1e-8)
  ),
  SAParameter$new(simulation = sim,
                  path = skin$permeant$K_de$path,
                  displayName = "K_de",
                  unit = ospUnits$Dimensionless$Unitless,
                  parameterDistribution = LogUniformDistribution$new(minimum = 1,maximum = 100)
  )
)


SC <- SAOutput$new(simulation = sim,
                   path = skin$DERMAL_APPLICATION_AREA$permeant$Stratum_corneum_observer$path,
                   displayName = "SC")
SC$addPKParameter(standardPKParameter = "C_max")
SC$addPKParameter(standardPKParameter = "AUC_inf")

ED <- SAOutput$new(simulation = sim,
                   path = skin$DERMAL_APPLICATION_AREA$permeant$Epidermis_observer$path,
                   displayName = "ED")
ED$addPKParameter(standardPKParameter = "C_max")


outputList <- list(SC,ED)

numberOfSamples <- 100



su <- runSU(simulation = sim,
            evaluateForAllParameters = FALSE,
            customParameters = parametersList,
            outputs = outputList,
            saveFolder = "data",
            numberOfUncertaintyAnalysisSamples = numberOfSamples,
            runParallel = TRUE)
