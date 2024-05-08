rm(list = ls())
library(ospsuite.globalsensitivity)

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
                  parameterDistribution = distribution$Uniform(minimum = 13,maximum = 40)
  ),
  SAParameter$new(simulation = sim,
                  path = skin$DERMAL_APPLICATION_AREA$skin_compartment$`Hydrated SC`$path,
                  displayName = "SC_hydration",
                  unit = ospUnits$Dimensionless$Unitless,
                  parameterDistribution = distribution$Uniform(minimum = 0,maximum = 1)
  ),
  SAParameter$new(simulation = sim,
                  path = skin$permeant$D_sc$path,
                  displayName = "D_sc",
                  unit = ospUnits$`Diffusion coefficient`$`dm²/min`,
                  parameterDistribution = distribution$LogUniform(minimum = 1e-12,maximum = 1e-8)
  )
)


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


parameterPaths <- c(skin$DERMAL_APPLICATION_AREA$skin_compartment$SC_skin_sublayer$SC_total_thickness$path,
                    skin$DERMAL_APPLICATION_AREA$skin_compartment$`Hydrated SC`$path)

numberOfSamples <- 10

sobolResults <- runSobol(simulation = sim, parameters = parametersList , outputs = outputList , numberOfSamples = numberOfSamples)
saveRDS(sobolResults,paste0("data/skin-sobol-results-df-",numberOfSamples,"-samples.rds"))
lowryPlot <- generateLowryPlot(gsaResultsDataframe = sobolResults$Results)
methods::show(lowryPlot$`DERMAL_APPLICATION_AREA|permeant|Stratum_corneum_observer`$C_max)
methods::show(lowryPlot$`DERMAL_APPLICATION_AREA|permeant|Stratum_corneum_observer`$AUC_tEnd)
methods::show(lowryPlot$`DERMAL_APPLICATION_AREA|permeant|Epidermis_observer`$C_max)
