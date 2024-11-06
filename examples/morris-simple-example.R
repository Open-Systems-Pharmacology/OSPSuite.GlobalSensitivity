rm(list = ls())
library(ospsuite.globalsensitivity)

simFilePath <- system.file("extdata", "qianSimGSA.pkml", package = "ospsuite.globalsensitivity")
sim <- loadSimulation(simFilePath)
tree <- getSimulationTree(sim)


parametersList <- list(
  SAParameter$new(simulation = sim,
                  path = tree$Comp$X1$path,
                  displayName = "X1",
                  unit = ospUnits$Dimensionless$Unitless,
                  parameterDistribution = UniformDistribution$new(minimum = 0,maximum = 1)
  ),
  SAParameter$new(simulation = sim,
                  path = tree$Comp$X2$path,
                  displayName = "X2",
                  unit = ospUnits$Dimensionless$Unitless,
                  parameterDistribution = UniformDistribution$new(minimum = 0,maximum = 1)
  ),
  SAParameter$new(simulation = sim,
                  path = tree$Comp$X3$path,
                  displayName = "X3",
                  unit = ospUnits$Dimensionless$Unitless,
                  parameterDistribution = UniformDistribution$new(minimum = 0,maximum = 1)
  ),
  SAParameter$new(simulation = sim,
                  path = tree$Comp$X4$path,
                  displayName = "X4",
                  unit = ospUnits$Dimensionless$Unitless,
                  parameterDistribution = UniformDistribution$new(minimum = 0,maximum = 1)
  )
)


Y <- SAOutput$new(simulation = sim,
                  path = tree$Comp$A$Y$path,
                  displayName = "Y")
Y$addPKParameter(standardPKParameter = "C_max")
outputList <- list(Y)

numberOfSamples <- 100
morrisResults <- runMorris(simulation = sim,
                           parameters = parametersList,
                           outputs = outputList,
                           numberOfSamples = numberOfSamples)

plts <- generateMorrisPlot(morrisResults$Results)

print(morrisResults)
