# These tests pin down the ospsuite SimulationBatch behaviour that the Morris and
# uncertainty analyses rely on: queued run values are only cleared when the .NET
# concurrent runner completes.  If it throws, the values stay queued and execute
# alongside whatever is queued next, which is why the analyses recreate their
# batches after a hard failure.
#
# A hard failure inside the .NET runner is forced by passing the same batch
# twice, which raises a NotUniqueIdException before any simulation runs.

simPath <- system.file("extdata", "getting-started-example.pkml", package = "ospsuite.globalsensitivity")
parameterPath <- "A|X1"

test_that("a successful batch run clears the queued run values", {
  simulation <- ospsuite::loadSimulation(simPath)
  batch <- getSimulationBatches(simulation, parameterPath, numberParallelThreads = 1)[[1]]

  runValuesId <- batch$addRunValues(parameterValues = 1)
  expect_equal(batch$runValuesIds, runValuesId)

  results <- ospsuite::runSimulationBatches(simulationBatches = list(batch))

  expect_length(batch$runValuesIds, 0)
  expect_equal(names(results[[1]]), runValuesId)
})

test_that("a hard failure inside the .NET runner leaves run values queued", {
  simulation <- ospsuite::loadSimulation(simPath)
  batch <- getSimulationBatches(simulation, parameterPath, numberParallelThreads = 1)[[1]]

  staleId <- batch$addRunValues(parameterValues = 1)
  expect_error(ospsuite::runSimulationBatches(simulationBatches = list(batch, batch)))

  expect_equal(batch$runValuesIds, staleId)
})

test_that("stale run values from a failed run execute first on the next run of the same batch", {
  simulation <- ospsuite::loadSimulation(simPath)
  batch <- getSimulationBatches(simulation, parameterPath, numberParallelThreads = 1)[[1]]

  staleId <- batch$addRunValues(parameterValues = 1)
  expect_error(ospsuite::runSimulationBatches(simulationBatches = list(batch, batch)))

  newId <- batch$addRunValues(parameterValues = 2)
  results <- ospsuite::runSimulationBatches(simulationBatches = list(batch))

  # Both the stale and the new values ran, and the stale result comes first,
  # so reading results[[1]][[1]] attributes it to the new values.
  expect_equal(names(results[[1]]), c(staleId, newId))
  expect_false(identical(names(results[[1]])[[1]], newId))
})

test_that("a recreated batch runs only the newly queued values after a hard failure", {
  simulation <- ospsuite::loadSimulation(simPath)
  batch <- getSimulationBatches(simulation, parameterPath, numberParallelThreads = 1)[[1]]

  batch$addRunValues(parameterValues = 1)
  expect_error(ospsuite::runSimulationBatches(simulationBatches = list(batch, batch)))

  batch <- getSimulationBatches(simulation, parameterPath, numberParallelThreads = 1)[[1]]
  expect_length(batch$runValuesIds, 0)

  newId <- batch$addRunValues(parameterValues = 2)
  results <- ospsuite::runSimulationBatches(simulationBatches = list(batch))

  expect_equal(names(results[[1]]), newId)
})

test_that("an error raised after the .NET runner completes still clears the queue", {
  simulation <- ospsuite::loadSimulation(simPath)
  batch <- getSimulationBatches(simulation, parameterPath, numberParallelThreads = 1)[[1]]

  # Two values for one variable parameter fail that run; stopIfFails turns the
  # failure into an R error raised only after the runner has finished.
  batch$addRunValues(parameterValues = c(1, 2))
  expect_error(ospsuite::runSimulationBatches(simulationBatches = list(batch), stopIfFails = TRUE))

  expect_length(batch$runValuesIds, 0)
})

test_that("getBatchRunResult returns the result for the requested run values even when stale values also ran", {
  simulation <- ospsuite::loadSimulation(simPath)
  batch <- getSimulationBatches(simulation, parameterPath, numberParallelThreads = 1)[[1]]

  staleId <- batch$addRunValues(parameterValues = 1)
  expect_error(ospsuite::runSimulationBatches(simulationBatches = list(batch, batch)))

  newId <- batch$addRunValues(parameterValues = 2)
  results <- ospsuite::runSimulationBatches(simulationBatches = list(batch))

  expect_identical(getBatchRunResult(results, 1, newId), results[[1]][[newId]])
  expect_false(identical(getBatchRunResult(results, 1, newId), results[[1]][[staleId]]))
})

test_that("getBatchRunResult returns NULL for a failed run or an unknown id", {
  expect_null(getBatchRunResult(NULL, 1, "some-id"))
  expect_null(getBatchRunResult(list(list(a = 1)), 1, "other-id"))
  expect_null(getBatchRunResult(list(list(a = 1)), 2, "a"))
  expect_equal(getBatchRunResult(list(list(a = 1)), 1, "a"), 1)
})
