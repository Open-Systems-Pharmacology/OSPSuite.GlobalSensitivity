test_that("UniformDistribution exposes its type and bounds and samples correctly", {
  dist <- UniformDistribution$new(minimum = 2, maximum = 6)

  expect_s3_class(dist, "SADistribution")
  expect_equal(dist$type, "Uniform")
  expect_equal(dist$minimum, 2)
  expect_equal(dist$maximum, 6)
  expect_equal(dist$quantilesToSample(c(0, 0.5, 1)), c(2, 4, 6))
})

test_that("UniformDistribution rejects non-numeric bounds", {
  expect_error(
    UniformDistribution$new(minimum = "a", maximum = 6),
    "must be of type 'numeric'"
  )
})

test_that("LogUniformDistribution exposes its type and samples geometrically", {
  dist <- LogUniformDistribution$new(minimum = 1, maximum = 100)

  expect_equal(dist$type, "LogUniform")
  expect_equal(dist$quantilesToSample(c(0, 0.5, 1)), c(1, 10, 100))
})

test_that("NormalDistribution exposes its parameters and samples at the mean", {
  dist <- NormalDistribution$new(mean = 5, stdv = 2)

  expect_equal(dist$type, "Normal")
  expect_equal(dist$mean, 5)
  expect_equal(dist$stdv, 2)
  expect_equal(dist$quantilesToSample(0.5), 5)
})

test_that("NormalDistribution rejects a negative standard deviation", {
  expect_error(NormalDistribution$new(mean = 0, stdv = -1), "must be positive")
})

test_that("LogNormalDistribution exposes its parameters and rejects negative CV", {
  dist <- LogNormalDistribution$new(mean = 1, CV = 0.5)

  expect_equal(dist$type, "LogNormal")
  expect_equal(dist$mean, 1)
  expect_equal(dist$CV, 0.5)
  expect_error(LogNormalDistribution$new(mean = 1, CV = -0.1), "must be non-negative")
})

test_that("the distribution factory builds the matching distribution objects", {
  expect_s3_class(distribution$Uniform(minimum = 0, maximum = 1), "UniformDistribution")
  expect_s3_class(distribution$LogUniform(minimum = 1, maximum = 2), "LogUniformDistribution")
  expect_s3_class(distribution$Normal(mean = 0, stdv = 1), "NormalDistribution")
  expect_s3_class(distribution$LogNormal(mean = 1, CV = 1), "LogNormalDistribution")
})
