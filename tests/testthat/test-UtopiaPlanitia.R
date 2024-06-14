library(testthat)
library(grf)
library(UtopiaPlanitia)

test_that("vis generated", {
  set.seed(123) # Ensure reproducibility in tests

  # Generate synthetic data
  X <- as.matrix(rnorm(100, 0, 1))
  Y <- rnorm(100, 0, 1)
  W <- rbinom(100, 1, .5)

  # Fit causal forest
  cf <- grf::causal_forest(X = X, Y = Y, W = W, num.trees = 100)

  # Run vimp_causal_forests function
  vimp_scores <- cf_loco(cf)

  # Check that the sum of variable importance scores is 1
  expect_equal(sum(vimp_scores), 1, tolerance = 1e-8)
})


