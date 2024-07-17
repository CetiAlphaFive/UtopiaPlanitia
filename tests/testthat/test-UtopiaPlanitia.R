library(testthat)
library(grf)
library(UtopiaPlanitia)

test_that("vis generated", {
  set.seed(1995) # Ensure reproducibility in tests

  # Generate synthetic data
  X <- matrix(rnorm(100*2,0,1),100,2)
  Y <- rnorm(100, 0, 1)
  W <- rbinom(100, 1, .5)

  # Fit causal forest
  cf <- grf::causal_forest(X = X, Y = Y, W = W, num.trees = 100)

  # Run vimp_causal_forests function
  vimp_scores <- cf_loco(cf,normalize = T)

  # Check that the sum of variable importance scores is 1
  expect_equal(sum(vimp_scores$Importance), 1, tolerance = 1e-8)
})

test_that("left perm runs", {
  set.seed(1995) # Ensure reproducibility in tests

  # Generate synthetic data
  X <- matrix(rnorm(100*5,0,1),100,5)
  Y <- rnorm(100, 0, 1)
  W <- rbinom(100, 1, .5)

  # Fit causal forest
  cf <- grf::causal_forest(X = X, Y = Y, W = W, num.trees = 100)

  # Run the lp function
  lp <- left_perm(cf,n_perm = 10,n_cv = 10)

  # Check that the sum of variable importance scores is 1
  expect_equal(lp$summary$Null_Mean[[1]], 0.1783335, tolerance = 1e-3)
})
