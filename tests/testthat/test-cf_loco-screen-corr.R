test_that("screen=TRUE drops only zero split-frequency covariates", {
  skip_if_not_installed("grf")
  set.seed(1995)
  n <- 300
  X <- matrix(rnorm(n * 4), n, 4)
  colnames(X) <- paste0("X", 1:4)
  X[, 4] <- 1                       # constant -> zero split frequency
  W <- rbinom(n, 1, 0.5)
  Y <- 2 * X[, 1] * W + rnorm(n)
  cf <- grf::causal_forest(X, Y, W, num.trees = 200)

  vi <- suppressMessages(cf_loco(cf, screen = TRUE, verbose = FALSE))
  expect_equal(nrow(vi$vimp), 4L)                                  # all vars present
  expect_equal(vi$vimp$Importance[vi$vimp$Variable == "X4"], 0)    # screened -> 0
})

test_that("screen=TRUE keeps all when every importance is zero", {
  skip_if_not_installed("grf")
  set.seed(7)
  n <- 200
  X <- matrix(1, n, 3)              # all exactly constant -> all zero importance
  colnames(X) <- paste0("X", 1:3)
  W <- rbinom(n, 1, 0.5)
  Y <- rnorm(n)
  cf <- grf::causal_forest(X, Y, W, num.trees = 100)
  expect_message(cf_loco(cf, screen = TRUE, verbose = FALSE), "keeping all")
})
