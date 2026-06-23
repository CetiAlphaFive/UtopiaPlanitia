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

.corr_cf <- function() {
  set.seed(1995)
  n <- 300
  x1 <- rnorm(n)
  x2 <- x1 + rnorm(n, sd = 0.1)        # r ~ 0.99 with x1
  x3 <- rnorm(n)                       # independent
  X <- cbind(X1 = x1, X2 = x2, X3 = x3)
  W <- rbinom(n, 1, 0.5)
  Y <- x1 * W + rnorm(n)
  grf::causal_forest(X, Y, W, num.trees = 200)
}

test_that("verbose=TRUE warns on correlated conditioning variables", {
  skip_if_not_installed("grf")
  cf <- .corr_cf()
  expect_warning(suppressMessages(cf_loco(cf, verbose = TRUE)), "correlated")
})

test_that("verbose=FALSE suppresses the correlation warning", {
  skip_if_not_installed("grf")
  cf <- .corr_cf()
  expect_no_warning(cf_loco(cf, verbose = FALSE))
})

test_that("verbose=TRUE prints the correlation matrix", {
  skip_if_not_installed("grf")
  cf <- .corr_cf()
  expect_output(suppressWarnings(suppressMessages(cf_loco(cf, verbose = TRUE))), "X1")
})

test_that("grouping suppresses the correlation warning but keeps the matrix", {
  skip_if_not_installed("grf")
  cf <- .corr_cf()
  expect_no_warning(suppressMessages(cf_loco(cf, group.by.corr = TRUE, verbose = TRUE)))
})
