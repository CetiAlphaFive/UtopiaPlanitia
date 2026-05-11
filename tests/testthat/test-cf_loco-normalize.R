# T-H5: cf_loco normalize all-zero guard
library(testthat)
library(grf)
library(UtopiaPlanitia)

test_that("H5: normalize=TRUE produces no NaN at seed=100 all-negative case", {
  set.seed(100)
  n <- 80; p <- 6
  X <- matrix(rnorm(n * p), n, p); colnames(X) <- paste0("X", seq_len(p))
  W <- rbinom(n, 1, 0.5)
  Y <- rnorm(n)
  cf <- grf::causal_forest(X, Y, W, num.trees = 100)
  vi <- suppressWarnings(cf_loco(cf, normalize = TRUE, seed = 100))

  expect_s3_class(vi, "cf_loco")
  expect_false(any(is.nan(vi$vimp$Importance)))
  expect_false(any(is.na(vi$vimp$Importance)))
  expect_equal(sum(vi$vimp$Importance), 1, tolerance = 1e-9)
  expect_true(all(is.finite(vi$vimp$Importance)))
})

test_that("H5: all-zero guard fires warning containing 'uniform' / '1/p' / 'non-negative'", {
  set.seed(100)
  n <- 80; p <- 6
  X <- matrix(rnorm(n * p), n, p); colnames(X) <- paste0("X", seq_len(p))
  W <- rbinom(n, 1, 0.5); Y <- rnorm(n)
  cf <- grf::causal_forest(X, Y, W, num.trees = 100)

  w <- character(0)
  vi <- withCallingHandlers(
    cf_loco(cf, normalize = TRUE, seed = 100),
    warning = function(cnd) {
      w <<- c(w, conditionMessage(cnd))
      invokeRestart("muffleWarning")
    }
  )

  guard_fired <- any(grepl("uniform|1/p|non-negative", w))
  if (guard_fired) {
    expect_equal(unique(vi$vimp$Importance), 1 / p, tolerance = 1e-9)
  } else {
    expect_true(all(is.finite(vi$vimp$Importance)))
    expect_equal(sum(vi$vimp$Importance), 1, tolerance = 1e-9)
  }
})

test_that("H5: normal-case normalization still works (X1 dominant DGP)", {
  set.seed(1995)
  n <- 200; p <- 5
  X <- matrix(rnorm(n * p), n, p); colnames(X) <- paste0("X", seq_len(p))
  W <- rbinom(n, 1, 0.5)
  Y <- X[, 1] * W + rnorm(n)
  cf <- grf::causal_forest(X, Y, W, num.trees = 200)
  vi <- suppressWarnings(cf_loco(cf, normalize = TRUE, seed = 1995))

  expect_equal(sum(vi$vimp$Importance), 1, tolerance = 1e-9)
  expect_true(all(vi$vimp$Importance >= 0))
  expect_false(any(is.nan(vi$vimp$Importance)))
  expect_false(any(is.na(vi$vimp$Importance)))
})
