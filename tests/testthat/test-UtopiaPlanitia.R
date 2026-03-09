library(testthat)
library(grf)
library(UtopiaPlanitia)

make_cf <- function(n = 200, p = 5, num.trees = 100) {
  set.seed(1995)
  X <- matrix(rnorm(n * p), n, p)
  colnames(X) <- paste0("X", seq_len(p))
  W <- rbinom(n, 1, .5)
  Y <- X[, 1] * W + rnorm(n)
  grf::causal_forest(X, Y, W, num.trees = num.trees)
}

test_that("cf_loco returns cf_loco class with correct structure", {
  cf <- make_cf()
  result <- cf_loco(cf, normalize = TRUE)
  expect_s3_class(result, "cf_loco")
  expect_named(result, c("vimp", "normalized", "n", "p"))
  expect_true(result$normalized)
  expect_equal(sum(result$vimp$Importance), 1, tolerance = 1e-8)
})

test_that("print.cf_loco runs without error", {
  cf <- make_cf()
  result <- cf_loco(cf)
  expect_output(print(result), "LOCO Variable Importance")
})

test_that("summary.cf_loco runs without error", {
  cf <- make_cf()
  result <- cf_loco(cf)
  expect_output(summary(result), "LOCO Variable Importance")
})

test_that("plot.cf_loco returns a ggplot", {
  skip_if_not_installed("ggplot2")
  cf <- make_cf()
  result <- cf_loco(cf)
  p <- plot(result)
  expect_s3_class(p, "gg")
})

test_that("summary.causal_forest returns correct class and structure", {
  cf <- make_cf()
  s <- summary(cf)
  expect_s3_class(s, "summary.causal_forest")
  expect_named(s, c("ate", "vimp", "heterogeneity"))
  expect_length(s$ate, 2)
  expect_named(s$ate, c("estimate", "std.err"))
  expect_length(s$vimp, 5)
  expect_s3_class(s$heterogeneity, "data.frame")
})

test_that("print.summary.causal_forest runs without error", {
  cf <- make_cf()
  s <- summary(cf)
  expect_output(print(s), "Average Treatment Effect")
})

test_that("plot.causal_forest validates type argument", {
  cf <- make_cf()
  expect_error(plot(cf, type = "invalid"), "should be one of")
})

test_that("rank_plot returns a ggplot", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("ggdist")
  cf <- make_cf()
  p <- rank_plot(cf)
  expect_s3_class(p, "gg")
})
