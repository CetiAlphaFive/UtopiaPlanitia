library(testthat)
library(grf)
library(UtopiaPlanitia)

make_hetero_cf <- function(n = 400, seed = 1995) {
  set.seed(seed)
  p <- 5
  X <- matrix(stats::rnorm(n * p), n, p)
  colnames(X) <- paste0("X", seq_len(p))
  W <- stats::rbinom(n, 1, 0.5)
  Y <- X[, 1] * W + stats::rnorm(n)
  grf::causal_forest(X, Y, W, num.trees = 200)
}

test_that("gates() returns cf_gates with K quartile groups", {
  cf <- make_hetero_cf()
  g <- gates(cf)

  expect_s3_class(g, "cf_gates")
  expect_equal(g$K, 4L)
  expect_equal(nrow(g$groups), 4L)
  expect_setequal(g$groups$group, paste0("G", 1:4))
  expect_true(all(is.finite(g$groups$estimate)))
  expect_equal(g$groups$lower, g$groups$estimate - stats::qnorm(0.975) * g$groups$std.err)
  expect_equal(g$groups$upper, g$groups$estimate + stats::qnorm(0.975) * g$groups$std.err)
  expect_equal(nrow(g$diff), 1L)
  expect_equal(g$diff$group, "G4-G1")
  expect_type(g$ate$estimate, "double")
})

test_that("gates() rejects non-binary treatment", {
  cf <- make_hetero_cf(n = 200)
  cf$W.orig <- stats::runif(nrow(cf$X.orig))
  expect_error(gates(cf), "binary treatment", ignore.case = TRUE)
})

test_that("gates() rejects invalid quantile.cutoffs", {
  cf <- make_hetero_cf()
  expect_error(gates(cf, quantile.cutoffs = c(0.5, 0.25)), "strictly increasing")
  expect_error(gates(cf, quantile.cutoffs = c(0, 0.5)), "values in \\(0, 1\\)")
})

test_that("print.cf_gates runs without error", {
  cf <- make_hetero_cf()
  g <- gates(cf)
  expect_output(print(g), "Sorted Group Average Treatment Effects")
})

test_that("plot_gates returns utopia_plot when ggplot2 available", {
  skip_if_not_installed("ggplot2")
  cf <- make_hetero_cf()
  g <- gates(cf)
  p <- plot_gates(g, groups = c("G1", "G4", "G4-G1"))
  expect_s3_class(p, "utopia_plot")
})

test_that("plot_gates errors on unknown group labels", {
  skip_if_not_installed("ggplot2")
  cf <- make_hetero_cf()
  g <- gates(cf)
  expect_error(plot_gates(g, groups = "G9"), "Unknown group label")
})

test_that("T-API: gates formals unchanged", {
  fn <- names(formals(gates))
  expect_setequal(
    fn,
    c("c.forest", "quantile.cutoffs", "HT", "subtract.from", "subtracted",
      "monotonize", "conf.level", "seed")
  )
})

test_that("T-API: plot_gates formals unchanged", {
  fn <- names(formals(plot_gates))
  expect_setequal(fn, c("x", "groups", "show.ate", "y.limits", "title", "..."))
})
