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

test_that("gates() returns cf_gates with grf subgroup ATEs", {
  cf <- make_hetero_cf()
  g <- gates(cf, monotonize = FALSE)

  expect_s3_class(g, "cf_gates")
  expect_equal(g$K, 4L)
  expect_setequal(g$groups$group, paste0("G", 1:4))
  expect_true(all(is.finite(g$groups$estimate)))
  expect_true(all(c("p.left.adj", "p.right.adj") %in% names(g$groups)))
  expect_equal(g$groups$lower, g$groups$estimate - stats::qnorm(0.975) * g$groups$std.err)
  expect_equal(g$diff$group, "G4-G1")
  expect_equal(g$diff$estimate,
               g$groups$estimate[4] - g$groups$estimate[1])
})

test_that("monotonize keeps diff consistent with displayed groups", {
  set.seed(42)
  cf <- make_hetero_cf(n = 500, seed = 42)
  g <- gates(cf, monotonize = TRUE)
  expect_equal(g$diff$estimate,
               g$groups$estimate[nrow(g$groups)] - g$groups$estimate[1])
})

test_that("gates() rejects non-0/1 binary treatment", {
  cf <- make_hetero_cf(n = 200)
  cf$W.orig <- sample(c(0, 2), nrow(cf$X.orig), replace = TRUE)
  expect_error(gates(cf), "coded as 0 and 1")
})

test_that("gates() rejects invalid quantile.cutoffs", {
  cf <- make_hetero_cf()
  expect_error(gates(cf, quantile.cutoffs = c(0.5, 0.25)), "strictly increasing")
  expect_error(gates(cf, quantile.cutoffs = c(0, 0.5)), "values in \\(0, 1\\)")
})

test_that("gates() rejects degenerate least-anchor contrast", {
  cf <- make_hetero_cf()
  expect_error(
    gates(cf, subtract.from = "least", subtracted = 1),
    "G1-G1 is degenerate"
  )
})

test_that("cross.fit path runs and sets eval.n", {
  cf <- make_hetero_cf()
  g <- gates(cf, cross.fit = TRUE, seed = 1995)
  expect_true(g$cross.fit)
  expect_equal(g$eval.n, floor(nrow(cf$X.orig) / 2))
})

test_that("print.cf_gates runs without error", {
  cf <- make_hetero_cf()
  g <- gates(cf)
  expect_output(print(g), "cross.fit")
})

test_that("plot_gates returns utopia_plot when ggplot2 available", {
  skip_if_not_installed("ggplot2")
  cf <- make_hetero_cf()
  g <- gates(cf)
  p <- plot_gates(g, groups = c("G1", "G4", "G4-G1"))
  expect_s3_class(p, "utopia_plot")
})

test_that("T-API: gates formals unchanged", {
  fn <- names(formals(gates))
  expect_setequal(
    fn,
    c("c.forest", "quantile.cutoffs", "HT", "cross.fit", "subtract.from",
      "subtracted", "monotonize", "conf.level", "seed")
  )
})

test_that("T-API: plot_gates formals unchanged", {
  fn <- names(formals(plot_gates))
  expect_setequal(fn, c("x", "groups", "show.ate", "y.limits", "title", "..."))
})
