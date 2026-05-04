library(testthat)
library(grf)
library(UtopiaPlanitia)

# ---------------------------------------------------------------------------
# Tests for glmcf(): cv.glmnet-nuisance causal forest.
#
# All cv.glmnet-using tests are gated by skip_if_not_installed("glmnet").
# Helper / signature / dispatch tests don't need glmnet and run everywhere.
# ---------------------------------------------------------------------------

make_small_cf <- function(n = 80, p = 4, num.trees = 50,
                          seed = 1995, w_kind = c("binary", "continuous")) {
  w_kind <- match.arg(w_kind)
  set.seed(seed)
  X <- matrix(stats::rnorm(n * p), n, p)
  colnames(X) <- paste0("X", seq_len(p))
  W <- if (w_kind == "binary") stats::rbinom(n, 1, 0.5) else stats::rnorm(n)
  Y <- X[, 1] * (if (w_kind == "binary") W else (W > 0)) + stats::rnorm(n)
  grf::causal_forest(X, Y, W, num.trees = num.trees)
}

# --- signature & top-level validation (no glmnet required) -------------------

test_that("glmcf signature exposes expected formals and defaults", {
  f <- formals(glmcf)
  expect_true(all(c("c.forest", "X", "Y", "W", "K", "seed", "eps",
                    "tuning", "alpha", "s", "relax", "glmnet_args",
                    "verbose") %in% names(f)))
  expect_equal(eval(f$K), 5L)
  expect_equal(eval(f$seed), 1995L)
  expect_equal(eval(f$eps), 1e-3)
  expect_equal(eval(f$alpha), c(0, 0.25, 0.5, 0.75, 1))
  expect_equal(eval(f$s), "lambda.min")
  expect_true(eval(f$relax))
})

test_that("glmcf rejects non-causal_forest input", {
  expect_error(glmcf("not a forest"), "causal_forest")
})

test_that("glmcf rejects invalid eps", {
  cf <- make_small_cf(n = 40, w_kind = "binary")
  expect_error(glmcf(cf, eps = 0),    "eps")
  expect_error(glmcf(cf, eps = 0.5),  "eps")
  expect_error(glmcf(cf, eps = -0.1), "eps")
  expect_error(glmcf(cf, eps = c(0.1, 0.2)), "eps")
})

test_that("glmcf rejects invalid alpha", {
  cf <- make_small_cf(n = 40, w_kind = "binary")
  expect_error(glmcf(cf, alpha = -0.1), "alpha")
  expect_error(glmcf(cf, alpha = 1.5),  "alpha")
  expect_error(glmcf(cf, alpha = c(0.5, NA)), "alpha")
  expect_error(glmcf(cf, alpha = "lasso"), "alpha")
})

test_that("glmcf rejects invalid relax", {
  cf <- make_small_cf(n = 40, w_kind = "binary")
  expect_error(glmcf(cf, relax = "yes"), "relax")
  expect_error(glmcf(cf, relax = c(TRUE, FALSE)), "relax")
  expect_error(glmcf(cf, relax = NA), "relax")
})

test_that("glmcf rejects non-integer K", {
  cf <- make_small_cf(n = 40, w_kind = "binary")
  expect_error(glmcf(cf, K = 2.5), "integer")
  expect_error(glmcf(cf, K = 1),   "integer")
  expect_error(glmcf(cf, K = c(2, 3)), "integer")
})

test_that("glmcf errors on NA in X / Y / W", {
  cf <- make_small_cf(n = 40, w_kind = "binary")
  X  <- cf$X.orig; Y <- cf$Y.orig; W <- cf$W.orig

  X_na <- X; X_na[1, 1] <- NA_real_
  expect_error(glmcf(cf, X = X_na, Y = Y, W = W), "NA")

  Y_na <- Y; Y_na[1] <- NA_real_
  expect_error(glmcf(cf, X = X, Y = Y_na, W = W), "NA")

  W_na <- W; W_na[1] <- NA
  expect_error(glmcf(cf, X = X, Y = Y, W = W_na), "NA")
})

test_that("glmcf errors on size-mismatched X/Y/W overrides", {
  cf <- make_small_cf(n = 40, w_kind = "binary")
  X  <- cf$X.orig; Y <- cf$Y.orig; W <- cf$W.orig
  expect_error(glmcf(cf, X = X, Y = Y[-1], W = W), "matching number")
  expect_error(glmcf(cf, X = X, Y = Y, W = W[-1]), "matching number")
})

# --- .glmcf_w_type ----------------------------------------------------------

test_that(".glmcf_w_type detects binary / continuous correctly", {
  f <- UtopiaPlanitia:::.glmcf_w_type
  expect_equal(f(factor(c("a", "b", "a", "b"))), "binary")
  expect_equal(f(c(TRUE, FALSE, TRUE)), "binary")
  expect_equal(f(c(0, 1, 1, 0)), "binary")
  expect_equal(f(c(0L, 1L, 1L, 0L)), "binary")
  expect_silent(out <- f(c(0, 1, 2, 1)))
  expect_equal(out, "continuous")
  expect_error(f(c(0, 0, 0)), "constant")
})

test_that(".glmcf_w_type warns on 2 unique non-{0,1} numeric values", {
  f <- UtopiaPlanitia:::.glmcf_w_type
  expect_warning(out <- f(c(2, 5, 5, 2)), "0, 1")
  expect_equal(out, "continuous")
})

test_that(".glmcf_w_type errors on factor with != 2 levels", {
  f <- UtopiaPlanitia:::.glmcf_w_type
  expect_error(f(factor(c("a", "b", "c"))), "2 levels")
  expect_error(f(factor("a")),              "2 levels")
})

# --- .glmcf_make_folds ------------------------------------------------------

test_that(".glmcf_make_folds is deterministic given seed", {
  mk <- UtopiaPlanitia:::.glmcf_make_folds
  set.seed(42); a <- mk(n = 30, K = 3)
  set.seed(42); b <- mk(n = 30, K = 3)
  expect_identical(a, b)
})

test_that(".glmcf_make_folds is cluster-respecting", {
  mk <- UtopiaPlanitia:::.glmcf_make_folds
  clusters <- rep(seq_len(6), each = 5)
  set.seed(99)
  fold <- mk(n = 30, K = 3, clusters = clusters)
  by_cl <- split(fold, clusters)
  for (cl in by_cl) expect_equal(length(unique(cl)), 1L)
})

test_that(".glmcf_make_folds errors on too-few clusters for K", {
  mk <- UtopiaPlanitia:::.glmcf_make_folds
  clusters <- rep(c(1, 2), each = 10)
  expect_error(mk(n = 20, K = 5, clusters = clusters), "cluster")
})

# --- .glmcf_clip_propensity -------------------------------------------------

test_that(".glmcf_clip_propensity clips both tails and counts correctly", {
  clip <- UtopiaPlanitia:::.glmcf_clip_propensity
  W.hat <- c(-0.1, 0, 0.0005, 0.5, 0.9999, 1, 1.5)
  out <- clip(W.hat, eps = 1e-3, active = TRUE)
  expect_equal(out$clipped, 6L)
  expect_true(all(out$W.hat >= 1e-3))
  expect_true(all(out$W.hat <= 1 - 1e-3))
  expect_equal(out$W.hat[4], 0.5)
})

test_that(".glmcf_clip_propensity is a no-op when active=FALSE", {
  clip <- UtopiaPlanitia:::.glmcf_clip_propensity
  W.hat <- c(-2, 0.5, 3)
  out <- clip(W.hat, eps = 1e-3, active = FALSE)
  expect_identical(out$W.hat, W.hat)
  expect_equal(out$clipped, 0L)
})

# --- .glmcf_to_binary -------------------------------------------------------

test_that(".glmcf_to_binary maps factor/logical/numeric to 0/1", {
  f <- UtopiaPlanitia:::.glmcf_to_binary
  expect_equal(f(factor(c("a", "b", "a"))), c(0L, 1L, 0L))
  expect_equal(f(c(TRUE, FALSE, TRUE)),     c(1L, 0L, 1L))
  expect_equal(f(c(0, 1, 1, 0)),            c(0,  1,  1,  0))
  expect_error(f(factor(c("a", "b", "c"))), "2 levels")
  expect_error(f(list(1, 2)), "Unsupported")
})

# --- glmnet-gated end-to-end tests -----------------------------------------

test_that("glmcf cross-fits cv.glmnet nuisances and returns a causal_forest (binary W)", {
  skip_if_not_installed("glmnet")
  cf  <- make_small_cf(n = 80, w_kind = "binary")
  cf2 <- suppressWarnings(glmcf(cf, K = 2))

  expect_s3_class(cf2, "causal_forest")
  expect_length(cf2$predictions, nrow(cf$X.orig))

  meta <- attr(cf2, "glmcf_meta")
  expect_equal(meta$K, 2L)
  expect_equal(meta$w_type, "binary")
  expect_equal(meta$eps, 1e-3)
  expect_length(meta$alpha_y, 2L)
  expect_length(meta$alpha_w, 2L)

  expect_false(isTRUE(all.equal(cf$Y.hat, cf2$Y.hat)))
  expect_false(isTRUE(all.equal(cf$W.hat, cf2$W.hat)))
})

test_that("glmcf handles continuous W (gaussian for propensity)", {
  skip_if_not_installed("glmnet")
  cf  <- make_small_cf(n = 80, w_kind = "continuous")
  cf2 <- suppressWarnings(glmcf(cf, K = 2))

  meta <- attr(cf2, "glmcf_meta")
  expect_equal(meta$w_type, "continuous")
  expect_equal(meta$clipped, 0L)
  expect_true(all(is.finite(cf2$W.hat)))
})

test_that("glmcf result is compatible with downstream methods", {
  skip_if_not_installed("glmnet")
  cf  <- make_small_cf(n = 80, w_kind = "binary")
  cf2 <- suppressWarnings(glmcf(cf, K = 2))

  expect_no_error(suppressWarnings(suppressMessages(summary(cf2))))
  pr <- stats::predict(cf2)
  expect_true(is.data.frame(pr) || is.list(pr))
  expect_length(pr$predictions, nrow(cf$X.orig))
})

test_that("glmcf accepts custom eps and propagates to meta", {
  skip_if_not_installed("glmnet")
  cf  <- make_small_cf(n = 60, w_kind = "binary")
  cf2 <- suppressWarnings(glmcf(cf, K = 2, eps = 0.05))
  meta <- attr(cf2, "glmcf_meta")
  expect_equal(meta$eps, 0.05)
  expect_true(all(cf2$W.hat >= 0.05))
  expect_true(all(cf2$W.hat <= 0.95))
})

test_that("glmcf with single alpha skips alpha tuning", {
  skip_if_not_installed("glmnet")
  cf  <- make_small_cf(n = 60, w_kind = "binary")
  cf2 <- suppressWarnings(glmcf(cf, K = 2, alpha = 1))
  meta <- attr(cf2, "glmcf_meta")
  expect_equal(meta$alpha_grid, 1)
  expect_true(all(meta$alpha_y == 1))
  expect_true(all(meta$alpha_w == 1))
})

test_that("glmcf with relax=FALSE matches non-relaxed cv.glmnet path", {
  skip_if_not_installed("glmnet")
  cf  <- make_small_cf(n = 60, w_kind = "binary")
  cf2 <- suppressWarnings(glmcf(cf, K = 2, relax = FALSE))
  meta <- attr(cf2, "glmcf_meta")
  expect_false(meta$relax)
})

# --- tuning argument and .glmcf_build_tune_args -----------------------------

test_that("glmcf rejects invalid tuning via match.arg", {
  cf <- make_small_cf(n = 40, w_kind = "binary")
  expect_error(glmcf(cf, tuning = "nope"), "should be one of")
  expect_error(glmcf(cf, tuning = "ORIG"), "should be one of")
})

test_that(".glmcf_build_tune_args returns inherited params for 'orig'", {
  build <- UtopiaPlanitia:::.glmcf_build_tune_args
  cf <- make_small_cf(n = 40, w_kind = "binary")
  out <- build(cf, "orig")
  expect_setequal(
    names(out),
    c("sample.fraction", "mtry", "min.node.size", "honesty.fraction",
      "honesty.prune.leaves", "alpha", "imbalance.penalty")
  )
  expect_false("tune.parameters" %in% names(out))
})

test_that(".glmcf_build_tune_args returns empty for 'cf.default'", {
  build <- UtopiaPlanitia:::.glmcf_build_tune_args
  cf <- make_small_cf(n = 40, w_kind = "binary")
  expect_length(build(cf, "cf.default"), 0L)
})

test_that(".glmcf_build_tune_args returns tune.parameters='all' for 'cf.autotune'", {
  build <- UtopiaPlanitia:::.glmcf_build_tune_args
  cf <- make_small_cf(n = 40, w_kind = "binary")
  out <- build(cf, "cf.autotune")
  expect_equal(out$tune.parameters, "all")
})

test_that(".glmcf_build_tune_args degrades gracefully when tunable.params is NULL", {
  build <- UtopiaPlanitia:::.glmcf_build_tune_args
  fake <- structure(list(tunable.params = NULL), class = "causal_forest")
  expect_length(build(fake, "orig"), 0L)
})
