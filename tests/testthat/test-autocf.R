library(testthat)
library(grf)
library(UtopiaPlanitia)

# ---------------------------------------------------------------------------
# Tests for autocf(): auto-selected nuisance causal forest.
#
# Helper / signature / dispatch tests don't need any optional dep and run
# everywhere. End-to-end tests are gated by skip_if_not_installed for the
# specific candidate dep stack.
# ---------------------------------------------------------------------------

make_small_cf <- function(n = 80, p = 4, num.trees = 50,
                          seed = 1995, w_kind = c("binary", "continuous"),
                          weights = NULL) {
  w_kind <- match.arg(w_kind)
  set.seed(seed)
  X <- matrix(stats::rnorm(n * p), n, p)
  colnames(X) <- paste0("X", seq_len(p))
  W <- if (w_kind == "binary") stats::rbinom(n, 1, 0.5) else stats::rnorm(n)
  Y <- X[, 1] * (if (w_kind == "binary") W else (W > 0)) + stats::rnorm(n)
  args <- list(X = X, Y = Y, W = W, num.trees = num.trees)
  if (!is.null(weights)) args$sample.weights <- weights
  do.call(grf::causal_forest, args)
}

# --- signature & top-level validation (no optional deps required) ---------

test_that("autocf signature exposes expected formals and defaults", {
  f <- formals(autocf)
  expect_true(all(c("c.forest", "X", "Y", "W", "K", "seed", "eps", "tuning",
                    "pool", "min_improvement", "term_evals", "tabpfn_args",
                    "glmnet_args", "xgboost_args", "bart_args",
                    "verbose") %in% names(f)))
  expect_equal(eval(f$K), 5L)
  expect_equal(eval(f$seed), 1995L)
  expect_equal(eval(f$eps), 1e-3)
  expect_equal(eval(f$pool), c("grf", "glmnet", "xgboost", "tabpfn", "bart"))
  expect_equal(eval(f$min_improvement), "1se")
  expect_equal(eval(f$term_evals), 10L)
})

test_that("autocf rejects non-causal_forest input", {
  expect_error(autocf("not a forest"), "causal_forest")
})

test_that("autocf rejects invalid eps", {
  cf <- make_small_cf(n = 40, w_kind = "binary")
  expect_error(autocf(cf, eps = 0,    pool = "grf"), "eps")
  expect_error(autocf(cf, eps = 0.5,  pool = "grf"), "eps")
  expect_error(autocf(cf, eps = -0.1, pool = "grf"), "eps")
})

test_that("autocf rejects pool without 'grf' baseline", {
  cf <- make_small_cf(n = 40, w_kind = "binary")
  expect_error(autocf(cf, pool = c("glmnet", "xgboost")), "baseline")
})

test_that("autocf rejects unknown candidate names in pool", {
  cf <- make_small_cf(n = 40, w_kind = "binary")
  expect_error(autocf(cf, pool = c("grf", "ranger")), "Unknown candidate")
})

test_that("autocf rejects invalid term_evals", {
  cf <- make_small_cf(n = 40, w_kind = "binary")
  expect_error(autocf(cf, term_evals = -1, pool = "grf"), "term_evals")
  expect_error(autocf(cf, term_evals = 1.5, pool = "grf"), "term_evals")
})

test_that("autocf rejects invalid min_improvement", {
  cf <- make_small_cf(n = 40, w_kind = "binary")
  expect_error(autocf(cf, min_improvement = "nope", pool = "grf"),
               "min_improvement")
  expect_error(autocf(cf, min_improvement = -0.5, pool = "grf"),
               "min_improvement")
  expect_error(autocf(cf, min_improvement = c(0.1, 0.2), pool = "grf"),
               "min_improvement")
})

test_that("autocf rejects non-integer K", {
  cf <- make_small_cf(n = 40, w_kind = "binary")
  expect_error(autocf(cf, K = 2.5, pool = "grf"), "integer")
  expect_error(autocf(cf, K = 1, pool = "grf"),   "integer")
})

test_that("autocf errors on NA in Y / W but tolerates NA in X", {
  cf <- make_small_cf(n = 40, w_kind = "binary")
  X  <- cf$X.orig; Y <- cf$Y.orig; W <- cf$W.orig
  X_na <- X; X_na[1, 1] <- NA_real_
  # NA in X is tolerated by NA-safe candidates (e.g. grf via MIA)
  expect_no_error(
    suppressMessages(autocf(cf, X = X_na, Y = Y, W = W, pool = "grf",
                            K = 3L, term_evals = 0L))
  )
  Y_na <- Y; Y_na[1] <- NA_real_
  expect_error(autocf(cf, X = X, Y = Y_na, W = W, pool = "grf"), "NA")
  W_na <- W; W_na[1] <- NA
  expect_error(autocf(cf, X = X, Y = Y, W = W_na, pool = "grf"), "NA")
})

test_that("autocf glmnet candidate auto-imputes NAs in X via makeX", {
  skip_if_not_installed("glmnet")
  cf <- make_small_cf(n = 80, w_kind = "binary")
  X  <- cf$X.orig; Y <- cf$Y.orig; W <- cf$W.orig
  X_na <- X
  X_na[cbind(c(1, 5, 9, 13), c(1, 2, 1, 2))] <- NA_real_
  expect_message(
    out <- autocf(cf, X = X_na, Y = Y, W = W,
                  pool = c("grf", "glmnet"),
                  K = 3L, term_evals = 0L),
    "glmnet.*makeX"
  )
  expect_s3_class(out, "causal_forest")
})

# --- .autocf_w_type / .autocf_make_folds / .autocf_clip_propensity --------

test_that(".autocf_w_type detects binary / continuous correctly", {
  f <- UtopiaPlanitia:::.autocf_w_type
  expect_equal(f(factor(c("a", "b", "a", "b"))), "binary")
  expect_equal(f(c(TRUE, FALSE, TRUE)), "binary")
  expect_equal(f(c(0, 1, 1, 0)), "binary")
  expect_silent(out <- f(c(0, 1, 2, 1)))
  expect_equal(out, "continuous")
  expect_error(f(c(0, 0, 0)), "constant")
})

test_that(".autocf_make_folds is deterministic and cluster-respecting", {
  mk <- UtopiaPlanitia:::.autocf_make_folds
  set.seed(42); a <- mk(n = 30, K = 3)
  set.seed(42); b <- mk(n = 30, K = 3)
  expect_identical(a, b)

  clusters <- rep(seq_len(6), each = 5)
  set.seed(99)
  fold <- mk(n = 30, K = 3, clusters = clusters)
  by_cl <- split(fold, clusters)
  for (cl in by_cl) expect_equal(length(unique(cl)), 1L)
})

test_that(".autocf_clip_propensity clips both tails", {
  clip <- UtopiaPlanitia:::.autocf_clip_propensity
  out <- clip(c(-0.1, 0.5, 1.2), eps = 1e-3, active = TRUE)
  expect_equal(out$clipped, 2L)
  expect_true(all(out$W.hat >= 1e-3 & out$W.hat <= 1 - 1e-3))
})

# --- .autocf_validate_min_improvement -------------------------------------

test_that(".autocf_validate_min_improvement accepts valid input", {
  f <- UtopiaPlanitia:::.autocf_validate_min_improvement
  expect_equal(f("1se"), "1se")
  expect_equal(f("0"), "0")
  expect_equal(f(0.05), 0.05)
  expect_equal(f(0L), 0)
})

# --- .autocf_select : unit-test the swap rule directly --------------------

test_that(".autocf_select retains baseline when nothing beats it", {
  scores <- list(
    summary = data.frame(
      candidate = c("grf", "glmnet"),
      y_loss = c(1.0, 1.05), y_se = c(0.05, 0.05),
      w_loss = c(0.20, 0.21), w_se = c(0.01, 0.01),
      stringsAsFactors = FALSE
    ),
    per_fold = data.frame(
      candidate = rep(c("grf", "glmnet"), each = 3),
      fold = rep(1:3, 2),
      y_loss = c(1.0, 1.0, 1.0, 1.05, 1.05, 1.05),
      w_loss = c(0.20, 0.20, 0.20, 0.21, 0.21, 0.21),
      stringsAsFactors = FALSE
    )
  )
  sel <- UtopiaPlanitia:::.autocf_select(scores, "grf", "1se")
  expect_equal(sel$winner_y, "grf")
  expect_equal(sel$winner_w, "grf")
  expect_false(sel$swap_y)
  expect_false(sel$swap_w)
})

test_that(".autocf_select swaps with min_improvement = '0' on any improvement", {
  scores <- list(
    summary = data.frame(
      candidate = c("grf", "glmnet"),
      y_loss = c(1.0, 0.99), y_se = c(0.5, 0.5),  # huge SE
      w_loss = c(0.20, 0.19), w_se = c(0.05, 0.05),
      stringsAsFactors = FALSE
    ),
    per_fold = data.frame(
      candidate = rep(c("grf", "glmnet"), each = 3),
      fold = rep(1:3, 2),
      y_loss = c(1.0, 1.0, 1.0, 0.99, 0.99, 0.99),
      w_loss = c(0.20, 0.20, 0.20, 0.19, 0.19, 0.19),
      stringsAsFactors = FALSE
    )
  )
  sel <- UtopiaPlanitia:::.autocf_select(scores, "grf", "0")
  expect_equal(sel$winner_y, "glmnet")
  expect_true(sel$swap_y)
})

test_that(".autocf_select with '1se' rule blocks noise-driven swaps", {
  # Per-fold differences are positive but with high variance -> 1-SE blocks.
  scores <- list(
    summary = data.frame(
      candidate = c("grf", "glmnet"),
      y_loss = c(1.0, 0.95), y_se = c(0.1, 0.1),
      w_loss = c(0.20, 0.20), w_se = c(0.01, 0.01),
      stringsAsFactors = FALSE
    ),
    per_fold = data.frame(
      candidate = rep(c("grf", "glmnet"), each = 3),
      fold = rep(1:3, 2),
      # diff = (0.6, 0.0, -0.45); mean = 0.05; SE = sd/sqrt(3) much > 0.05
      y_loss = c(1.5, 1.0, 0.5, 0.9, 1.0, 0.95),
      w_loss = c(0.20, 0.20, 0.20, 0.20, 0.20, 0.20),
      stringsAsFactors = FALSE
    )
  )
  sel <- UtopiaPlanitia:::.autocf_select(scores, "grf", "1se")
  expect_equal(sel$winner_y, "grf")
  expect_false(sel$swap_y)
})

# --- end-to-end (gated by deps) -------------------------------------------

test_that("autocf with pool='grf' only is a no-op-style sanity run (binary W)", {
  cf  <- make_small_cf(n = 80, w_kind = "binary")
  cf2 <- autocf(cf, K = 2, pool = "grf")
  expect_s3_class(cf2, "causal_forest")
  expect_length(cf2$predictions, nrow(cf$X.orig))
  m <- attr(cf2, "autocf_meta")
  expect_equal(m$pool_run, "grf")
  expect_equal(m$winner_y, "grf")
  expect_equal(m$winner_w, "grf")
  expect_false(m$swap_y); expect_false(m$swap_w)
})

test_that("autocf with grf+glmnet runs end-to-end and reports comparison", {
  skip_if_not_installed("glmnet")
  cf  <- make_small_cf(n = 80, w_kind = "binary")
  cf2 <- suppressWarnings(autocf(cf, K = 2, pool = c("grf", "glmnet")))
  m <- attr(cf2, "autocf_meta")
  expect_setequal(m$pool_run, c("grf", "glmnet"))
  expect_setequal(unique(m$scores$candidate), c("grf", "glmnet"))
  expect_true(all(c("y_loss", "y_se", "w_loss", "w_se") %in%
                  names(m$scores)))
  expect_length(m$winner_y, 1L)
  expect_length(m$winner_w, 1L)
})

test_that("autocf handles continuous W (gaussian for both Y and W nuisances)", {
  skip_if_not_installed("glmnet")
  cf  <- make_small_cf(n = 80, w_kind = "continuous")
  cf2 <- suppressWarnings(autocf(cf, K = 2, pool = c("grf", "glmnet")))
  m <- attr(cf2, "autocf_meta")
  expect_equal(m$w_type, "continuous")
  expect_equal(m$clipped, 0L)
  expect_true(all(is.finite(cf2$W.hat)))
})

test_that("autocf result is compatible with downstream summary()", {
  skip_if_not_installed("glmnet")
  cf  <- make_small_cf(n = 80, w_kind = "binary")
  cf2 <- suppressWarnings(autocf(cf, K = 2, pool = c("grf", "glmnet")))
  expect_no_error(suppressWarnings(suppressMessages(summary(cf2))))
})

test_that("autocf with grf+bart runs end-to-end (binary W)", {
  skip_if_not_installed("dbarts")
  cf  <- make_small_cf(n = 80, w_kind = "binary")
  cf2 <- suppressWarnings(autocf(
    cf, K = 2, pool = c("grf", "bart"),
    bart_args = list(ndpost = 100L, nskip = 50L, ntree = 50L)
  ))
  m <- attr(cf2, "autocf_meta")
  expect_setequal(m$pool_run, c("grf", "bart"))
  expect_true(all(is.finite(cf2$Y.hat)))
  expect_true(all(is.finite(cf2$W.hat)))
  # binary W: predicted propensities live in [eps, 1-eps]
  expect_true(all(cf2$W.hat > 0 & cf2$W.hat < 1))
})

test_that("autocf with grf+bart runs end-to-end (continuous W)", {
  skip_if_not_installed("dbarts")
  cf  <- make_small_cf(n = 80, w_kind = "continuous")
  cf2 <- suppressWarnings(autocf(
    cf, K = 2, pool = c("grf", "bart"),
    bart_args = list(ndpost = 100L, nskip = 50L, ntree = 50L)
  ))
  m <- attr(cf2, "autocf_meta")
  expect_equal(m$w_type, "continuous")
  expect_true(all(is.finite(cf2$Y.hat)))
  expect_true(all(is.finite(cf2$W.hat)))
})

test_that("autocf drops bart from pool when dbarts is missing", {
  skip_if(requireNamespace("dbarts", quietly = TRUE),
          "dbarts is installed; cannot exercise the missing-dep branch.")
  cf <- make_small_cf(n = 60, w_kind = "binary")
  expect_warning(
    cf2 <- autocf(cf, K = 2, pool = c("grf", "bart")),
    "bart"
  )
  m <- attr(cf2, "autocf_meta")
  expect_false("bart" %in% m$pool_run)
})

test_that("autocf drops tabpfn from pool when sample.weights are non-trivial", {
  skip_if_not_installed("tabpfn")
  skip_if(!nzchar(Sys.getenv("TABPFN_TOKEN")),
          "TABPFN_TOKEN not set")
  set.seed(1)
  n <- 80; p <- 4
  w <- runif(n, 0.5, 1.5)  # non-trivial weights
  cf <- make_small_cf(n = n, p = p, w_kind = "binary", weights = w)
  expect_warning(
    cf2 <- autocf(cf, K = 2, pool = c("grf", "tabpfn")),
    "tabpfn"
  )
  m <- attr(cf2, "autocf_meta")
  expect_false("tabpfn" %in% m$pool_run)
})
