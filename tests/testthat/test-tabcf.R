library(testthat)
library(grf)
library(UtopiaPlanitia)

# ---------------------------------------------------------------------------
# Tests for tabcf(): TabPFN-nuisance causal forest.
#
# All TabPFN-using tests are gated by skip_if_not_installed("tabpfn") AND
# skip_if(!nzchar(Sys.getenv("TABPFN_TOKEN"))) so the suite stays green on
# machines without the Python TabPFN backend or without an accepted TabPFN
# license. Helper / signature / dispatch tests don't need TabPFN and run
# everywhere.
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

# --- signature & top-level validation (no tabpfn required) -------------------

test_that("tabcf signature exposes expected formals and defaults", {
  f <- formals(tabcf)
  expect_true(all(c("c.forest", "X", "Y", "W", "K", "seed", "eps",
                    "tabpfn_args", "verbose") %in% names(f)))
  expect_equal(eval(f$K), 5L)
  expect_equal(eval(f$seed), 1995L)
  expect_equal(eval(f$eps), 1e-3)
})

test_that("tabcf rejects non-causal_forest input", {
  expect_error(tabcf("not a forest"), "causal_forest")
})

test_that("tabcf rejects invalid eps", {
  cf <- make_small_cf(n = 40, w_kind = "binary")
  expect_error(tabcf(cf, eps = 0),    "eps")
  expect_error(tabcf(cf, eps = 0.5),  "eps")
  expect_error(tabcf(cf, eps = -0.1), "eps")
  expect_error(tabcf(cf, eps = c(0.1, 0.2)), "eps")
})

test_that("tabcf errors on NA in X / Y / W", {
  cf <- make_small_cf(n = 40, w_kind = "binary")
  X  <- cf$X.orig; Y <- cf$Y.orig; W <- cf$W.orig

  X_na <- X; X_na[1, 1] <- NA_real_
  expect_error(tabcf(cf, X = X_na, Y = Y, W = W), "NA")

  Y_na <- Y; Y_na[1] <- NA_real_
  expect_error(tabcf(cf, X = X, Y = Y_na, W = W), "NA")

  W_na <- W; W_na[1] <- NA
  expect_error(tabcf(cf, X = X, Y = Y, W = W_na), "NA")
})

test_that("tabcf errors on size-mismatched X/Y/W overrides", {
  cf <- make_small_cf(n = 40, w_kind = "binary")
  X  <- cf$X.orig; Y <- cf$Y.orig; W <- cf$W.orig
  expect_error(
    tabcf(cf, X = X, Y = Y[-1], W = W),
    "matching number"
  )
  expect_error(
    tabcf(cf, X = X, Y = Y, W = W[-1]),
    "matching number"
  )
})

# --- .tabcf_w_type ----------------------------------------------------------

test_that(".tabcf_w_type detects binary / continuous correctly", {
  f <- UtopiaPlanitia:::.tabcf_w_type

  # factor with 2 levels -> binary
  expect_equal(f(factor(c("a", "b", "a", "b"))), "binary")
  # logical -> binary
  expect_equal(f(c(TRUE, FALSE, TRUE)), "binary")
  # numeric {0, 1} -> binary
  expect_equal(f(c(0, 1, 1, 0)), "binary")
  # integer 0L/1L -> binary
  expect_equal(f(c(0L, 1L, 1L, 0L)), "binary")
  # numeric {0, 1, 2} -> continuous (no warning)
  expect_silent(out <- f(c(0, 1, 2, 1)))
  expect_equal(out, "continuous")
  # numeric {0} (single value) -> error (constant W)
  expect_error(f(c(0, 0, 0)), "constant")
})

test_that(".tabcf_w_type warns on 2 unique non-{0,1} numeric values", {
  f <- UtopiaPlanitia:::.tabcf_w_type
  expect_warning(out <- f(c(2, 5, 5, 2)), "0, 1")
  expect_equal(out, "continuous")

  expect_warning(out <- f(c(-1, 1, 1, -1)), "0, 1")
  expect_equal(out, "continuous")
})

test_that(".tabcf_w_type errors on factor with != 2 levels", {
  f <- UtopiaPlanitia:::.tabcf_w_type
  expect_error(f(factor(c("a", "b", "c"))), "2 levels")
  expect_error(f(factor("a")),              "2 levels")
})

test_that(".tabcf_w_type errors on unsupported types", {
  f <- UtopiaPlanitia:::.tabcf_w_type
  expect_error(f(list(1, 2)), "logical")
  expect_error(f(as.Date("2020-01-01")), "logical")
})

# --- .tabcf_make_folds ------------------------------------------------------

test_that(".tabcf_make_folds is deterministic given seed", {
  mk <- UtopiaPlanitia:::.tabcf_make_folds
  set.seed(42); a <- mk(n = 30, K = 3)
  set.seed(42); b <- mk(n = 30, K = 3)
  expect_identical(a, b)
})

test_that(".tabcf_make_folds yields each fold non-empty", {
  mk <- UtopiaPlanitia:::.tabcf_make_folds
  set.seed(1)
  fold <- mk(n = 30, K = 5)
  expect_length(fold, 30)
  expect_setequal(unique(fold), seq_len(5))
  # every fold non-empty
  tab <- table(fold)
  expect_true(all(tab >= 1))
})

test_that(".tabcf_make_folds is cluster-respecting", {
  mk <- UtopiaPlanitia:::.tabcf_make_folds
  # 6 clusters of 5 obs each = 30 obs total, K = 3
  clusters <- rep(seq_len(6), each = 5)
  set.seed(99)
  fold <- mk(n = 30, K = 3, clusters = clusters)
  # Every cluster's units must all be in the same fold
  by_cl <- split(fold, clusters)
  for (cl in by_cl) {
    expect_equal(length(unique(cl)), 1L)
  }
})

test_that(".tabcf_make_folds errors on degenerate clusters (fewer than K)", {
  mk <- UtopiaPlanitia:::.tabcf_make_folds
  # only 2 clusters, asking for 5 folds
  clusters <- rep(c(1, 2), each = 10)
  expect_error(
    mk(n = 20, K = 5, clusters = clusters),
    "cluster"
  )
})

test_that(".tabcf_make_folds falls back to non-cluster mode for trivial clusters", {
  mk <- UtopiaPlanitia:::.tabcf_make_folds
  # NULL, empty, or single-cluster all collapse to plain k-fold
  set.seed(7)
  fold_null <- mk(n = 20, K = 4, clusters = NULL)
  expect_length(fold_null, 20L)
  expect_setequal(unique(fold_null), seq_len(4))

  set.seed(7)
  fold_one <- mk(n = 20, K = 4, clusters = rep(1, 20))
  expect_length(fold_one, 20L)
  expect_setequal(unique(fold_one), seq_len(4))
})

# --- .tabcf_clip_propensity (lo/hi) -----------------------------------------
test_that(".tabcf_clip_propensity clips both tails and counts correctly", {
  clip <- UtopiaPlanitia:::.tabcf_clip_propensity
  W.hat <- c(-0.1, 0, 0.0005, 0.5, 0.9999, 1, 1.5)
  out <- clip(W.hat, lo = 1e-3, hi = 1 - 1e-3, active = TRUE)
  expect_equal(out$clipped, 6L)
  expect_true(all(out$W.hat >= 1e-3))
  expect_true(all(out$W.hat <= 1 - 1e-3))
  expect_equal(out$W.hat[4], 0.5)
})

test_that(".tabcf_clip_propensity is a no-op when active=FALSE", {
  clip <- UtopiaPlanitia:::.tabcf_clip_propensity
  W.hat <- c(-2, 0.5, 3)
  out <- clip(W.hat, lo = 1e-3, hi = 1 - 1e-3, active = FALSE)
  expect_identical(out$W.hat, W.hat)
  expect_equal(out$clipped, 0L)
})

test_that(".tabcf_clip_propensity respects custom lo/hi", {
  clip <- UtopiaPlanitia:::.tabcf_clip_propensity
  W.hat <- c(0.05, 0.5, 0.95)
  out <- clip(W.hat, lo = 0.1, hi = 0.9, active = TRUE)
  expect_equal(out$clipped, 2L)
  expect_equal(out$W.hat, c(0.1, 0.5, 0.9))
})

test_that(".tabcf_clip_propensity returns clipped=0 when nothing to clip", {
  clip <- UtopiaPlanitia:::.tabcf_clip_propensity
  W.hat <- c(0.2, 0.5, 0.8)
  out <- clip(W.hat, lo = 1e-3, hi = 1 - 1e-3, active = TRUE)
  expect_equal(out$clipped, 0L)
  expect_identical(out$W.hat, W.hat)
})

# --- TabPFN-gated end-to-end tests ------------------------------------------

test_that("tabcf cross-fits TabPFN nuisances and returns a causal_forest (binary W)", {
  skip_if_not_installed("tabpfn")
  skip_if(!nzchar(Sys.getenv("TABPFN_TOKEN")),
          "TABPFN_TOKEN not set; skipping live TabPFN test.")
  cf  <- make_small_cf(n = 80, w_kind = "binary")
  cf2 <- tabcf(cf, K = 2)

  expect_s3_class(cf2, "causal_forest")
  expect_length(cf2$predictions, nrow(cf$X.orig))

  meta <- attr(cf2, "tabcf_meta")
  expect_equal(meta$K, 2L)
  expect_equal(meta$w_type, "binary")
  expect_equal(meta$eps, 1e-3)

  # TabPFN nuisances should differ from grf's default ones (sanity).
  expect_false(isTRUE(all.equal(cf$Y.hat, cf2$Y.hat)))
  expect_false(isTRUE(all.equal(cf$W.hat, cf2$W.hat)))
})

test_that("tabcf handles continuous W (regressor for propensity)", {
  skip_if_not_installed("tabpfn")
  skip_if(!nzchar(Sys.getenv("TABPFN_TOKEN")),
          "TABPFN_TOKEN not set; skipping live TabPFN test.")
  cf  <- make_small_cf(n = 80, w_kind = "continuous")
  cf2 <- tabcf(cf, K = 2)

  meta <- attr(cf2, "tabcf_meta")
  expect_equal(meta$w_type, "continuous")
  expect_equal(meta$clipped, 0L)
  expect_true(all(is.finite(cf2$W.hat)))
})

test_that("tabcf result is compatible with downstream methods", {
  skip_if_not_installed("tabpfn")
  skip_if(!nzchar(Sys.getenv("TABPFN_TOKEN")),
          "TABPFN_TOKEN not set; skipping live TabPFN test.")
  cf  <- make_small_cf(n = 80, w_kind = "binary")
  cf2 <- tabcf(cf, K = 2)

  expect_no_error(suppressWarnings(suppressMessages(summary(cf2))))
  pr <- stats::predict(cf2)
  expect_true(is.data.frame(pr) || is.list(pr))
  expect_length(pr$predictions, nrow(cf$X.orig))
})

test_that("tabcf accepts custom eps and propagates to meta", {
  skip_if_not_installed("tabpfn")
  skip_if(!nzchar(Sys.getenv("TABPFN_TOKEN")),
          "TABPFN_TOKEN not set; skipping live TabPFN test.")
  cf  <- make_small_cf(n = 60, w_kind = "binary")
  cf2 <- tabcf(cf, K = 2, eps = 0.05)
  meta <- attr(cf2, "tabcf_meta")
  expect_equal(meta$eps, 0.05)
  expect_true(all(cf2$W.hat >= 0.05))
  expect_true(all(cf2$W.hat <= 0.95))
})

# --- tuning argument and .tabcf_build_tune_args -----------------------------

test_that("tabcf signature exposes tuning with correct default and choices", {
  f <- formals(tabcf)
  expect_true("tuning" %in% names(f))
  # Default = first element of the choices vector = "orig"
  tuning_default <- eval(f$tuning)
  expect_equal(tuning_default[[1]], "orig")
  expect_setequal(tuning_default, c("orig", "cf.default", "cf.autotune"))
})

test_that("tabcf rejects invalid tuning via match.arg", {
  cf <- make_small_cf(n = 40, w_kind = "binary")
  # Should error before any tabpfn dispatch (match.arg runs first).
  expect_error(tabcf(cf, tuning = "nope"), "should be one of")
  expect_error(tabcf(cf, tuning = "ORIG"), "should be one of")
})

test_that(".tabcf_build_tune_args returns inherited params for 'orig'", {
  build <- UtopiaPlanitia:::.tabcf_build_tune_args
  cf <- make_small_cf(n = 40, w_kind = "binary")
  out <- build(cf, "orig")
  expect_type(out, "list")
  expect_setequal(
    names(out),
    c("sample.fraction", "mtry", "min.node.size", "honesty.fraction",
      "honesty.prune.leaves", "alpha", "imbalance.penalty")
  )
  # Values must match c.forest$tunable.params verbatim.
  tp <- cf$tunable.params
  expect_equal(out$sample.fraction,      tp$sample.fraction)
  expect_equal(out$mtry,                 tp$mtry)
  expect_equal(out$min.node.size,        tp$min.node.size)
  expect_equal(out$honesty.fraction,     tp$honesty.fraction)
  expect_equal(out$honesty.prune.leaves, tp$honesty.prune.leaves)
  expect_equal(out$alpha,                tp$alpha)
  expect_equal(out$imbalance.penalty,    tp$imbalance.penalty)
  # Must NOT include a tune.parameters key.
  expect_false("tune.parameters" %in% names(out))
})

test_that(".tabcf_build_tune_args returns empty list for 'cf.default'", {
  build <- UtopiaPlanitia:::.tabcf_build_tune_args
  cf <- make_small_cf(n = 40, w_kind = "binary")
  out <- build(cf, "cf.default")
  expect_type(out, "list")
  expect_length(out, 0L)
})

test_that(".tabcf_build_tune_args returns tune.parameters='all' for 'cf.autotune'", {
  build <- UtopiaPlanitia:::.tabcf_build_tune_args
  cf <- make_small_cf(n = 40, w_kind = "binary")
  out <- build(cf, "cf.autotune")
  expect_type(out, "list")
  expect_equal(out$tune.parameters, "all")
  # Must NOT include any inherited tunable params.
  forbidden <- c("sample.fraction", "mtry", "min.node.size",
                 "honesty.fraction", "honesty.prune.leaves", "alpha",
                 "imbalance.penalty")
  expect_false(any(forbidden %in% names(out)))
})

test_that(".tabcf_build_tune_args validates the tuning argument", {
  build <- UtopiaPlanitia:::.tabcf_build_tune_args
  cf <- make_small_cf(n = 40, w_kind = "binary")
  expect_error(build(cf, "nonsense"), "should be one of")
})

test_that(".tabcf_build_tune_args degrades gracefully when tunable.params is NULL", {
  # Defensive: a stripped/synthetic c.forest with no $tunable.params should
  # behave like cf.default for "orig" (no keys forwarded).
  build <- UtopiaPlanitia:::.tabcf_build_tune_args
  fake <- structure(list(tunable.params = NULL), class = "causal_forest")
  out <- build(fake, "orig")
  expect_type(out, "list")
  expect_length(out, 0L)
})

# --- TabPFN-gated end-to-end test for tuning modes --------------------------

test_that("tabcf records the tuning mode in tabcf_meta (binary W)", {
  skip_if_not_installed("tabpfn")
  skip_if(!nzchar(Sys.getenv("TABPFN_TOKEN")),
          "TABPFN_TOKEN not set; skipping live TabPFN test.")
  cf  <- make_small_cf(n = 60, w_kind = "binary")

  cf_orig <- tabcf(cf, K = 2, tuning = "orig")
  expect_equal(attr(cf_orig, "tabcf_meta")$tuning, "orig")

  cf_def  <- tabcf(cf, K = 2, tuning = "cf.default")
  expect_equal(attr(cf_def, "tabcf_meta")$tuning, "cf.default")
})

# --- .tabcf_resolve_clip ----------------------------------------------------
test_that(".tabcf_resolve_clip: FALSE means no clipping", {
  rc <- UtopiaPlanitia:::.tabcf_resolve_clip
  out <- rc(clip = FALSE, eps = NULL)
  expect_false(out$active)
})

test_that(".tabcf_resolve_clip: TRUE uses default bound", {
  rc <- UtopiaPlanitia:::.tabcf_resolve_clip
  out <- rc(clip = TRUE, eps = NULL)
  expect_true(out$active)
  expect_equal(out$lo, 1e-3)
  expect_equal(out$hi, 1 - 1e-3)
})

test_that(".tabcf_resolve_clip: c(lo,hi) sets manual range", {
  rc <- UtopiaPlanitia:::.tabcf_resolve_clip
  out <- rc(clip = c(0.01, 0.99), eps = NULL)
  expect_true(out$active)
  expect_equal(out$lo, 0.01)
  expect_equal(out$hi, 0.99)
})

test_that(".tabcf_resolve_clip: invalid range errors", {
  rc <- UtopiaPlanitia:::.tabcf_resolve_clip
  expect_error(rc(clip = c(0.9, 0.1)), "0 < lo < hi < 1")
  expect_error(rc(clip = c(-0.1, 0.9)), "0 < lo < hi < 1")
  expect_error(rc(clip = c(0.1, 1.2)), "0 < lo < hi < 1")
  expect_error(rc(clip = c(0.1, 0.2, 0.3)), "FALSE, TRUE, or a numeric")
})

test_that(".tabcf_resolve_clip: eps is deprecated and maps to clip range", {
  rc <- UtopiaPlanitia:::.tabcf_resolve_clip
  expect_warning(out <- rc(clip = FALSE, eps = 0.05), "deprecated")
  expect_true(out$active)
  expect_equal(out$lo, 0.05)
  expect_equal(out$hi, 0.95)
})

test_that(".tabcf_resolve_clip: invalid eps errors", {
  rc <- UtopiaPlanitia:::.tabcf_resolve_clip
  expect_error(suppressWarnings(rc(eps = 0)), "eps")
  expect_error(suppressWarnings(rc(eps = 0.5)), "eps")
  expect_error(suppressWarnings(rc(eps = c(0.1, 0.2))), "eps")
})

test_that(".tabcf_resolve_clip: eps + non-default clip conflict errors", {
  rc <- UtopiaPlanitia:::.tabcf_resolve_clip
  expect_error(rc(clip = c(0.01, 0.99), eps = 0.05), "not both")
  expect_error(rc(clip = TRUE, eps = 0.05), "not both")
})

# --- .tabcf_check_overlap ---------------------------------------------------
test_that(".tabcf_check_overlap warns on out-of-range propensities", {
  chk <- UtopiaPlanitia:::.tabcf_check_overlap
  expect_warning(chk(c(0.005, 0.5, 0.995), active = TRUE),
                 "outside \\[0.01, 0.99\\]")
})

test_that(".tabcf_check_overlap is silent when all in range", {
  chk <- UtopiaPlanitia:::.tabcf_check_overlap
  expect_silent(chk(c(0.2, 0.5, 0.8), active = TRUE))
})

test_that(".tabcf_check_overlap is silent when inactive", {
  chk <- UtopiaPlanitia:::.tabcf_check_overlap
  expect_silent(chk(c(0.001, 0.5, 0.999), active = FALSE))
})

test_that(".tabcf_check_overlap reports count and min/max", {
  chk <- UtopiaPlanitia:::.tabcf_check_overlap
  expect_warning(chk(c(0.005, 0.5, 0.995), active = TRUE), "2 of 3")
})

# --- .tabcf_average_repeats -------------------------------------------------
test_that(".tabcf_average_repeats averages per-unit and sums clipped", {
  avg <- UtopiaPlanitia:::.tabcf_average_repeats
  reps <- list(
    list(Y.hat = c(0, 2), W.hat = c(0.2, 0.4), clipped = 1L),
    list(Y.hat = c(2, 4), W.hat = c(0.4, 0.6), clipped = 2L)
  )
  out <- avg(reps)
  expect_equal(out$Y.hat, c(1, 3))
  expect_equal(out$W.hat, c(0.3, 0.5))
  expect_equal(out$clipped, 3L)
})

test_that(".tabcf_average_repeats with R=1 returns the single repeat verbatim", {
  avg <- UtopiaPlanitia:::.tabcf_average_repeats
  reps <- list(list(Y.hat = c(1, 2, 3), W.hat = c(0.1, 0.2, 0.3),
                    clipped = 0L))
  out <- avg(reps)
  expect_equal(out$Y.hat, c(1, 2, 3))
  expect_equal(out$W.hat, c(0.1, 0.2, 0.3))
  expect_equal(out$clipped, 0L)
})

# --- .tabcf_crossfit_once (mocked) ------------------------------------------
test_that(".tabcf_crossfit_once cross-fits with mocked TabPFN and clips", {
  cf1 <- UtopiaPlanitia:::.tabcf_crossfit_once
  set.seed(1)
  X <- matrix(stats::rnorm(40 * 3), 40, 3); colnames(X) <- paste0("V", 1:3)
  Y <- stats::rnorm(40); W <- stats::rbinom(40, 1, 0.5)

  testthat::local_mocked_bindings(
    .tabcf_fit_predict = function(X_train, y_train, X_test, kind,
                                  tabpfn_args = list()) {
      rep(if (kind == "classifier") 0.9999 else 1.0, nrow(X_test))
    }
  )

  out <- cf1(X = X, Y = Y, W = W, w_type = "binary", K = 2,
             clusters = NULL, fold_seed = 1995L, tabpfn_random_base = 1995L,
             clip_active = TRUE, lo = 1e-3, hi = 1 - 1e-3,
             tabpfn_args = list(), user_supplied_control = FALSE,
             verbose = FALSE, repeat_id = 1L)

  expect_length(out$Y.hat, 40L)
  expect_length(out$W.hat, 40L)
  expect_true(all(out$Y.hat == 1.0))
  expect_true(all(out$W.hat <= 1 - 1e-3))   # 0.9999 > 0.999 -> clipped to 0.999
  expect_equal(out$clipped, 40L)            # all 0.9999 > 1-1e-3
})

test_that(".tabcf_crossfit_once aborts on non-finite output", {
  cf1 <- UtopiaPlanitia:::.tabcf_crossfit_once
  X <- matrix(stats::rnorm(20 * 2), 20, 2); colnames(X) <- c("V1", "V2")
  Y <- stats::rnorm(20); W <- stats::rbinom(20, 1, 0.5)
  testthat::local_mocked_bindings(
    .tabcf_fit_predict = function(X_train, y_train, X_test, kind,
                                  tabpfn_args = list()) {
      rep(NA_real_, nrow(X_test))
    }
  )
  expect_error(
    cf1(X = X, Y = Y, W = W, w_type = "binary", K = 2, clusters = NULL,
        fold_seed = 1L, tabpfn_random_base = 1L, clip_active = FALSE,
        lo = NA_real_, hi = NA_real_, tabpfn_args = list(),
        user_supplied_control = FALSE, verbose = FALSE, repeat_id = 2L),
    "repeat 2"
  )
})
