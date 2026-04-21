library(testthat)
library(grf)
library(UtopiaPlanitia)

# ------------------------------------------------------------------
# Regression tests for Sequential RATE small-sample stability (Task A).
#
# Background: at n <= 300, the first training fold of rate_sequential()
# is too small (~40-60 rows) for the per-fold CATE forest to detect
# heterogeneity. The CATE forest predicts ~constant, which makes
# rank_average_treatment_effect.fit() return estimate = 0, std.err = 0,
# so the per-fold t-statistic is 0/0 = NaN. That NaN used to propagate
# into the aggregated Sequential RATE p-value, surfacing in the print
# output as a silent "NA" in the hetero_detected column.
#
# These tests verify:
#   (i)   no NaN leaks into the output data frame
#   (ii)  the upfront size-check warning fires
#   (iii) when folds are dropped, omni_hetero() either returns a clean
#         NA_real_ (with a human-readable message) or aggregates the
#         valid folds correctly
# ------------------------------------------------------------------

make_small_null_cf <- function(n = 200, p = 5, num.trees = 100, seed = 1995) {
  set.seed(seed)
  X <- matrix(rnorm(n * p), n, p)
  colnames(X) <- paste0("X", seq_len(p))
  W <- rbinom(n, 1, 0.5)
  # null DGP: no treatment effect, no heterogeneity
  Y <- rnorm(n)
  grf::causal_forest(X, Y, W, num.trees = num.trees)
}

test_that("omni_hetero at n = 200 null DGP produces no NaN p-values", {
  cf <- make_small_null_cf(n = 200, num.trees = 100, seed = 1995)
  result <- suppressWarnings(suppressMessages(omni_hetero(cf)))

  expect_s3_class(result, "omni_hetero")

  # Sequential RATE row (first row) must be either a valid probability
  # in [0, 1] or NA_real_ -- never NaN.
  seq.pval <- result$p_value[result$heterogeneity_test ==
                             "Sequential RATE (Wager, 2024)"]
  expect_false(is.nan(seq.pval))
  expect_true(is.na(seq.pval) || (seq.pval >= 0 && seq.pval <= 1))

  # The hetero_detected column must be strictly logical (NA is fine,
  # NaN is not allowed as a logical value).
  expect_type(result$hetero_detected, "logical")
  # Critically: no NaN anywhere in p_value.
  expect_false(any(is.nan(result$p_value)))
})

test_that("omni_hetero emits size-check warning at small n", {
  cf <- make_small_null_cf(n = 200, num.trees = 100, seed = 1995)
  # suppress downstream per-fold warnings; we only care that the
  # upfront size warning is part of the emitted warnings.
  w <- tryCatch(
    withCallingHandlers(
      omni_hetero(cf),
      warning = function(w) {
        # collect the warning message and continue
        invokeRestart("muffleWarning")
      },
      message = function(m) invokeRestart("muffleMessage")
    ),
    error = function(e) e
  )

  # cleaner check: capture all warnings via withCallingHandlers
  msgs <- character(0)
  suppressMessages(
    withCallingHandlers(
      omni_hetero(cf),
      warning = function(w) {
        msgs <<- c(msgs, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    )
  )
  expect_true(any(grepl("Sequential RATE may be unstable",
                        msgs, fixed = TRUE)))
})

test_that("omni_hetero prints cleanly at n = 200 (no NaN hetero_detected)", {
  cf <- make_small_null_cf(n = 200, num.trees = 100, seed = 1995)
  result <- suppressWarnings(suppressMessages(omni_hetero(cf)))
  out <- capture.output(print(result))
  # Must not contain a literal "NaN" token from the Sequential RATE row
  expect_false(any(grepl("NaN", out, fixed = TRUE)))
})

test_that("omni_hetero exposes min_fold_n parameter", {
  # Signature must accept min_fold_n as a named argument
  expect_true("min_fold_n" %in% names(formals(omni_hetero)))
  # Default value is 100
  expect_equal(eval(formals(omni_hetero)$min_fold_n), 100)
})

test_that("omni_hetero min_fold_n = 1 disables the size-check warning at n = 500", {
  # Build a slightly larger forest where n / num.folds = 100; with
  # min_fold_n = 1 the size-check should not fire.
  set.seed(1995)
  n <- 500; p <- 5
  X <- matrix(rnorm(n * p), n, p)
  W <- rbinom(n, 1, 0.5)
  Y <- rnorm(n)
  cf <- grf::causal_forest(X, Y, W, num.trees = 100)

  msgs <- character(0)
  suppressMessages(
    withCallingHandlers(
      omni_hetero(cf, min_fold_n = 1),
      warning = function(w) {
        msgs <<- c(msgs, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    )
  )
  # Size-check warning should NOT be present (n = 500 >= 400, and
  # n/num.folds = 100 >= min_fold_n = 1).
  expect_false(any(grepl("Sequential RATE may be unstable", msgs, fixed = TRUE)))
})

test_that("omni_hetero at n = 150 either returns valid Sequential RATE or NA (never NaN)", {
  # Aggressive stress test: n = 150, num.trees = 100 under a null DGP.
  # With 5 folds, the first training fold is ~30 rows -- almost
  # guaranteed to produce degenerate CATE forests.
  cf <- make_small_null_cf(n = 150, num.trees = 100, seed = 7)
  msgs <- character(0)
  result <- suppressMessages(
    withCallingHandlers(
      omni_hetero(cf),
      warning = function(w) {
        msgs <<- c(msgs, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    )
  )
  seq.pval <- result$p_value[result$heterogeneity_test ==
                             "Sequential RATE (Wager, 2024)"]
  expect_false(is.nan(seq.pval))
  expect_true(is.na(seq.pval) || (seq.pval >= 0 && seq.pval <= 1))
  # And: the size-check warning must have fired
  expect_true(any(grepl("Sequential RATE may be unstable", msgs, fixed = TRUE)))
})
