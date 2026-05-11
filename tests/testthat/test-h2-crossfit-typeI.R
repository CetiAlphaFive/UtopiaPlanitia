# T-H2.1: Monte Carlo Type I rate of cross-fit High/Low test.
# Pre-fix (naive median-split): 0.45 [0.26, 0.66] at n=200, REPS=20
# Post-fix (cross-fit):         0.05 [0.01, 0.24]
# Pass criterion: rejection rate <= 0.20
#
# Constraints: n=200, num.trees=200, REPS=20 (matches audit-20260511/h2_repro.R)
# Wall-clock: ~2 min on a modern laptop.
# Gated behind UTOPIA_RUN_SLOW_TESTS=1 + skip_on_cran/ci so default
# devtools::test() remains fast.

library(testthat)
library(grf)
library(UtopiaPlanitia)

run_slow <- nzchar(Sys.getenv("UTOPIA_RUN_SLOW_TESTS"))

test_that("H2 Monte Carlo: cross-fit Type I rate <= 0.20 at n=200, REPS=20", {
  skip_on_cran()
  skip_on_ci()
  if (!run_slow) skip("slow; set UTOPIA_RUN_SLOW_TESTS=1 to run")

  REPS   <- 20L
  N      <- 200L
  P      <- 5L
  NTREES <- 200L
  ALPHA  <- 0.05

  pvals <- numeric(REPS)
  for (r in seq_len(REPS)) {
    set.seed(42L + r)
    X <- matrix(rnorm(N * P), N, P); colnames(X) <- paste0("X", seq_len(P))
    W <- rbinom(N, 1, 0.5)
    Y <- 0.5 * X[, 1] + rnorm(N)   # main effect only; tau == 0

    cf <- suppressWarnings(
      grf::causal_forest(X, Y, W, num.trees = NTREES, seed = 42L + r)
    )
    oh <- suppressWarnings(suppressMessages(omni_hetero(cf, seed = 1995L)))
    idx <- grep("High vs", oh$heterogeneity_test)
    expect_length(idx, 1)
    pvals[r] <- oh$p_value[idx]
    message(sprintf("  rep %2d  p=%.4f", r, pvals[r]))
  }

  valid <- !is.na(pvals)
  rej <- mean(pvals[valid] <= ALPHA)
  k   <- sum(pvals[valid] <= ALPHA)
  n_v <- sum(valid)

  # Wilson 95% CI
  z <- qnorm(0.975)
  phat <- k / n_v
  denom <- 1 + z^2 / n_v
  lo <- (phat + z^2 / (2 * n_v) -
         z * sqrt(phat * (1 - phat) / n_v + z^2 / (4 * n_v^2))) / denom
  hi <- (phat + z^2 / (2 * n_v) +
         z * sqrt(phat * (1 - phat) / n_v + z^2 / (4 * n_v^2))) / denom

  message(sprintf("=== H2 Type I: %d/%d rejections (rate %.3f) Wilson 95%% CI [%.3f, %.3f]",
                  k, n_v, rej, lo, hi))
  message("Per-rep p-values: ", paste(sprintf("%.4f", pvals), collapse = ", "))

  expect_true(all(pvals >= 0 & pvals <= 1, na.rm = TRUE))
  expect_lte(rej, 0.20)
})
