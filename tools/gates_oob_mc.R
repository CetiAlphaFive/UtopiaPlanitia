# Monte Carlo: OOB vs cross-fit GATES contrast calibration
# Run: Rscript tools/gates_oob_mc.R
# Or gated test in test-gates-oob-mc.R

run_gates_mc <- function(REPS = 40L, N = 300L, NTREES = 150L, ALPHA = 0.10) {
  reject_oob <- reject_cf <- numeric(REPS)
  power_oob <- power_cf <- numeric(REPS)

  for (r in seq_len(REPS)) {
    set.seed(1000L + r)
    p <- 5
    X <- matrix(rnorm(N * p), N, p)
    colnames(X) <- paste0("X", seq_len(p))
    W <- rbinom(N, 1, 0.5)

    Y_null <- rnorm(N)
    cf0 <- grf::causal_forest(X, Y_null, W, num.trees = NTREES, seed = 1000L + r)
    g0_oob <- UtopiaPlanitia::gates(cf0, monotonize = FALSE, seed = 1995L)
    g0_cf  <- UtopiaPlanitia::gates(cf0, cross.fit = TRUE, monotonize = FALSE,
                                   seed = 1995L)
    reject_oob[r] <- g0_oob$diff$p.right[1] < ALPHA
    reject_cf[r]  <- g0_cf$diff$p.right[1] < ALPHA

    Y_alt <- X[, 1] * W + rnorm(N)
    cf1 <- grf::causal_forest(X, Y_alt, W, num.trees = NTREES, seed = 2000L + r)
    g1_oob <- UtopiaPlanitia::gates(cf1, monotonize = FALSE, seed = 1995L)
    g1_cf  <- UtopiaPlanitia::gates(cf1, cross.fit = TRUE, monotonize = FALSE,
                                   seed = 1995L)
    power_oob[r] <- g1_oob$diff$p.right[1] < ALPHA
    power_cf[r]  <- g1_cf$diff$p.right[1] < ALPHA
  }

  list(
    null_reject_oob = mean(reject_oob),
    null_reject_cf  = mean(reject_cf),
    alt_reject_oob  = mean(power_oob),
    alt_reject_cf   = mean(power_cf),
    REPS = REPS,
    N = N,
    ALPHA = ALPHA
  )
}

if (sys.nframe() == 0L) {
  if (!requireNamespace("pkgload", quietly = TRUE)) {
    stop("pkgload required")
  }
  pkgload::load_all()
  res <- run_gates_mc(REPS = 50L)
  cat("GATES MC (one-sided G4-G1, nominal alpha =", res$ALPHA, ")\n")
  cat("Null rejection  OOB:", round(res$null_reject_oob, 3),
      " cross-fit:", round(res$null_reject_cf, 3), "\n")
  cat("Alt rejection   OOB:", round(res$alt_reject_oob, 3),
      " cross-fit:", round(res$alt_reject_cf, 3), "\n")
}
