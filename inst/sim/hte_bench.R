## =============================================================================
## hte_bench.R — Monte Carlo benchmark for omni_hetero() (overnight run)
##
## OVERNIGHT CONFIG: n in {500..3000}, num.trees = 2000, reps = 50 per cell.
## This intentionally overrides the old n<=300/trees<=300/reps<=20 constraint
## that was set for quick local iteration. Do NOT cargo-cult the old limits back.
##
## Tests 5 heterogeneity tests on type-I error, power, and p-value calibration.
## High vs. Low test uses a cross-fit (sample-split) version to correct winner's
## curse bias — see §Change 2 notes and grf PR #1502 (merged 2025-08-01).
##
## Spec: inst/sim/hte_bench_spec.md
##
## Usage (from package root):
##   Rscript inst/sim/hte_bench.R
##
## Replicability: fully determined by MASTER_SEED alone.
##   Per-rep seed: rep_seed <- MASTER_SEED + 10000L * scenario_idx + rep_idx
## =============================================================================

## -- 0. Setup ----------------------------------------------------------------

suppressPackageStartupMessages({
  devtools::load_all(".", quiet = TRUE)   # loads omni_hetero()
  library(grf)
})

OUT_DIR <- file.path("inst", "sim", "results")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

## -- 1. Global configuration --------------------------------------------------

MASTER_SEED <- 20260422L      # all randomness derives from this single value
REPS        <- 50L
NUM_TREES   <- 2000L          # grf default
N_GRID      <- seq(500L, 3000L, by = 500L)   # {500, 1000, 1500, 2000, 2500, 3000}
SIGMA_SWEEP_N  <- 3000L       # n for sigma.tau sweep
SIGMA_SWEEP_VALS <- c(0.0, 0.15, 0.5, 1.0)

## Parallelism: use mclapply on Linux/macOS, sequential on Windows
USE_PARALLEL <- Sys.info()[["sysname"]] != "Windows"
N_CORES      <- if (USE_PARALLEL) max(1L, parallel::detectCores() - 1L) else 1L

## -- 2. Short test labels (same order as omni_hetero() output rows) ----------

TEST_LABELS <- c(
  "Sequential RATE (Wager, 2024)"                     = "seq_rate",
  "Calibration Test (Chernozhukov et al., 2018)"       = "calibration",
  "High vs. Low CATE (Athey and Wager, 2019)"          = "high_low_xfit",
  "OOB RATE, two-sided (heuristic, anti-conservative)" = "oob_2sided",
  "OOB RATE, one-sided (heuristic)"                   = "oob_1sided"
)

## -- 3. DGP functions ---------------------------------------------------------
## Each returns list(X, Y, W, tau_true).  All set their own seed at entry.

#' Null DGP: tau(x) = 0, nonlinear main effect pmin(X[,3], 0).
#' Mirrors the grf rate_cv vignette null scenario.
dgp_null <- function(n, p = 10, seed) {
  set.seed(seed)
  X   <- matrix(rnorm(n * p), n, p)
  W   <- rbinom(n, 1, 0.5)
  tau <- rep(0, n)
  Y   <- pmin(X[, 3], 0) + W * tau + rnorm(n)
  list(X = X, Y = Y, W = W, tau_true = tau, dgp_label = "null")
}

#' Weak HTE via nw1 (Nie & Wager, 2021).
#' sigma.tau = 0 is the null-equivalent for this DGP in the signal sweep.
dgp_nw1 <- function(n, p = 10, sigma.tau = 0.15, seed) {
  set.seed(seed)
  d <- grf::generate_causal_data(n, p,
                                  sigma.tau   = sigma.tau,
                                  sigma.noise = 1,
                                  dgp         = "nw1")
  list(X = d$X, Y = d$Y, W = d$W, tau_true = d$tau,
       dgp_label = paste0("nw1_s", sigma.tau))
}

#' Sparse nonlinear HTE: tau = pmax(X[,1], 0).
dgp_sparse <- function(n, p = 10, seed) {
  set.seed(seed)
  X   <- matrix(rnorm(n * p), n, p)
  W   <- rbinom(n, 1, 0.5)
  tau <- pmax(X[, 1], 0)
  Y   <- W * tau + rnorm(n)
  list(X = X, Y = Y, W = W, tau_true = tau, dgp_label = "sparse")
}

#' Strong linear HTE: tau = 1.5 * X[,1].  Near-ceiling power regime.
dgp_strong <- function(n, p = 10, seed) {
  set.seed(seed)
  X   <- matrix(rnorm(n * p), n, p)
  W   <- rbinom(n, 1, 0.5)
  tau <- 1.5 * X[, 1]
  Y   <- W * tau + rnorm(n)
  list(X = X, Y = Y, W = W, tau_true = tau, dgp_label = "strong")
}

## -- 4. Cross-fit High vs. Low CATE test -------------------------------------
##
## Motivation: the naive high/low test (median-split of OOB predictions, then
## ATE comparison in the same data) suffers from winner's curse — the DR scores
## used to form the subgroups are NOT independent of the DR scores used to
## estimate ATEs within those subgroups.  grf PR #1502 (merged 2025-08-01)
## removed the naive test from the grf diagnostics vignette for this reason;
## mock run confirmed 35-45% null rejection vs. the nominal 5%.
##
## Fix: 50/50 sample split.  Fold 1 forms the subgroups; fold 2 estimates ATEs.
## This eliminates the dependence and should recover ~5% null rejection.

xfit_high_low <- function(X, Y, W, seed, num.trees) {
  n    <- nrow(X)

  # deterministic 50/50 split
  set.seed(seed)
  fold <- sample(rep(1:2, length.out = n))

  # Fold 1: fit forest to predict CATEs used for grouping
  cf1 <- suppressWarnings(
    grf::causal_forest(X[fold == 1L, , drop = FALSE],
                       Y[fold == 1L],
                       W[fold == 1L],
                       num.trees = num.trees,
                       seed      = seed)
  )

  # Predict on fold 2 using fold-1 forest
  tau_hat2 <- predict(cf1, X[fold == 2L, , drop = FALSE])$predictions

  # Fold 2: fit separate forest whose DR scores we use for ATE estimation
  cf2 <- suppressWarnings(
    grf::causal_forest(X[fold == 2L, , drop = FALSE],
                       Y[fold == 2L],
                       W[fold == 2L],
                       num.trees = num.trees,
                       seed      = seed + 1L)
  )

  # Median-split of fold-2 units based on fold-1 predictions
  med_tau <- median(tau_hat2)
  hi2     <- tau_hat2 > med_tau

  # ATE in each subgroup using fold-2 forest's DR scores
  ate_hi <- grf::average_treatment_effect(cf2, subset =  hi2)
  ate_lo <- grf::average_treatment_effect(cf2, subset = !hi2)

  diff_est <- ate_hi[["estimate"]] - ate_lo[["estimate"]]
  diff_se  <- sqrt(ate_hi[["std.err"]]^2 + ate_lo[["std.err"]]^2)

  2 * pnorm(-abs(diff_est / diff_se))
}

## -- 5. Single-replication runner --------------------------------------------

#' Run one replication: generate data, fit forest, run omni_hetero(), then
#' replace the naive high/low p-value with the cross-fit version.
#'
#' @param dgp_fn   DGP function (one of dgp_*).
#' @param dgp_args Named list of extra args beyond (n, seed) for dgp_fn.
#' @param n        Sample size.
#' @param seed     Integer seed (controls DGP, omni_hetero folds, xfit split).
#' @param num.trees Number of trees for causal_forest.
#' @return data.frame with one row per test (5 rows), plus wide-format p-value
#'   columns used for hte_bench_full.csv.
run_one_rep <- function(dgp_fn, dgp_args = list(), n, seed, num.trees) {

  result <- tryCatch({

    # Generate data
    d <- do.call(dgp_fn, c(list(n = n, seed = seed), dgp_args))

    # Fit causal forest
    cf <- suppressWarnings(
      grf::causal_forest(d$X, d$Y, d$W, num.trees = num.trees, seed = seed)
    )

    # Run omni_hetero battery
    oh <- suppressWarnings(omni_hetero(cf, seed = seed))

    # Map test labels to short names
    short_labels <- TEST_LABELS[oh$heterogeneity_test]

    # -- Cross-fit High vs. Low (replaces naive high_low) ---------------------
    p_xfit <- tryCatch(
      xfit_high_low(d$X, d$Y, d$W,
                    seed      = seed + 999999L,   # distinct from DGP seed
                    num.trees = num.trees),
      error = function(e) NA_real_
    )

    # Build long-format result (5 rows), substituting xfit p-value
    p_vals  <- oh$p_value
    xfit_idx <- which(names(TEST_LABELS) == "High vs. Low CATE (Athey and Wager, 2019)")
    p_vals[xfit_idx] <- p_xfit

    data.frame(
      test      = as.character(short_labels),
      p_value   = p_vals,
      rejected  = ifelse(is.na(p_vals), NA, p_vals <= 0.05),
      seed      = seed,
      dgp       = d$dgp_label,
      n         = n,
      rep_ok    = TRUE,
      error_msg = NA_character_,
      stringsAsFactors = FALSE
    )

  }, error = function(e) {
    data.frame(
      test      = unname(TEST_LABELS),
      p_value   = NA_real_,
      rejected  = NA,
      seed      = seed,
      dgp       = NA_character_,
      n         = n,
      rep_ok    = FALSE,
      error_msg = conditionMessage(e),
      stringsAsFactors = FALSE
    )
  })

  result
}

## Helper to convert a 5-row long result to the wide columns needed for the
## full CSV.  Returns a named list.
long_to_wide_pvals <- function(df5) {
  if (!all(df5$rep_ok)) {
    return(list(
      p_seq_rate    = NA_real_,
      p_calibration = NA_real_,
      p_high_low    = NA_real_,
      p_oob_2s      = NA_real_,
      p_oob_1s      = NA_real_,
      na_flag_seq_rate = NA
    ))
  }
  get_p <- function(lbl) {
    v <- df5$p_value[df5$test == lbl]
    if (length(v) == 0L) NA_real_ else v[1L]
  }
  p_sr <- get_p("seq_rate")
  list(
    p_seq_rate       = p_sr,
    p_calibration    = get_p("calibration"),
    p_high_low       = get_p("high_low_xfit"),
    p_oob_2s         = get_p("oob_2sided"),
    p_oob_1s         = get_p("oob_1sided"),
    na_flag_seq_rate = is.na(p_sr)
  )
}

## -- 6. Scenario definitions --------------------------------------------------

# Main grid: 4 DGPs × 6 n-values
main_scenarios <- list(
  list(label = "null",   dgp_fn = dgp_null,   dgp_args = list()),
  list(label = "nw1",    dgp_fn = dgp_nw1,    dgp_args = list(sigma.tau = 0.15)),
  list(label = "sparse", dgp_fn = dgp_sparse,  dgp_args = list()),
  list(label = "strong", dgp_fn = dgp_strong,  dgp_args = list())
)

scenarios <- list()
scenario_idx <- 0L

for (sc in main_scenarios) {
  for (n_val in N_GRID) {
    scenario_idx <- scenario_idx + 1L
    scenarios[[scenario_idx]] <- list(
      scenario_id = scenario_idx,
      label       = sc$label,
      dgp_fn      = sc$dgp_fn,
      dgp_args    = sc$dgp_args,
      n           = n_val,
      sweep_type  = "main",
      sigma_tau   = if (sc$label == "nw1") 0.15 else NA_real_
    )
  }
}

# Signal sweep: nw1 at n = SIGMA_SWEEP_N, sigma.tau in SIGMA_SWEEP_VALS
for (sig in SIGMA_SWEEP_VALS) {
  scenario_idx <- scenario_idx + 1L
  scenarios[[scenario_idx]] <- list(
    scenario_id = scenario_idx,
    label       = paste0("nw1_sweep_s", sig),
    dgp_fn      = dgp_nw1,
    dgp_args    = list(sigma.tau = sig),
    n           = SIGMA_SWEEP_N,
    sweep_type  = "signal_sweep",
    sigma_tau   = sig
  )
}

N_SCENARIOS <- length(scenarios)

cat("=================================================================\n")
cat("HTE Bench — Overnight Run\n")
cat(sprintf("MASTER_SEED : %d\n", MASTER_SEED))
cat(sprintf("Scenarios   : %d | Reps per cell: %d | num.trees: %d\n",
            N_SCENARIOS, REPS, NUM_TREES))
cat(sprintf("Total reps  : %d\n", N_SCENARIOS * REPS))
cat(sprintf("Parallelism : %s (%d cores)\n",
            if (USE_PARALLEL) "mclapply" else "sequential", N_CORES))
cat(sprintf("Start       : %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
cat("=================================================================\n\n")

## -- 7. Main simulation loop --------------------------------------------------
##
## Each replication is wrapped in run_one_rep().  We collect results into a
## flat list and rbind at the end to avoid memory fragmentation.
##
## Seed derivation (fully reproducible from MASTER_SEED):
##   rep_seed <- MASTER_SEED + 10000L * scenario_idx + rep_idx
##
## mc.set.seed = FALSE because we pass explicit seeds to each worker.

all_long  <- vector("list", N_SCENARIOS)    # one element per scenario
all_wide  <- vector("list", N_SCENARIOS)    # parallel: wide p-value rows
t_start   <- proc.time()

for (sc in scenarios) {

  sid <- sc$scenario_id
  cat(sprintf("[Scenario %d/%d] dgp=%s, n=%d\n",
              sid, N_SCENARIOS, sc$label, sc$n))
  sc_t0 <- proc.time()

  rep_seeds <- MASTER_SEED + 10000L * sid + seq_len(REPS)

  # Worker function for a single rep (captures sc, num.trees from environment)
  run_rep_i <- function(rep_idx) {
    s   <- rep_seeds[rep_idx]
    row <- run_one_rep(dgp_fn    = sc$dgp_fn,
                       dgp_args  = sc$dgp_args,
                       n         = sc$n,
                       seed      = s,
                       num.trees = NUM_TREES)
    row$scenario_id  <- sid
    row$scenario_lbl <- sc$label
    row$sweep_type   <- sc$sweep_type
    row$sigma_tau    <- sc$sigma_tau
    row$rep          <- rep_idx
    list(long = row, wide = long_to_wide_pvals(row))
  }

  if (USE_PARALLEL && N_CORES > 1L) {
    rep_results <- parallel::mclapply(
      seq_len(REPS), run_rep_i,
      mc.cores    = N_CORES,
      mc.set.seed = FALSE
    )
  } else {
    rep_results <- lapply(seq_len(REPS), run_rep_i)
  }

  # Collect long-format rows
  long_rows <- do.call(rbind, lapply(rep_results, `[[`, "long"))
  all_long[[sid]] <- long_rows

  # Collect wide rows
  wide_rows <- do.call(rbind, lapply(seq_along(rep_results), function(ri) {
    w   <- rep_results[[ri]][["wide"]]
    row <- rep_results[[ri]][["long"]][1L, ]   # metadata from first row
    data.frame(
      seed             = row$seed,
      dgp              = row$dgp,
      n                = sc$n,
      sigma_tau        = sc$sigma_tau,
      rep_id           = row$rep,
      p_seq_rate       = w$p_seq_rate,
      p_calibration    = w$p_calibration,
      p_high_low       = w$p_high_low,
      p_oob_2s         = w$p_oob_2s,
      p_oob_1s         = w$p_oob_1s,
      na_flag_seq_rate = w$na_flag_seq_rate,
      stringsAsFactors = FALSE
    )
  }))
  all_wide[[sid]] <- wide_rows

  sc_elapsed <- (proc.time() - sc_t0)[["elapsed"]]
  cat(sprintf("  -> done in %.1fs (%.2fs/rep)\n\n",
              sc_elapsed, sc_elapsed / REPS))
}

total_elapsed <- (proc.time() - t_start)[["elapsed"]]
cat(sprintf("=================================================================\n"))
cat(sprintf("All done. Total time: %.1fs (%.1f min)\n",
            total_elapsed, total_elapsed / 60))
cat(sprintf("End: %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
cat("=================================================================\n\n")

## -- 8. Assemble raw results --------------------------------------------------

raw <- do.call(rbind, all_long)
rownames(raw) <- NULL

wide_full <- do.call(rbind, all_wide)
rownames(wide_full) <- NULL

# Diagnostic: failure rate
fail_rate <- mean(!raw$rep_ok, na.rm = TRUE)
cat(sprintf("Overall failure rate: %.1f%%\n", 100 * fail_rate))
if (fail_rate > 0) {
  cat("Failure messages:\n")
  errs <- unique(raw$error_msg[!raw$rep_ok & !is.na(raw$error_msg)])
  for (e in errs) cat("  -", e, "\n")
  cat("\n")
}

# NA rate for Sequential RATE
seq_na_rate <- mean(is.na(raw$p_value[raw$test == "seq_rate"]), na.rm = TRUE)
cat(sprintf("Sequential RATE NA rate (all scenarios): %.1f%%\n\n",
            100 * seq_na_rate))

## -- 9. Save raw results ------------------------------------------------------

raw_rds  <- file.path(OUT_DIR, "hte_bench_raw.rds")
raw_csv  <- file.path(OUT_DIR, "hte_bench_raw.csv")
full_csv <- file.path(OUT_DIR, "hte_bench_full.csv")

saveRDS(raw, raw_rds)
write.csv(raw,       raw_csv,  row.names = FALSE)
write.csv(wide_full, full_csv, row.names = FALSE)

cat(sprintf("Raw results saved:\n  %s\n  %s\n", raw_rds, raw_csv))
cat(sprintf("Wide full results saved:\n  %s\n\n", full_csv))

## -- 10. Compute summary table ------------------------------------------------

rej_stats <- function(x) {
  x_use <- x[!is.na(x)]
  n_use <- length(x_use)
  if (n_use == 0L) return(c(rej_rate = NA_real_, mc_se = NA_real_,
                             n_reps = 0L, n_na = sum(is.na(x))))
  p_hat <- mean(x_use)
  mc_se <- sqrt(p_hat * (1 - p_hat) / n_use)
  c(rej_rate = p_hat, mc_se = mc_se,
    n_reps = n_use, n_na = sum(is.na(x)))
}

raw_ok <- raw[raw$rep_ok, ]

group_keys <- unique(raw_ok[, c("scenario_lbl", "n", "sigma_tau", "sweep_type")])
rownames(group_keys) <- NULL

# test_order matches the short labels used in TEST_LABELS
test_order <- c("seq_rate", "calibration", "high_low_xfit",
                "oob_2sided", "oob_1sided")

summary_rows <- vector("list", nrow(group_keys) * length(test_order))
si <- 0L

for (i in seq_len(nrow(group_keys))) {
  gk  <- group_keys[i, ]
  sub <- raw_ok[
    raw_ok$scenario_lbl == gk$scenario_lbl &
    raw_ok$n            == gk$n &
    (is.na(raw_ok$sigma_tau) & is.na(gk$sigma_tau) |
     !is.na(raw_ok$sigma_tau) & !is.na(gk$sigma_tau) &
     raw_ok$sigma_tau == gk$sigma_tau),
  ]

  for (tst in test_order) {
    sub_t <- sub[sub$test == tst, ]
    stats <- rej_stats(sub_t$rejected)
    si <- si + 1L
    summary_rows[[si]] <- data.frame(
      scenario    = gk$scenario_lbl,
      sweep_type  = gk$sweep_type,
      n           = gk$n,
      sigma_tau   = gk$sigma_tau,
      test        = tst,
      rej_rate    = stats[["rej_rate"]],
      mc_se       = stats[["mc_se"]],
      ci_lo       = pmax(0, stats[["rej_rate"]] - 1.96 * stats[["mc_se"]]),
      ci_hi       = pmin(1, stats[["rej_rate"]] + 1.96 * stats[["mc_se"]]),
      n_reps_used = stats[["n_reps"]],
      n_na        = stats[["n_na"]],
      stringsAsFactors = FALSE
    )
  }
}

summary_df <- do.call(rbind, summary_rows[seq_len(si)])
rownames(summary_df) <- NULL

## -- 11. Print summary table --------------------------------------------------

cat("=================================================================\n")
cat("SUMMARY: Rejection rates at alpha = 0.05\n")
cat(sprintf("(Overnight run: reps = %d; MC SE at p=0.05 ~ %.1f%%)\n",
            REPS, 100 * sqrt(0.05 * 0.95 / REPS)))
cat("=================================================================\n\n")

main_sum <- summary_df[summary_df$sweep_type == "main", ]
main_sum <- main_sum[order(main_sum$scenario, main_sum$n, main_sum$test), ]

wide_rows <- unique(main_sum[, c("scenario", "n")])
wide_rows <- wide_rows[order(wide_rows$scenario, wide_rows$n), ]

header <- sprintf("%-20s %5s  %10s %12s %14s %12s %12s",
                  "Scenario", "n",
                  "seq_rate", "calibration", "high_low_xfit",
                  "oob_2sided", "oob_1sided")
cat(header, "\n")
cat(strrep("-", nchar(header)), "\n")

for (i in seq_len(nrow(wide_rows))) {
  sc_nm <- wide_rows$scenario[i]
  n_val <- wide_rows$n[i]
  row_sub <- main_sum[main_sum$scenario == sc_nm & main_sum$n == n_val, ]

  vals <- vapply(test_order, function(t) {
    r <- row_sub[row_sub$test == t, ]
    if (nrow(r) == 0 || is.na(r$rej_rate)) return("  NA    ")
    sprintf("%5.1f%% +/-%4.1f%%", 100 * r$rej_rate, 100 * r$mc_se)
  }, character(1))

  cat(sprintf("%-20s %5d  %s  %s  %s  %s  %s\n",
              sc_nm, n_val, vals[1], vals[2], vals[3], vals[4], vals[5]))
}

cat("\n")

# Signal sweep
sweep_sum <- summary_df[summary_df$sweep_type == "signal_sweep", ]
sweep_sum <- sweep_sum[order(sweep_sum$sigma_tau, sweep_sum$test), ]

sigma_vals_all <- sort(unique(sweep_sum$sigma_tau))
if (length(sigma_vals_all) > 0) {
  cat(sprintf("Signal sweep (nw1, n = %d): rejection rates by sigma.tau\n",
              SIGMA_SWEEP_N))
  sw_header <- sprintf("%-12s  %10s %12s %14s %12s %12s",
                       "sigma_tau",
                       "seq_rate", "calibration", "high_low_xfit",
                       "oob_2sided", "oob_1sided")
  cat(sw_header, "\n")
  cat(strrep("-", nchar(sw_header)), "\n")

  for (sig in sigma_vals_all) {
    row_sub <- sweep_sum[!is.na(sweep_sum$sigma_tau) &
                           sweep_sum$sigma_tau == sig, ]
    vals <- vapply(test_order, function(t) {
      r <- row_sub[row_sub$test == t, ]
      if (nrow(r) == 0 || is.na(r$rej_rate)) return("  NA    ")
      sprintf("%5.1f%% +/-%4.1f%%", 100 * r$rej_rate, 100 * r$mc_se)
    }, character(1))
    cat(sprintf("%-12.2f  %s  %s  %s  %s  %s\n",
                sig, vals[1], vals[2], vals[3], vals[4], vals[5]))
  }
  cat("\n")
}

## -- 12. Save summary CSV ----------------------------------------------------

sum_csv <- file.path(OUT_DIR, "hte_bench_summary.csv")
write.csv(summary_df, sum_csv, row.names = FALSE)
cat(sprintf("Summary saved: %s\n\n", sum_csv))

## -- 13. Sanity checks --------------------------------------------------------

cat("=================================================================\n")
cat("SANITY CHECKS\n")
cat("=================================================================\n")

# OOB 2-sided null rejection at largest n
null_oob2 <- summary_df[
  summary_df$scenario == "null" &
  summary_df$test     == "oob_2sided" &
  summary_df$n        == max(N_GRID), ]

if (nrow(null_oob2) > 0 && !is.na(null_oob2$rej_rate)) {
  oob2_rate <- null_oob2$rej_rate
  cat(sprintf(
    "OOB RATE two-sided, null, n=%d: %.1f%% [%.1f%%, %.1f%%]\n",
    max(N_GRID),
    100 * oob2_rate,
    100 * max(0, oob2_rate - 1.96 * null_oob2$mc_se),
    100 * min(1, oob2_rate + 1.96 * null_oob2$mc_se)
  ))
  if (oob2_rate > 0.10) {
    cat("  -> EXPECTED: OOB RATE two-sided is anti-conservative.\n")
  } else {
    cat("  -> NOTE: OOB RATE null rejection <= 10%; expected elevation.\n")
  }
}

# Cross-fit high/low null rejection — should be near 5%
null_hl <- summary_df[
  summary_df$scenario == "null" &
  summary_df$test     == "high_low_xfit" &
  summary_df$n        == max(N_GRID), ]

if (nrow(null_hl) > 0 && !is.na(null_hl$rej_rate)) {
  hl_rate <- null_hl$rej_rate
  cat(sprintf(
    "High/Low xfit, null, n=%d: %.1f%% [%.1f%%, %.1f%%]\n",
    max(N_GRID),
    100 * hl_rate,
    100 * max(0, hl_rate - 1.96 * null_hl$mc_se),
    100 * min(1, hl_rate + 1.96 * null_hl$mc_se)
  ))
  if (hl_rate <= 0.10) {
    cat("  -> OK: cross-fit high/low near nominal 5%.\n")
  } else {
    cat("  -> WARNING: cross-fit high/low null rejection > 10%.\n")
    cat("     Investigate: should be ~5% with sample splitting.\n")
  }
}

# Sequential RATE null rejection
null_seq <- summary_df[
  summary_df$scenario == "null" &
  summary_df$test     == "seq_rate" &
  summary_df$n        == max(N_GRID), ]

if (nrow(null_seq) > 0 && !is.na(null_seq$rej_rate)) {
  seq_rate_null <- null_seq$rej_rate
  na_ct <- null_seq$n_na
  cat(sprintf(
    "Sequential RATE, null, n=%d: %.1f%% [%.1f%%, %.1f%%] (NAs: %d/%d)\n",
    max(N_GRID),
    100 * seq_rate_null,
    100 * max(0, seq_rate_null - 1.96 * null_seq$mc_se),
    100 * min(1, seq_rate_null + 1.96 * null_seq$mc_se),
    na_ct, REPS
  ))
  if (seq_rate_null > 0.15) {
    cat("  -> WARNING: Sequential RATE null rejection > 15% at n=", max(N_GRID), ".\n")
    cat("     Investigate: may indicate size distortion or pipeline bug.\n")
  } else {
    cat("  -> OK: within plausible range.\n")
  }
}

# Failure rate
fail_by_sc <- aggregate(rep_ok ~ scenario_lbl + n, data = raw,
                        FUN = function(x) mean(!x))
names(fail_by_sc)[3] <- "fail_rate"
fail_by_sc <- fail_by_sc[fail_by_sc$fail_rate > 0, ]
if (nrow(fail_by_sc) > 0) {
  cat("\nScenarios with non-zero failure rate:\n")
  print(fail_by_sc)
} else {
  cat("\nAll replications completed without error.\n")
}

cat("\n=================================================================\n")
cat("hte_bench.R complete.\n")
cat(sprintf("Re-run hte_bench_plots.R to generate visualizations.\n"))
cat("=================================================================\n")
