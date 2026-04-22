## =============================================================================
## hte_bench.R — Monte Carlo benchmark for omni_hetero() (mock scaffold)
##
## Tests 5 heterogeneity tests on type-I error, power, and p-value calibration.
## Spec: inst/sim/hte_bench_spec.md
##
## Usage (from package root):
##   Rscript inst/sim/hte_bench.R
##
## Budget: n in {200, 300}, num.trees = 300, reps = 20 per cell.
## Full run: bump to reps >= 200, n up to 5000, num.trees = 2000.
## =============================================================================

## -- 0. Setup ----------------------------------------------------------------

suppressPackageStartupMessages({
  devtools::load_all(".", quiet = TRUE)   # loads omni_hetero()
  library(grf)
})

OUT_DIR <- file.path("inst", "sim", "results")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

## -- 1. Short test labels (same order as omni_hetero() output rows) ----------

TEST_LABELS <- c(
  "Sequential RATE (Wager, 2024)"                     = "seq_rate",
  "Calibration Test (Chernozhukov et al., 2018)"       = "calibration",
  "High vs. Low CATE (Athey and Wager, 2019)"          = "high_low",
  "OOB RATE, two-sided (heuristic, anti-conservative)" = "oob_2sided",
  "OOB RATE, one-sided (heuristic)"                   = "oob_1sided"
)

## -- 2. DGP functions ---------------------------------------------------------
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

## -- 3. Single-replication runner --------------------------------------------

#' Run one replication: generate data, fit forest, run omni_hetero().
#'
#' @param dgp_fn  DGP function (one of dgp_*).
#' @param dgp_args Named list of extra args beyond (n, seed) for dgp_fn.
#' @param n       Sample size.
#' @param seed    Integer seed (controls DGP + omni_hetero fold assignment).
#' @param num.trees Number of trees for causal_forest.
#' @return data.frame with one row per test (5 rows), columns:
#'   test, p_value, rejected, seed, dgp, n, rep_ok, error_msg.
run_one_rep <- function(dgp_fn, dgp_args = list(), n, seed, num.trees = 300) {

  # Attempt the full replication; catch errors to record failure rate
  result <- tryCatch({

    # Generate data
    d <- do.call(dgp_fn, c(list(n = n, seed = seed), dgp_args))

    # Fit causal forest (suppress n<400 warnings that arise from fold fits
    # inside omni_hetero; these are expected at n=200-300)
    cf <- suppressWarnings(
      grf::causal_forest(d$X, d$Y, d$W, num.trees = num.trees, seed = seed)
    )

    # Run test battery (suppress same warnings)
    oh <- suppressWarnings(omni_hetero(cf, seed = seed))

    # Tidy output: one row per test
    short_labels <- TEST_LABELS[oh$heterogeneity_test]

    data.frame(
      test      = as.character(short_labels),
      p_value   = oh$p_value,
      rejected  = ifelse(is.na(oh$p_value), NA, oh$p_value <= 0.05),
      seed      = seed,
      dgp       = d$dgp_label,
      n         = n,
      rep_ok    = TRUE,
      error_msg = NA_character_,
      stringsAsFactors = FALSE
    )

  }, error = function(e) {
    # Return a failure record for all 5 tests
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

## -- 4. Scenario definitions -------------------------------------------------

# Budget parameters
REPS      <- 20
NUM_TREES <- 300
N_VALUES  <- c(200L, 300L)

# Main grid: 4 DGPs × 2 n-values
main_scenarios <- list(
  list(label = "null",   dgp_fn = dgp_null,   dgp_args = list()),
  list(label = "nw1",    dgp_fn = dgp_nw1,    dgp_args = list(sigma.tau = 0.15)),
  list(label = "sparse", dgp_fn = dgp_sparse,  dgp_args = list()),
  list(label = "strong", dgp_fn = dgp_strong,  dgp_args = list())
)

# Signal sweep: nw1 only at n = 300, sigma.tau in {0, 0.15, 0.5, 1.0}
# (sigma.tau = 0.15 at n = 300 is already in the main grid; deduplicate)
sigma_sweep_vals <- c(0.0, 0.15, 0.5, 1.0)

# Build full scenario list: each element is one (DGP, n) cell
scenarios <- list()
scenario_idx <- 0L

for (sc in main_scenarios) {
  for (n_val in N_VALUES) {
    scenario_idx <- scenario_idx + 1L
    scenarios[[scenario_idx]] <- list(
      scenario_id = scenario_idx,
      label       = sc$label,
      dgp_fn      = sc$dgp_fn,
      dgp_args    = sc$dgp_args,
      n           = n_val,
      sweep_type  = "main"
    )
  }
}

# Add nw1 sigma sweep at n=300, excluding sigma.tau=0.15 (already in main)
for (sig in sigma_sweep_vals[sigma_sweep_vals != 0.15]) {
  scenario_idx <- scenario_idx + 1L
  scenarios[[scenario_idx]] <- list(
    scenario_id = scenario_idx,
    label       = paste0("nw1_sweep_s", sig),
    dgp_fn      = dgp_nw1,
    dgp_args    = list(sigma.tau = sig),
    n           = 300L,
    sweep_type  = "signal_sweep",
    sigma_tau   = sig
  )
}

cat("=================================================================\n")
cat("HTE Bench — Mock Scaffold\n")
cat(sprintf("Scenarios: %d | Reps per scenario: %d | num.trees: %d\n",
            length(scenarios), REPS, NUM_TREES))
cat(sprintf("Total replications: %d\n", length(scenarios) * REPS))
cat(sprintf("Start: %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
cat("=================================================================\n\n")

## -- 5. Main simulation loop -------------------------------------------------

all_results <- vector("list", length(scenarios) * REPS)
result_idx  <- 0L
t_start     <- proc.time()

for (sc in scenarios) {

  cat(sprintf("[Scenario %d/%d] dgp=%s, n=%d\n",
              sc$scenario_id, length(scenarios), sc$label, sc$n))

  sc_t_start <- proc.time()

  for (r in seq_len(REPS)) {

    # Deterministic seed: scenario offset + rep index
    rep_seed <- 1000L * sc$scenario_id + r

    result_idx <- result_idx + 1L
    row <- run_one_rep(
      dgp_fn    = sc$dgp_fn,
      dgp_args  = sc$dgp_args,
      n         = sc$n,
      seed      = rep_seed,
      num.trees = NUM_TREES
    )

    # Tag with scenario metadata
    row$scenario_id  <- sc$scenario_id
    row$scenario_lbl <- sc$label
    row$sweep_type   <- sc$sweep_type
    row$sigma_tau    <- if (!is.null(sc$sigma_tau)) sc$sigma_tau else
      if (sc$label == "nw1") 0.15 else NA_real_
    row$rep          <- r

    all_results[[result_idx]] <- row

    # Progress: print every 5 reps or on last rep
    if (r %% 5 == 0 || r == REPS) {
      elapsed <- (proc.time() - sc_t_start)[["elapsed"]]
      cat(sprintf("  rep %2d/%d  elapsed: %.1fs\n", r, REPS, elapsed))
    }
  }

  sc_elapsed <- (proc.time() - sc_t_start)[["elapsed"]]
  cat(sprintf("  -> scenario done in %.1fs\n\n", sc_elapsed))
}

total_elapsed <- (proc.time() - t_start)[["elapsed"]]
cat(sprintf("=================================================================\n"))
cat(sprintf("All done. Total time: %.1fs (%.1f min)\n",
            total_elapsed, total_elapsed / 60))
cat(sprintf("End: %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
cat("=================================================================\n\n")

## -- 6. Assemble raw results -------------------------------------------------

raw <- do.call(rbind, all_results)
rownames(raw) <- NULL

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

## -- 7. Save raw results -----------------------------------------------------

raw_rds <- file.path(OUT_DIR, "hte_bench_raw.rds")
raw_csv <- file.path(OUT_DIR, "hte_bench_raw.csv")
saveRDS(raw, raw_rds)
write.csv(raw, raw_csv, row.names = FALSE)
cat(sprintf("Raw results saved:\n  %s\n  %s\n\n", raw_rds, raw_csv))

## -- 8. Compute summary table ------------------------------------------------

# Helper: rejection rate and MC SE for a vector of logical/NA values
rej_stats <- function(x) {
  x_use <- x[!is.na(x)]           # exclude NAs from denominator
  n_use <- length(x_use)
  if (n_use == 0L) return(c(rej_rate = NA_real_, mc_se = NA_real_,
                             n_reps = 0L, n_na = sum(is.na(x))))
  p_hat <- mean(x_use)
  mc_se <- sqrt(p_hat * (1 - p_hat) / n_use)
  c(rej_rate = p_hat, mc_se = mc_se,
    n_reps = n_use, n_na = sum(is.na(x)))
}

# Group by (scenario_lbl, n, sigma_tau, test)
raw_ok <- raw[raw$rep_ok, ]   # drop error reps for summary

group_keys <- unique(raw_ok[, c("scenario_lbl", "n", "sigma_tau", "sweep_type")])
rownames(group_keys) <- NULL

test_order <- c("seq_rate", "calibration", "high_low", "oob_2sided", "oob_1sided")

summary_rows <- vector("list", nrow(group_keys) * length(test_order))
si <- 0L

for (i in seq_len(nrow(group_keys))) {
  gk <- group_keys[i, ]
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

## -- 9. Print summary table --------------------------------------------------

cat("=================================================================\n")
cat("SUMMARY: Rejection rates at alpha = 0.05\n")
cat(sprintf("(Mock scaffold: reps = %d; MC SE at p=0.05 ~ 4.9%%)\n", REPS))
cat("=================================================================\n\n")

# Main grid: 4 DGPs × 2 n-values
main_sum <- summary_df[summary_df$sweep_type == "main", ]
main_sum <- main_sum[order(main_sum$scenario, main_sum$n, main_sum$test), ]

# Print as wide table: rows = scenario × n, cols = tests
wide_rows <- unique(main_sum[, c("scenario", "n")])
wide_rows <- wide_rows[order(wide_rows$scenario, wide_rows$n), ]

header <- sprintf("%-20s %4s  %10s %12s %10s %12s %12s",
                  "Scenario", "n",
                  "seq_rate", "calibration", "high_low",
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
    sprintf("%5.1f%% +/-%4.1f%%",
            100 * r$rej_rate, 100 * r$mc_se)
  }, character(1))

  cat(sprintf("%-20s %4d  %s  %s  %s  %s  %s\n",
              sc_nm, n_val, vals[1], vals[2], vals[3], vals[4], vals[5]))
}

cat("\n")

# Signal sweep
sweep_sum <- summary_df[summary_df$sweep_type == "signal_sweep" |
                           (summary_df$sweep_type == "main" &
                            summary_df$scenario == "nw1"), ]
sweep_sum <- sweep_sum[order(sweep_sum$sigma_tau, sweep_sum$test), ]

sigma_vals_all <- sort(unique(sweep_sum$sigma_tau))
if (length(sigma_vals_all) > 0) {
  cat("Signal sweep (nw1, n = 300): rejection rates by sigma.tau\n")
  sw_header <- sprintf("%-12s  %10s %12s %10s %12s %12s",
                       "sigma_tau",
                       "seq_rate", "calibration", "high_low",
                       "oob_2sided", "oob_1sided")
  cat(sw_header, "\n")
  cat(strrep("-", nchar(sw_header)), "\n")

  for (sig in sigma_vals_all) {
    row_sub <- sweep_sum[!is.na(sweep_sum$sigma_tau) &
                           sweep_sum$sigma_tau == sig &
                           sweep_sum$n == 300, ]
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

## -- 10. Save summary CSV ----------------------------------------------------

sum_csv <- file.path(OUT_DIR, "hte_bench_summary.csv")
write.csv(summary_df, sum_csv, row.names = FALSE)
cat(sprintf("Summary saved: %s\n\n", sum_csv))

## -- 11. Sanity checks -------------------------------------------------------

cat("=================================================================\n")
cat("SANITY CHECKS\n")
cat("=================================================================\n")

# Check OOB 2-sided null rejection rate
null_oob2 <- summary_df[
  summary_df$scenario == "null" &
  summary_df$test      == "oob_2sided" &
  summary_df$n         == 300, ]

if (nrow(null_oob2) > 0 && !is.na(null_oob2$rej_rate)) {
  oob2_rate <- null_oob2$rej_rate
  cat(sprintf(
    "OOB RATE two-sided, null, n=300: %.1f%% [%.1f%%, %.1f%%]\n",
    100 * oob2_rate,
    100 * max(0, oob2_rate - 1.96 * null_oob2$mc_se),
    100 * min(1, oob2_rate + 1.96 * null_oob2$mc_se)
  ))
  if (oob2_rate > 0.05) {
    cat("  -> EXPECTED: OOB RATE two-sided is anti-conservative.\n")
  } else {
    cat("  -> WARNING: OOB RATE null rejection <= 5%; expected elevation.\n")
    cat("     (With reps=20 this is within MC noise but worth noting.)\n")
  }
} else {
  cat("OOB RATE two-sided, null, n=300: not computed.\n")
}

# Check Sequential RATE null rejection
null_seq <- summary_df[
  summary_df$scenario == "null" &
  summary_df$test      == "seq_rate" &
  summary_df$n         == 300, ]

if (nrow(null_seq) > 0 && !is.na(null_seq$rej_rate)) {
  seq_rate_null <- null_seq$rej_rate
  na_ct <- null_seq$n_na
  cat(sprintf(
    "Sequential RATE, null, n=300: %.1f%% [%.1f%%, %.1f%%] (NAs: %d/%d)\n",
    100 * seq_rate_null,
    100 * max(0, seq_rate_null - 1.96 * null_seq$mc_se),
    100 * min(1, seq_rate_null + 1.96 * null_seq$mc_se),
    na_ct, REPS
  ))
  if (seq_rate_null > 0.20) {
    cat("  -> WARNING: Sequential RATE null rejection > 20% at n=300.\n")
    cat("     Investigate: may indicate size distortion or pipeline bug.\n")
  } else {
    cat("  -> OK: within plausible range for reps=20.\n")
  }
}

# Check failure rate per scenario
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
