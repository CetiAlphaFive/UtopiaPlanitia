# HTE Bench — Monte Carlo Simulation Specification
## `omni_hetero()` Power, Type-I Error, and p-value Calibration

**Version:** v0.2 — overnight run
**Date:** 2026-04-22
**Status:** OVERNIGHT — reps = 50, n in {500, 1000, 1500, 2000, 2500, 3000}

---

## 1. Objective

Benchmark all five heterogeneity tests in `omni_hetero()` on:

1. **Type-I error control** (null rejection rate at alpha = 0.05 under no HTE)
2. **Power** (rejection rate when HTE is present, varying DGP and n)
3. **p-value calibration** (uniformity of p-values under the null)

Reference design: the grf RATE CV vignette
(<https://grf-labs.github.io/grf/articles/rate_cv.html>), which uses the
same nw1 DGP and motivates the sequential RATE test as a correction for the
OOB RATE's anti-conservatism.

**Key change in v0.2:** the naive High vs. Low CATE test has been replaced
with a cross-fit (50/50 sample-split) version that corrects winner's curse
bias.  See §Change 2 and §7.

---

## 2. Tests Benchmarked

Five tests are returned by a single `omni_hetero(c.forest)` call.  The High
vs. Low test p-value is *replaced in the runner* (not in `omni_hetero()`) by
the cross-fit version; see §Change 2.

| Row | Short label | Category | Null hypothesis |
|-----|-------------|----------|-----------------|
| 1 | **seq_rate** | Preferred | RATE = 0 under k-fold CV; targeting = OOB CATE ranking |
| 2 | **calibration** | Preferred | `beta_differential = 0` |
| 3 | **high_low_xfit** | Corrected heuristic | ATE_high - ATE_low = 0 (cross-fit, sample-split) |
| 4 | **oob_2sided** | Heuristic (anti-conservative) | RATE = 0 using OOB predictions directly |
| 5 | **oob_1sided** | Heuristic | RATE <= 0 (one-sided) |

"Reject" = `p_value <= 0.05`.

---

## 3. Data Generating Processes

All DGPs share: p = 10 covariates, W ~ Bernoulli(0.5) RCT, X ~ N(0, I_p).

### DGP 1: Null (no HTE)

```r
dgp_null <- function(n, p = 10, seed) {
  set.seed(seed)
  X <- matrix(rnorm(n * p), n, p)
  W <- rbinom(n, 1, 0.5)
  tau <- rep(0, n)                        # no heterogeneity
  Y   <- pmin(X[, 3], 0) + W * tau + rnorm(n)
  list(X = X, Y = Y, W = W, tau_true = tau)
}
```

Mirrors the null scenario in the grf rate_cv vignette. `pmin(X[,3], 0)` is a
nonlinear main effect that should not inflate type-I error of a properly
calibrated test.

### DGP 2: Weak HTE via nw1 (signal sweep)

```r
dgp_nw1 <- function(n, p = 10, sigma.tau = 0.15, seed) {
  set.seed(seed)
  d <- grf::generate_causal_data(n, p,
                                  sigma.tau   = sigma.tau,
                                  sigma.noise = 1,
                                  dgp         = "nw1")
  list(X = d$X, Y = d$Y, W = d$W, tau_true = d$tau)
}
```

`sigma.tau = 0` is the null-equivalent for this DGP (used in the sigma.tau
sweep). The nw1 DGP is defined in Nie & Wager (2021) and is the canonical
benchmark in the grf vignette. `generate_causal_data()` signature confirmed:
`(n, p, sigma.m=1, sigma.tau=0.1, sigma.noise=1, dgp=...)`.

**sigma.tau sweep** (at n = 3000): {0.0, 0.15, 0.5, 1.0}

### DGP 3: Sparse nonlinear HTE

```r
dgp_sparse <- function(n, p = 10, seed) {
  set.seed(seed)
  X <- matrix(rnorm(n * p), n, p)
  W <- rbinom(n, 1, 0.5)
  tau <- pmax(X[, 1], 0)                 # half-normal shape; sparsely driven
  Y   <- W * tau + rnorm(n)
  list(X = X, Y = Y, W = W, tau_true = tau)
}
```

### DGP 4: Strong linear HTE

```r
dgp_strong <- function(n, p = 10, seed) {
  set.seed(seed)
  X <- matrix(rnorm(n * p), n, p)
  W <- rbinom(n, 1, 0.5)
  tau <- 1.5 * X[, 1]                   # ATE = 0; HTE strong, symmetric
  Y   <- W * tau + rnorm(n)
  list(X = X, Y = Y, W = W, tau_true = tau)
}
```

---

## 4. Scenario Grid

### 4a. Main grid (all 4 DGPs)

| DGP | n values | reps | num.trees | Notes |
|-----|----------|------|-----------|-------|
| Null | 500, 1000, 1500, 2000, 2500, 3000 | 50 | 2000 | Type-I error |
| nw1 (sigma.tau=0.15) | 500, 1000, 1500, 2000, 2500, 3000 | 50 | 2000 | Weak HTE |
| Sparse | 500, 1000, 1500, 2000, 2500, 3000 | 50 | 2000 | Nonlinear power |
| Strong | 500, 1000, 1500, 2000, 2500, 3000 | 50 | 2000 | High power |

Total: 4 DGPs × 6 n-values × 50 reps = **1200 replications** in the main grid.

### 4b. Signal sweep (nw1 only, at n = 3000)

| sigma.tau | n | reps | Notes |
|-----------|---|------|-------|
| 0.00 | 3000 | 50 | Null-equivalent for nw1 |
| 0.15 | 3000 | 50 | Moderate signal |
| 0.50 | 3000 | 50 | Moderate signal |
| 1.00 | 3000 | 50 | Strong signal |

Total: 4 signal levels × 50 reps = **200 replications**.

**Grand total: 1400 replications.**

---

## 5. Change Log (v0.1 → v0.2)

### Change 1 — Wide full-results CSV (`hte_bench_full.csv`)

In addition to the existing long-format `hte_bench_raw.csv` and aggregated
`hte_bench_summary.csv`, the runner now writes:

```
inst/sim/results/hte_bench_full.csv
```

One row per replication, columns:

| Column | Description |
|--------|-------------|
| `seed` | Per-rep seed used |
| `dgp` | DGP label string |
| `n` | Sample size |
| `sigma_tau` | sigma.tau for nw1 DGPs; NA otherwise |
| `rep_id` | Replication index within scenario (1..50) |
| `p_seq_rate` | Sequential RATE p-value |
| `p_calibration` | Calibration test p-value |
| `p_high_low` | Cross-fit High/Low p-value (`high_low_xfit`) |
| `p_oob_2s` | OOB RATE two-sided p-value |
| `p_oob_1s` | OOB RATE one-sided p-value |
| `na_flag_seq_rate` | Logical; TRUE when sequential RATE returned NA |

This wide layout is easy to inspect in a spreadsheet or pass to downstream
analysis without reshaping.

### Change 2 — Cross-fit High vs. Low CATE test

**Problem:** The naive High vs. Low test in `omni_hetero()` computes DR scores
from the full-sample forest and then uses those same DR scores to (a) form
high/low subgroups via a median split on predicted CATEs, and (b) estimate
the ATE within each subgroup.  Because steps (a) and (b) use the same DR
scores, the subgroup assignment is *not independent* of the ATE estimates,
producing winner's curse: the high-CATE subgroup is selected partly because
its DR scores happen to be high, inflating the apparent ATE difference.

This produces severely anti-conservative null rejection.  Mock run at
n = {200, 300}, reps = 20 showed 35–45% null rejection vs. the nominal 5%.
grf PR #1502 (merged 2025-08-01,
<https://github.com/grf-labs/grf/pull/1502>) removed the naive test from
the grf diagnostics vignette for this reason.

**Fix — 50/50 cross-fit (sample splitting):**

```r
xfit_high_low <- function(X, Y, W, seed, num.trees) {
  n    <- nrow(X)
  set.seed(seed)
  fold <- sample(rep(1:2, length.out = n))

  # Fold 1 forest: used only for CATE predictions (subgroup formation)
  cf1 <- grf::causal_forest(X[fold == 1L, ], Y[fold == 1L], W[fold == 1L],
                             num.trees = num.trees, seed = seed)
  tau_hat2 <- predict(cf1, X[fold == 2L, ])$predictions

  # Fold 2 forest: used only for ATE estimation (independent DR scores)
  cf2 <- grf::causal_forest(X[fold == 2L, ], Y[fold == 2L], W[fold == 2L],
                             num.trees = num.trees, seed = seed + 1L)

  med_tau <- median(tau_hat2)
  hi2     <- tau_hat2 > med_tau
  ate_hi  <- grf::average_treatment_effect(cf2, subset =  hi2)
  ate_lo  <- grf::average_treatment_effect(cf2, subset = !hi2)

  diff_est <- ate_hi[["estimate"]] - ate_lo[["estimate"]]
  diff_se  <- sqrt(ate_hi[["std.err"]]^2 + ate_lo[["std.err"]]^2)
  2 * pnorm(-abs(diff_est / diff_se))
}
```

The per-rep seed for the split uses `seed + 999999L` (distinct from the DGP
seed to avoid correlation in small-n cases).

**Expected outcome after fix:** null rejection rate at n = 1000 should drop
from ~35–45% to ~5–10%.  The overnight run will confirm this.

**Implementation note:** `omni_hetero()` in `R/omni_hetero.R` is NOT
modified.  The runner calls `omni_hetero()` as usual, then overwrites the
`high_low` row with the cross-fit p-value.  The label in outputs is renamed
to `high_low_xfit` to make the substitution visible.

### Change 3 — Overnight run configuration

| Parameter | Old (mock) | New (overnight) |
|-----------|-----------|-----------------|
| `N_GRID` | {200, 300} | {500, 1000, 1500, 2000, 2500, 3000} |
| `REPS` | 20 | 50 |
| `NUM_TREES` | 300 | 2000 (grf default) |
| Sigma sweep n | 300 | 3000 |
| Parallelism | none | `mclapply`, `detectCores() - 1` on Linux/macOS |

**Parallelism:** Linux/macOS uses `parallel::mclapply` with
`mc.cores = max(1, detectCores() - 1)`.  Windows falls back to sequential
`lapply`.  `mc.set.seed = FALSE` is set; per-rep seeds are passed explicitly
via the seed derivation below.

---

## 6. Seed Strategy — Master Seed Replicability

**Master seed:** `MASTER_SEED <- 20260422L`

All randomness in the overnight run derives from this single value.

**Per-rep seed derivation:**

```r
rep_seed <- MASTER_SEED + 10000L * scenario_idx + rep_idx
```

where `scenario_idx` is the sequential index over the full scenario list
(1..70 for the overnight grid: 24 main × ... + 4 sweep) and `rep_idx` is
1..50.  The offset of 10000 per scenario ensures no seed collisions up to
10000 reps per scenario.

Each DGP function calls `set.seed(rep_seed)` at entry.  `omni_hetero()` is
called with `seed = rep_seed`.  The cross-fit split uses `seed + 999999L`.

**Consequence:** re-running `hte_bench.R` from scratch with no saved state
produces byte-identical results to a prior run with the same `MASTER_SEED`.

---

## 7. Hypothesized Patterns (full overnight run)

| Test | Under null (n=3000) | Weak HTE | Sparse HTE | Strong HTE |
|------|---------------------|----------|------------|------------|
| seq_rate | ~5% | low-moderate | moderate | high |
| calibration | ~5% | low-moderate | moderate | high |
| **high_low_xfit** | **~5%** (was 35-45% naive) | moderate | high | high |
| oob_2sided | **~30%** (anti-conservative) | elevated | high | high |
| oob_1sided | ~5-15% | moderate | high | high |

The cross-fit high/low test is expected to recover nominal size (~5%) under
the null.  If it still exceeds ~10% at n = 3000, the implementation should
be audited.

MC SE at reps = 50, p = 0.05: sqrt(0.05 × 0.95 / 50) ≈ 3.1%.
MC SE at reps = 50, p = 0.30: sqrt(0.30 × 0.70 / 50) ≈ 6.5%.

---

## 8. Performance Metrics

For each scenario (DGP × n × sigma.tau):

| Metric | Formula | Notes |
|--------|---------|-------|
| **Rejection rate** | (# p <= 0.05) / R | Primary; = empirical size under null, = power under alternative |
| **MC SE** | sqrt(p_hat * (1 - p_hat) / R) | Monte Carlo standard error |
| **95% CI** | p_hat +/- 1.96 * MC_SE | Uncertainty band |
| **NA rate** | (# NA p-values) / R | Sequential RATE NAs from degenerate folds |
| **p-value ECDF** | Visual vs. Uniform(0,1) | Plotted in hte_bench_plots.R |

---

## 9. Estimator Interface

```r
devtools::load_all(".")

cf <- grf::causal_forest(X, Y, W, num.trees = 2000)
oh <- suppressWarnings(omni_hetero(cf, seed = rep_seed))

# Extract p-values; high_low row is overwritten by xfit_high_low()
oh$p_value          # length-5 numeric vector
oh$heterogeneity_test  # test names for mapping to short labels
```

---

## 10. Output Files

| File | Description |
|------|-------------|
| `inst/sim/hte_bench.R` | Simulation runner |
| `inst/sim/hte_bench_plots.R` | Visualization script |
| `inst/sim/results/hte_bench_raw.rds` | Raw long-format results (data.frame, all tests × reps) |
| `inst/sim/results/hte_bench_raw.csv` | Same, as CSV |
| `inst/sim/results/hte_bench_full.csv` | Wide CSV, one row per replication (5 p-value columns) |
| `inst/sim/results/hte_bench_summary.csv` | Aggregated rejection rates with MC SE |
| `inst/sim/results/hte_bench_ecdf.pdf` | p-value ECDF plots under null |
| `inst/sim/results/hte_bench_power_n.pdf` | Power vs. n curves |
| `inst/sim/results/hte_bench_power_sigma.pdf` | Power vs. sigma.tau (nw1) |

---

## 11. Runtime Estimate

At n = 3000, num.trees = 2000:

- `causal_forest()` fit: ~8–12 s
- `omni_hetero()` sequential RATE fits ~3 forests internally: ~25–40 s
- `xfit_high_low()` fits 2 more forests: ~16–24 s
- **Per-rep total: ~50–75 s** at n = 3000

Main grid weighted average (smaller n faster):

- n = 500: ~5–10 s/rep; n = 3000: ~50–75 s/rep
- Rough average: ~25 s/rep × 1200 reps = 30 000 s single-thread
- Signal sweep at n = 3000: ~60 s/rep × 200 reps = 12 000 s single-thread
- **Single-thread total: ~42 000 s (~12 hours)**

With parallelism on a machine with ~16 cores using 15 workers:

- Effective: ~42 000 / 15 ≈ 2 800 s (~45 min) if perfectly parallel
- Realistic (overhead, memory pressure): **2–4 hours**

---

## 12. Assumptions and Flags

1. **`generate_causal_data` signature confirmed** as `(n, p, sigma.m, sigma.tau, sigma.noise, dgp)`.

2. **Sequential RATE NAs at small n** are expected. At n = 500 the fold-level
   forests may degenerate; NA counts are reported and excluded from the
   rejection-rate denominator.

3. **Anti-conservatism of OOB RATE two-sided**: expected ~30% null rejection
   at n >= 1000. This is the primary motivation for the sequential RATE test
   and for the cross-fit correction to high/low.

4. **`suppressWarnings()` around `causal_forest`**: Sequential RATE internally
   refits forests on folds, triggering n < 400 stability warnings at the
   smaller n values. Suppressed in the runner to keep logs readable.

5. **Cross-fit seed independence**: the xfit split seed is `rep_seed + 999999L`,
   which differs from the DGP seed (`rep_seed`) by a large prime-ish offset,
   ensuring the split assignment is not accidentally correlated with the
   generated data.

6. **No modification to `R/omni_hetero.R`**: the naive High vs. Low p-value
   is produced by `omni_hetero()` and then silently overwritten in the runner.
   The label `high_low_xfit` in all output files distinguishes the corrected
   version from the naive version.
