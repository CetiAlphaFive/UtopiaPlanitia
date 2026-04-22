# HTE Bench — Monte Carlo Simulation Specification
## `omni_hetero()` Power, Type-I Error, and p-value Calibration

**Version:** mock scaffold (v0.1)
**Date:** 2026-04-22
**Status:** MOCK — reps = 20, n <= 300. See §8 for full-run requirements.

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

---

## 2. Tests Benchmarked

Five tests are returned by a single `omni_hetero(c.forest)` call:

| Row | `heterogeneity_test` label (abbreviated) | Category | Null hypothesis |
|-----|------------------------------------------|----------|-----------------|
| 1 | **Sequential RATE** (Wager, 2024) | Preferred | RATE = 0 under k-fold cross-validation; targeting function is OOB CATE ranking |
| 2 | **Calibration / BLP** (Chernozhukov et al., 2018) | Preferred | `beta_differential = 0`: forest predictions explain no variation in DR scores beyond the ATE |
| 3 | **High vs. Low CATE** (Athey & Wager, 2019) | Heuristic | ATE_high - ATE_low = 0 (median-split of predicted CATEs) |
| 4 | **OOB RATE two-sided** (heuristic) | Heuristic | RATE = 0 using OOB predictions directly (anti-conservative; known ~30% rejection under null) |
| 5 | **OOB RATE one-sided** (heuristic) | Heuristic | RATE <= 0 (one-sided; approximately valid when direction of heterogeneity is pre-specified) |

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

**sigma.tau sweep** (at n = 300 only): {0.0, 0.15, 0.5, 1.0}

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

Strong signal from one covariate only, nonlinear threshold at zero. Tests
whether the battery is sensitive to sparse structure.

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

Near-ceiling power regime. OOB RATE should approach 100%; sequential RATE
and calibration test should also show high power.

---

## 4. Scenario Grid

### 4a. Main grid (all 4 DGPs)

| DGP | n values | reps | num.trees | Notes |
|-----|----------|------|-----------|-------|
| Null | 200, 300 | 20 | 300 | Type-I error |
| nw1 (sigma.tau=0.15) | 200, 300 | 20 | 300 | Weak HTE |
| Sparse | 200, 300 | 20 | 300 | Nonlinear power |
| Strong | 200, 300 | 20 | 300 | High power |

Total: 4 DGPs × 2 n-values × 20 reps = **160 replications** in the main grid.

### 4b. Signal sweep (nw1 only, at n = 300)

| sigma.tau | n | reps | Notes |
|-----------|---|------|-------|
| 0.00 | 300 | 20 | Null-equivalent for nw1 |
| 0.15 | 300 | 20 | Same as main grid |
| 0.50 | 300 | 20 | Moderate signal |
| 1.00 | 300 | 20 | Strong signal |

Total: 4 signal levels × 20 reps = **80 replications** (reps at sigma.tau=0.15
shared with main grid; runner deduplicates).

**Grand total: ~220 replications** (with deduplication), each requiring ~5-10s
for the sequential RATE forest fits. Expected wall-clock: 20-35 min.

---

## 5. Performance Metrics

For each scenario (DGP × n × sigma.tau):

| Metric | Formula | Notes |
|--------|---------|-------|
| **Rejection rate** | (# p <= 0.05) / R | Primary metric; = empirical size under null, = power under alternative |
| **MC SE** | sqrt(p_hat * (1 - p_hat) / R) | Monte Carlo standard error for rejection rate |
| **95% CI** | p_hat +/- 1.96 * MC_SE | Uncertainty band on rejection rate |
| **Mean p-value** | mean(p_values) | Should be ~0.5 under null (uniform) |
| **Median p-value** | median(p_values) | Robust check; should be ~0.5 under null |
| **NA rate** | (# NA p-values) / R | Sequential RATE NAs from degenerate folds |
| **p-value uniformity** | Visual ECDF vs. Uniform(0,1) | Plotted; KS stat optional |

---

## 6. Seed Strategy

- **Master seed:** The r-th replication in scenario s uses
  `seed_r = 1000 * scenario_index + r` where scenario indices are assigned
  sequentially over the full grid.
- Each DGP function calls `set.seed(seed_r)` at entry.
- `omni_hetero()` is called with `seed = seed_r` to make fold assignments
  reproducible per replication.
- Re-running `hte_bench.R` from scratch with no saved state produces identical
  results.

---

## 7. Hypothesized Patterns

These are the expected results for the full-run (reps >= 200). The mock
(reps = 20) has too little precision to confirm or refute them, but gross
violations would flag pipeline bugs.

| Test | Under null | Weak HTE (nw1) | Sparse HTE | Strong HTE |
|------|-----------|----------------|------------|------------|
| Sequential RATE | ~5% | low-moderate | moderate | high |
| Calibration / BLP | ~5% | low-moderate | moderate | high |
| High vs. Low CATE | ~5%? | moderate | high | high |
| OOB RATE two-sided | **~30%** (anti-conservative) | elevated | high | high |
| OOB RATE one-sided | ~5-15%? | moderate | high | high |

The 30% null rejection for OOB RATE two-sided is documented in the grf vignette
and is the primary motivation for the sequential RATE test. This benchmark
should reproduce that finding at n >= 1000 with reps >= 200.

**Sanity checks for the mock run:**
- OOB RATE two-sided null rejection rate should be visibly elevated (>= 5%,
  likely 15-40% given reps=20 noise).
- Sequential RATE and Calibration null rejection rates should be <= ~20%
  (within 2 MC-SE of 5% at reps=20 is 0-14.9%; rates above 20% would be
  anomalous).
- Any NA rate > 50% for Sequential RATE at n=200 would warrant investigation.

---

## 8. What the Mock CAN and CANNOT Tell Us

### MC SE at reps = 20

At p = 0.05 (true type-I error = 5%), MC SE = sqrt(0.05 * 0.95 / 20) = 4.87%.
The 95% confidence interval for the observed rejection rate spans
[0%, 14.9%]. In other words, **with 20 reps, an observed rejection rate
anywhere from 0% to 15% is consistent with true 5% size**. Similarly, an
observed rate of 20% is only 3 MC-SEs above nominal.

### What the mock IS useful for

- Catching pipeline bugs (estimator failures, NA storms, wrong p-value column)
- Verifying the DGP produces data of the right shape
- Confirming that strong-HTE scenarios produce visibly higher rejection rates
  than null scenarios
- Timing estimates for the full run
- Confirming that OOB RATE two-sided produces elevated rejections under null
  (should be detectably > 5% even at 20 reps if the effect is real)

### What the mock CANNOT tell us

- Whether type-I error of Sequential RATE or Calibration is exactly 5%
- Whether one test is meaningfully more powerful than another (all CIs overlap)
- p-value uniformity (20 points on an ECDF is uninformative)

### Full-run requirements (for publication-grade results)

```
reps     >= 200   (MC SE at p=0.05: ~1.5%)
n values  = {200, 500, 1000, 2000, 5000}
num.trees = 2000
```

At reps = 200 and p = 0.30 (expected OOB RATE null rejection), MC SE =
sqrt(0.30 * 0.70 / 200) = 3.2%, so the 30% null rejection is detectable
with 95% CI of [23.6%, 36.4%].

---

## 9. Estimator Interface

```r
# Load package (development mode)
devtools::load_all(".")

# Fit forest
cf <- grf::causal_forest(X, Y, W, num.trees = 300)

# Run battery (suppressWarnings for expected small-n Sequential RATE warning)
result <- suppressWarnings(omni_hetero(cf, seed = rep_seed))

# Extract p-values (one row per test, in order)
result$p_value   # length-5 numeric vector; may contain NA for Sequential RATE
result$heterogeneity_test  # test names for labeling
```

The `heterogeneity_test` column values (in order) are:
1. `"Sequential RATE (Wager, 2024)"`
2. `"Calibration Test (Chernozhukov et al., 2018)"`
3. `"High vs. Low CATE (Athey and Wager, 2019)"`
4. `"OOB RATE, two-sided (heuristic, anti-conservative)"`
5. `"OOB RATE, one-sided (heuristic)"`

Short labels used in tables/plots:
1. `seq_rate`
2. `calibration`
3. `high_low`
4. `oob_2sided`
5. `oob_1sided`

---

## 10. Output Files

| File | Description |
|------|-------------|
| `inst/sim/hte_bench.R` | Simulation runner |
| `inst/sim/hte_bench_plots.R` | Visualization script |
| `inst/sim/results/hte_bench_raw.rds` | Raw per-replication results (data.frame) |
| `inst/sim/results/hte_bench_raw.csv` | Same, as CSV |
| `inst/sim/results/hte_bench_summary.csv` | Aggregated rejection rates with MC SE |
| `inst/sim/results/hte_bench_ecdf.pdf` | p-value ECDF plots under null |
| `inst/sim/results/hte_bench_power_n.pdf` | Power vs. n curves |
| `inst/sim/results/hte_bench_power_sigma.pdf` | Power vs. sigma.tau (nw1) |

---

## 11. Assumptions and Flags

1. **`generate_causal_data` signature confirmed** as `(n, p, sigma.m, sigma.tau, sigma.noise, dgp)`.
   sigma.tau = 0 in nw1 is treated as the null-equivalent for the signal sweep.

2. **Sequential RATE NAs at small n** are expected and informative. NA counts
   are reported in the summary table. Reps with NA p-values are excluded from
   rejection rate computation (denominator = usable reps).

3. **Anti-conservatism of OOB RATE two-sided**: expected ~30% null rejection
   at n >= 1000. At n = 200-300, the magnitude may be somewhat different
   (causal forests have noisier OOB predictions at small n). The mock should
   show elevated rejection relative to the Preferred tests.

4. **`suppressWarnings()` around `causal_forest`**: the Sequential RATE
   function internally refits causal forests on folds, each of which will
   trigger the n < 400 stability warning. This is documented expected behavior
   and suppressed in the runner to keep logs readable. Warnings are noted in
   this spec.

5. **`sample.weights` in `omni_hetero`**: when the original forest has no
   sample weights, `c.forest$sample.weights` is NULL. The internal calls to
   `causal_forest` pass `sw[train]` which would error if `sw` is NULL. This
   is handled inside `omni_hetero()` (it extracts `sw <- c.forest$sample.weights`
   and passes it through); the DGPs in this benchmark do not use sample weights
   so this path is not exercised.
