# Design: `cf_perm()` — PermuCATE variable importance for grf causal forests

**Date:** 2026-06-29
**Status:** Approved (design); implementation pending
**Author:** Jack T. Rametta (with Claude Code)
**Feature:** New exported function `cf_perm()` porting the PermuCATE algorithm to grf causal forests.

## Reference

- Paillard, J., Reyero Lobo, A. D., Kolodyazhniy, V., Thirion, B., & Engemann, D.-A. (2025). *Measuring Variable Importance in Heterogeneous Treatment Effects with Confidence.* ICML 2025. arXiv:2408.13002.
- Chamma, A., Engemann, D.-A., & Thirion, B. (2023). *Statistically Valid Variable Importance Assessment through Conditional Permutations.* arXiv:2309.07593. (Conditional Permutation Importance / CPI.)
- Python reference implementation: https://github.com/jpaillard/permucate

## Motivation

The package already ships `cf_loco()` (Benard & Josse 2023 debiased LOCO). PermuCATE is the
complementary method: instead of dropping a covariate and **refitting** `p+1` forests, it
**conditionally permutes** each covariate and re-scores the *fixed* fitted forest. Its headline
advantages over LOCO are (a) lower variance / higher power in finite samples, and (b) built-in
**inference** — p-values and confidence intervals per covariate. `cf_perm()` brings that to grf
users, returning a classed object with print/summary/plot S3 methods consistent with `cf_loco`.

## Algorithm

### What is measured

Per-covariate total-Sobol-style heterogeneity index `E[ Var[ tau(X) | X_-j ] ]`: how much of the
treatment-effect heterogeneity is driven by covariate `j`. Operationalized as the increase in CATE
risk when `X_j` is conditionally permuted (its association with the outcome broken while the joint
covariate distribution is preserved).

### Inputs pulled from the fitted forest

`X = c.forest$X.orig`, `Y = c.forest$Y.orig`, `W = c.forest$W.orig`,
`m = c.forest$Y.hat` (= E[Y|X]), `pi = c.forest$W.hat` (= propensity),
`tau.hat = c.forest$predictions` (OOB CATE).

### Per-observation risk (only tau varies; nuisances stay at original X)

- **R-loss (default):** `L_i(tau) = ((Y_i - m_i) - (W_i - pi_i) * tau_i)^2`
- **AIPW PO-risk:** `psi = grf::get_scores(c.forest)` (length-n DR pseudo-outcome, verified:
  `mean(psi) == average_treatment_effect(cf)`); `L_i(tau) = (psi_i - tau_i)^2`

Keeping `m, pi, psi, Y, W` fixed at original X (only `tau` re-evaluated on permuted X) matches the
Python `compute_r_risk` / `compute_pseudo_outcome_risk` and is what makes the method cheap.

### Conditional permutation sampler (internal `.cf_perm_cp_sample`)

For covariate `j`:
1. Fit `nu_hat_j = E[X_j | X_-j]`:
   - continuous `X_j`: `grf::regression_forest(X[,-j], X[,j])`, predictions OOB.
   - discrete `X_j` (factor, or integer with cardinality <= `disc.max` [default 10]):
     `grf::probability_forest(X[,-j], factor(X[,j]))`; new values drawn from predicted class
     probabilities (mirrors Python `rng.choice`).
2. Continuous: residuals `e = X_j - nu_hat_j`; for each perm `X_j_perm = nu_hat_j + sample(e)`.
   Discrete: `X_j_perm = sampled class per row from predicted probs`.
3. Returns an `n x n.perm` matrix of permuted `X_j` columns.

### Importance

For each perm `k`: rebuild `X_perm` (column j replaced), `tau_perm = predict(c.forest, X_perm)$predictions`,
per-obs contribution `d_i^(k) = (L_i(tau_perm) - L_i(tau.hat)) / 2`.
Average over perms: `d_bar_i = mean_k d_i^(k)`. Importance `Psi_j = mean_i d_bar_i`.

## Inference

### Light path (`cross.fit = FALSE`, default)

Influence-function-style SE on the per-observation contributions (standard for single-split CPI,
Williamson et al.):
`SE_j = sd(d_bar_i) / sqrt(n)`, `z_j = Psi_j / SE_j`, one-sided `p = 1 - Phi(z_j)` (H0: Psi_j <= 0),
CI `Psi_j +/- z_{1-alpha/2} * SE_j`. Documented as **approximate** (single fitted forest; permuted
predictions are in-sample re-scores, OOB `tau.hat` mitigates optimism for the baseline term).
**Amendment (2026-06-29):** the baseline risk uses the forest's out-of-bag predictions (`c.forest$predictions`), not in-sample predictions, so null inference is conservative; the permuted predictions remain non-OOB. This trades some power on weak effect modifiers for false-positive control. cross.fit = TRUE remains the route to unbiased inference.

### Cross-fit path (`cross.fit = TRUE`)

Faithful to the paper:
1. K-fold split, stratified on `W` (`num.folds`, default 5).
2. Per fold: refit `causal_forest` on the training rows, reusing the passed forest's
   hyperparameters (same clone pattern as `cf_loco`: `_num_trees`, `tunable.params`, `clusters`,
   `sample.weights`, etc.). Compute `m, pi, tau.hat` (and `psi` for AIPW) on the held-out test rows.
3. Fit `nu_hat_j` on train, residualize + permute on test, score importance per test-fold obs.
4. Per-fold importance `Psi_j^(k) = mean over test-fold obs`.
5. Aggregate across folds with the **Nadeau-Bengio** corrected variance (mirror Python
   `compute_p_val`): `z_j = mean_k Psi_j^(k) / sd_k(Psi_j^(k))` with the NB finite-sample
   correction term; `p = norm.sf(z_j)`.

## Screening

Reuse `cf_loco`'s split-frequency screening (`screen = FALSE | TRUE | integer-k`) via
`grf::variable_importance()` (free). Screened-out covariates receive `Importance = 0`, `p.value = 1`,
`SE = NA`, and are skipped (no CP sampler / no refit).

## Output object — class `"cf_perm"`

```
list(
  vimp       = data.frame(Variable, Importance, SE, z, p.value, CI.lower, CI.upper),
  loss       = "R" | "AIPW",
  cross.fit  = logical,
  n.perm     = integer,
  num.folds  = integer | NA,
  normalized = logical,
  conf.level = numeric,
  n          = integer,
  p          = integer
)
```

`normalize = TRUE` follows `cf_loco`: clip negatives to 0, rescale Importance to sum 1 (uniform-1/p
fallback with warning when all <= 0). Normalization affects the `Importance` column only; SE/z/p/CI
reflect the raw scale and are set to `NA` when normalized (documented).

### S3 methods (`R/cf_perm_methods.R`, mirroring `cf_loco_methods.R`)

- `print.cf_perm` / `summary.cf_perm`: table sorted by importance with significance stars
  (`*** ** * .`), header showing `n`, `p`, `loss`, `cross.fit`, `n.perm`.
- `plot.cf_perm`: points with CI error bars (lollipop-style to match `plot.cf_loco`), serif theme,
  colored / flagged by significance at `conf.level`. Returns a ggplot; guarded by
  `requireNamespace("ggplot2")`.

## Files

| Path | Role |
|------|------|
| `R/cf_perm.R` | `cf_perm()` + internal `.cf_perm_cp_sample`, `.cf_perm_risk` |
| `R/cf_perm_methods.R` | `print`/`summary`/`plot` S3 |
| `tests/testthat/test-cf_perm.R` | unit + correctness tests |
| `NEWS.md` | dev-version entry |
| `_pkgdown.yml` | reference index entry |
| `man/*` | regenerated via `devtools::document()` |

## Testing

- **Smoke:** runs on a toy DGP, returns `cf_perm` with the documented columns/classes.
- **Correctness:** in `Y = X1 * W + noise`, `X1` gets significantly positive importance; pure-noise
  covariates ~0 and non-significant.
- **Both losses** (`R`, `AIPW`) run and agree in sign on the signal variable.
- **Cross-fit path** runs and returns fold-based p-values.
- **Screen** semantics: screened covariates -> `Importance = 0`, `p.value = 1`.
- **Discrete covariate** path: binary/factor `X_j` permuted via probability forest.
- **Input validation:** non-`causal_forest` input errors.
- **S3:** `print`/`summary`/`plot` return expected types.
- **Reproducibility:** same `seed` -> identical output.
- Heavy Type-I-rate Monte Carlo gated behind `UTOPIA_RUN_SLOW_TESTS=1` + `skip_on_cran()`.

## Scope / non-goals (v1)

- No `variable.groups` (single-covariate importance only; grouped CP is future work).
- No separate `mu_0` / `mu_1` refit — AIPW uses `grf::get_scores()`.
- grf-only CP sampler (no ranger / pluggable estimator in v1).
- New export -> no backwards-compatibility break; add to NAMESPACE via `document()`.

## Open checks for implementation

1. **AIPW baseline self-favoring:** `grf::get_scores()` bakes the forest's own `tau.hat` into `psi`,
   so `L_i(tau.hat)` for AIPW is the DR-correction residual rather than PermuCATE's
   outcome-model-based `psi - tau.hat`. Empirically compare AIPW vs R-loss importances on a known
   DGP before trusting AIPW magnitudes; if biased, document R-loss as preferred.
2. **Discrete detection cutoff** (`disc.max`): confirm 10 is reasonable; expose as an internal
   constant first, promote to an argument only if needed.
3. **Cross-fit `get_scores` on test rows:** confirm the correct way to obtain DR scores for held-out
   rows using train-fold nuisances (may require manual AIPW assembly rather than `get_scores` on the
   train forest).
