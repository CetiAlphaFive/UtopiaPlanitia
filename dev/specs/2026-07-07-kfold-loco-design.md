# Design: k-fold cross-fit LOCO

**Date:** 2026-07-07
**Companion plan:** dev/plans/2026-07-07-kfold-loco.md

## 1. API (append-only; backwards-compatible)
Extend `loco()`'s signature by appending:
- `cross.fit = FALSE` — when `FALSE` (default), behavior is EXACTLY today's
  single-split `loco_custom_split()`. When `TRUE`, use K-fold cross-fitting.
- `n.folds = 5L` — number of folds (ignored when `cross.fit = FALSE`).
No existing argument is reordered, renamed, or has its default changed. The
`T-API: loco formals unchanged` guard must be updated deliberately (append the two
new formals at the end) and re-pinned.

## 2. Procedure (cross.fit = TRUE)
Let n observations, predictors P, target set T (per-variable or per-group).
1. Assign observations to K folds via `seed` (stratification not required for
   regression; for probability/classification, stratify by class to avoid empty
   classes in a fold).
2. For each fold k = 1..K:
   a. Train the FULL model on the other K-1 folds.
   b. For each target t in T, train the REDUCED model (drop t's columns) on the
      same K-1 folds.
   c. Predict full and reduced on fold k; compute per-observation loss-residual
      differences `d_i^{(t)} = L(reduced_t) - L(full)` for i in fold k, using the
      SAME per-observation loss functions already in `loco_custom_split()`
      (abs/mse for regression; brier/zero_one/log for probability).
3. Pool `d_i^{(t)}` across all folds -> a length-n vector per target (each obs once).

## 3. Inference (the crux)
The pooled `d_i` are NOT i.i.d.: observations in different folds are evaluated by
models trained on overlapping data, inducing positive correlation. A naive Z-test on
`mean(d)/ (sd(d)/sqrt(n))` is anti-conservative.
- Use the **Nadeau-Bengio corrected variance** for K-fold CV, exactly as
  `cf_perm()`'s `cross.fit = TRUE` path already implements. Concretely: aggregate
  per-fold means `m_k = mean_{i in fold k}(d_i)`, and estimate the corrected
  variance of the overall mean with the train/test-overlap inflation term
  (`1/K + n_test/n_train`), rather than the naive `sd(d)^2/n`.
- One-sided test H0: importance <= 0 vs H1: > 0, Z statistic on the corrected SE.
- Bonferroni correction across |T| targets, identical bookkeeping to the current
  split path.
- **Wilcoxon is not supported under `cross.fit = TRUE`** (signed-rank validity
  assumes independent, symmetric-under-null differences; violated by fold
  dependence). If `method = "wilcox"` and `cross.fit = TRUE`, error early with a
  clear message pointing to `method = "z"`. This mirrors `cf_perm`'s restriction of
  cross-fit inference to the R-loss.

## 4. Refactor
- Generalize `loco_custom_split()` to accept a fold assignment. The current single
  50/50 split becomes the special case "one held-out fold" (K-effective = 1 split);
  keep that code path byte-for-byte for `cross.fit = FALSE` to guarantee
  reproducibility — do NOT reimplement the default through the K-fold code unless a
  bit-identical regression test proves equality.
- Factor the per-observation residual computation (already isolated in
  `residuals_one()`) so both paths share it.
- Reuse `cf_perm`'s NB-variance helper if it is factored out; otherwise extract a
  shared internal (do not duplicate the formula).

## 5. Return object
Same `loco_vimp` classed object as split mode (Variable/Importance/CI.lower/
CI.upper/p.value + metadata). Add metadata fields: `cross.fit` (logical) and
`n.folds` (integer, NA when `cross.fit = FALSE`) so `print.loco_vimp` can note the
mode. plot/summary/print methods need only a one-line header addition.

## 6. Losses x modes matrix
- Regression (abs, mse): supported under cross.fit.
- Probability (brier, zero_one, log): supported; stratify folds by class.
- OOB mode: unaffected (cross.fit only applies to split mode; `cross.fit = TRUE`
  with `split = FALSE` should error or be ignored with a warning — decide: error is
  cleaner).
- Group mode: supported (reduced model drops the whole group each fold).

## 7. Verification (simulation study — no closed-form gold)
Deliver a `sim-spec` for the simulator pipeline:
- **DGP-null:** covariate with zero incremental signal -> estimate empirical Type-I
  rate of the one-sided test at alpha = 0.10 over many reps; assert it is <= ~alpha
  (within Monte-Carlo error) for K in {5, 10} and n in {200, 500}.
- **DGP-signal:** covariate with known signal -> compare Monte-Carlo variance of the
  importance estimate and empirical power between `cross.fit = TRUE` and the single
  split at equal n; assert cross-fit variance is lower / power higher.
- **Back-compat regression test** (`tests/testthat/test-loco-kfold.R`): `cross.fit =
  FALSE` reproduces the pre-change `loco()` output bit-identically.
- **Sanity:** point estimates from cross-fit and split agree in expectation on a
  large-n draw.
Gate on the Type-I study holding nominal level — that is the correctness bar.

## 8. Docs / NEWS
- roxygen: document `cross.fit` and `n.folds`; add a cross-fit example; note the
  Wilcoxon restriction and the compute cost.
- NEWS dev entry under the development-version heading.
- ARCHITECTURE.md: note the cross-fit path.

## 9. Open questions for Jack
- Default `n.folds` = 5 vs 10? (5 is the common default; 10 lowers bias further at
  more compute.)
- Should `cross.fit = TRUE` with `method = "wilcox"` error (recommended) or silently
  fall back to Z with a warning?
- Is the Nadeau-Bengio correction the desired inference, or would you prefer a
  cluster-robust / fold-jackknife variance? (NB matches the existing cf_perm path.)
