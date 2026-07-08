# Plan: k-fold cross-fit LOCO (data efficiency)

**Date:** 2026-07-07
**Status:** proposed
**Scope:** extend `loco()`'s split-sample mode to optional K-fold cross-fitting.

## What
Add an optional K-fold cross-fitting path to split-mode `loco()` (both ranger and
grf outcome-forest backends), so every observation contributes to the
loss-residual difference exactly once, instead of only the ~n/2 held-out half used
by the current single 50/50 split.

## Why
`loco_custom_split()` currently does ONE 50/50 partition: fit full + reduced models
on half 1, evaluate per-observation loss-residual differences `d_i = L(reduced) -
L(full)` on half 2, then run a Z-test / Wilcoxon signed-rank test on those `d_i`.
Only n/2 observations enter each importance estimate, so:
- point estimates and CIs are noisier than necessary;
- power to detect a truly-important covariate is low at small n;
- results are sensitive to the particular random half-split.

K-fold cross-fitting reuses all n observations for evaluation (each obs held out
once across the K folds), reducing the variance of the importance estimate for the
SAME target functional, at the cost of K× more model refits.

## Non-goals
- No change to the DEFAULT behavior: the current single-split result must be
  reproducible bit-for-bit (backwards-compat contract; `loco()` is now released-adjacent
  and its outputs land in replication materials).
- No change to OOB mode.
- Not adding a new estimand — cross-fit LOCO targets the same LOCO importance.

## Approach (summary; full design in dev/specs/2026-07-07-kfold-loco-design.md)
1. Reuse the existing `cf_perm()` `cross.fit = TRUE` machinery as the template — it
   already does K-fold cross-fitting with Nadeau-Bengio corrected inference for the
   R-loss. loco's cross-fit path should mirror it for consistency of API and
   inference.
2. New arguments at the END of `loco()`'s signature (append-only, defaults reproduce
   current behavior): `cross.fit = FALSE`, `n.folds = 5L`. `cross.fit = FALSE`
   (default) => today's single-split path unchanged. `cross.fit = TRUE` => K-fold.
3. Inference under cross-fitting must use the Nadeau-Bengio variance correction (the
   pooled `d_i` are dependent across folds because models share training data);
   the naive Z-test would be anti-conservative. Restrict cross-fit inference to the
   Z-method (as `cf_perm` restricts its cross-fit inference to the R-loss), and
   document that Wilcoxon is unavailable under `cross.fit = TRUE`.

## Verification strategy
There is no closed-form gold for k-fold LOCO, so correctness is established by a
Monte Carlo **simulation study** (StatsClaw workflow 11/12, simulator pipeline):
- **Type-I / coverage:** under null covariates (no incremental predictive value),
  the one-sided test must hold its nominal level (e.g. ~alpha at alpha = 0.10) and
  CIs their nominal coverage, across n and K.
- **Efficiency gain:** under a covariate with real signal, cross-fit importance must
  show lower Monte-Carlo variance / higher power than the single-split path at
  equal n.
- **Backwards-compat:** `cross.fit = FALSE` reproduces the current split output
  bit-identically (regression test).
- **Point-estimate consistency:** cross-fit and split point estimates agree in
  expectation (same target).

## Risks
- **Inference validity under fold dependence** is the crux; the NB correction's
  assumptions (exchangeable folds, stable learner) must be stated and checked by the
  Type-I simulation.
- **Compute:** cross-fit does `K * (p + 1)` model fits (vs `p + 1` on half-data now);
  document the cost and keep it opt-in.
- **grf honesty / refit determinism** across folds (seed handling per fold).

## Rollout
Plan-first per repo convention: this plan + the design spec, then a
`tests/testthat/test-loco-kfold.R`, a `sim-spec`-driven coverage study, and a
`NEWS.md` dev entry. Ship behind the opt-in flag with defaults unchanged.
