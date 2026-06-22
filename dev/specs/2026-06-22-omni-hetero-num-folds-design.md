# Expose `num.folds` in the Sequential RATE test

**Date:** 2026-06-22
**Status:** Approved (design)
**Scope:** `omni_hetero()`, `summary.causal_forest()`, tests, docs

## Problem

The Sequential RATE test (Wager, 2024) inside `omni_hetero()` runs a K-fold
cross-validation where K is hardcoded to 5. The fold count is not reachable by
users. A default of 5 is sensible, but larger K is sometimes preferable (more
training data per fold, finer aggregation), and there is no way to set it
without editing the package source.

Today the value 5 is hardcoded in **two** places that must agree:

1. The inner closure default: `rate_sequential <- function(X, Y, W, num.folds = 5)`
   (`R/omni_hetero.R:257`), which is never overridden -- the call site
   (`R/omni_hetero.R:455`) is `rate_sequential(X, Y, W)`.
2. A duplicate literal `.seq.num.folds <- 5L` (`R/omni_hetero.R:441`) used only
   for the upfront sample-size warning (`n / num.folds`).

These two magic 5s are a latent inconsistency bug: change one, the other
silently disagrees.

## Goal

Expose a single user-facing `num.folds` knob on `omni_hetero()` (and, by
forwarding, on `summary.causal_forest()`), default 5, fully validated, with the
upfront size warning scaled by the actual fold count.

## Decisions

| Decision | Choice | Rationale |
|----------|--------|-----------|
| Parameter name | `num.folds` | Matches grf convention, the existing internal closure arg, and the current `min_fold_n` roxygen text (which already references `num.folds`). Least churn. |
| Lower bound | `stop()` if `< 3` | The fold loop runs `k in 2:num.folds`, i.e. `num.folds - 1` attempts. The aggregation requires `>= 2` usable folds, so `num.folds = 2` *always* returns `NA`. Erroring at `< 3` fails loud and early. Docs recommend `>= 5`. |
| Upper bound | `stop()` if `> n` | `sample(rep(1:num.folds, length = nrow(X)))` leaves high fold-ids unassigned when `num.folds > n`, so `samples.by.fold[[k]]` is missing and the loop crashes (subscript error, not a clean `NA`). A hard guard prevents the crash. |
| `summary()` reach | Forward via `...` | `summary.causal_forest()` already has `...` (currently unused) and calls `omni_hetero(object, seed = seed)`. Forwarding makes `summary(cf, num.folds = 10)` work with one knob everywhere. |

## Changes

### `R/omni_hetero.R`

1. **Signature** (append, no positional break):
   ```r
   omni_hetero <- function(c.forest, seed = 1995, min_fold_n = 100, num.folds = 5)
   ```

2. **Validation** immediately after the existing `inherits(c.forest, ...)` check.
   Validate against `nrow(c.forest$X.orig)`:
   ```r
   if (!is.numeric(num.folds) || length(num.folds) != 1L ||
       !is.finite(num.folds) || num.folds != round(num.folds) ||
       num.folds < 3) {
     stop("num.folds must be a single integer >= 3 (5 or more recommended).",
          call. = FALSE)
   }
   num.folds <- as.integer(num.folds)
   if (num.folds > nrow(c.forest$X.orig)) {
     stop("num.folds (", num.folds, ") cannot exceed the number of ",
          "observations (", nrow(c.forest$X.orig), ").", call. = FALSE)
   }
   ```

3. **Upfront size warning** (line ~441): delete the duplicate
   `.seq.num.folds <- 5L`; use `num.folds` directly so `n / num.folds` reflects
   the user's choice.

4. **Call site** (line ~455):
   ```r
   rate_result <- rate_sequential(X, Y, W, num.folds = num.folds)
   ```
   The inner closure keeps its own `num.folds = 5` default as a harmless
   fallback; it is now always passed explicitly.

5. No change to the `sum(t) / sqrt(num.folds - 1)` aggregation, the per-fold
   degeneracy guards, or the `num_folds` payload field (already populated from
   the closure arg and consumed by `plot_rate.R` for the K-label).

### `R/summary.causal_forest.R`

- Line ~71: `omni_hetero(object, seed = seed)` -> `omni_hetero(object, seed = seed, ...)`.
- `summary.causal_forest(object, seed = 1995, ...)` already declares `...`; it is
  currently dropped. Forwarding it is the only behavior change.

### Docs

- roxygen `@param num.folds` on `omni_hetero()`: number of folds K for the
  Sequential RATE cross-validation (Wager, 2024). Integer `>= 3`; default 5,
  `>= 5` recommended. Larger K gives each per-fold CATE forest more training
  data, but more folds at small n raises the risk of degenerate folds (see
  `min_fold_n`).
- roxygen `@param ...` on `summary.causal_forest()`: forwarded to
  `omni_hetero()` (e.g. `num.folds`, `min_fold_n`).
- `NEWS.md`: entry under the development version.
- `devtools::document()` regenerates `man/` and feeds pkgdown.

### Tests (`tests/testthat/test-omni_hetero.R`)

Extend the existing patterns (`min_fold_n` formals test is the template):

1. **Exposed**: `"num.folds" %in% names(formals(omni_hetero))`; default `== 5`.
2. **Validation errors**: `expect_error()` for `num.folds = 2`, `0`, `5.5`,
   `c(3, 4)`, `"5"`, and `num.folds = nrow + 1` (over-n guard).
3. **Respected**: adequate-n forest with `num.folds = 3` ->
   `attr(result, "rate")$num_folds == 3`, and the per-fold data frame has at
   most `num.folds - 1` rows.
4. **summary passthrough**:
   `attr(summary(cf, num.folds = 3)$heterogeneity, "rate")$num_folds == 3`.
5. **Regression**: default behavior at `num.folds = 5` unchanged; existing
   snapshots unaffected.

## Verification

`devtools::document()` -> `devtools::test()` -> `lintr::lint_package()` ->
`devtools::check()`.

## Out of scope (YAGNI)

- No change to the RATE aggregation formula or fold-independence handling.
- No change to `min_fold_n` semantics.
- No new plot logic (`plot_rate.R` already reads `num_folds` from the payload).
- No change to the cross-fit High/Low half-sample split (driven by `seed`, not
  `num.folds`).

## Files touched

- `R/omni_hetero.R`
- `R/summary.causal_forest.R`
- `tests/testthat/test-omni_hetero.R`
- `NEWS.md`
- regenerated `man/omni_hetero.Rd`, `man/summary.causal_forest.Rd`
