# cf_loco(): screen simplification + correlation diagnostic

**Date:** 2026-06-23
**Function:** `cf_loco()` in `R/cf_loco.R` (helper `compute_vimp()` unchanged)
**Status:** approved design, ready for implementation plan

## Motivation

1. The `screen = TRUE` auto-screen rule (keep split-frequency importance above the
   *mean*) over-selects: in practice it drops too many genuinely-relevant
   covariates. The forest's split-frequency importance is exactly `0` only for
   covariates the trees never split on; those are the safe ones to screen out.
2. `cf_loco` conditions on the supplied covariates. When covariates are
   correlated, LOCO importance is divided among them and can understate each
   one's individual relevance. Users currently get no signal that this is
   happening. We want a console-visible correlation diagnostic.

## Change 1 — screen simplification

### Behavior
- `screen` argument unchanged in type: `FALSE` (default) | `TRUE` | positive integer `k`.
- **Remove** the interactive cost-prompt branch entirely
  (`else if (isFALSE(screen) && interactive())`, current cf_loco.R lines 213-233,
  including the `readline()` call). The old "prompt the user to screen on a big
  job" idea is dropped -- it was a bad UX.
- `grf::variable_importance()` is computed **only** when `!isFALSE(screen)`.
  Plain default runs (`screen = FALSE`) never call it, interactive or not.
- `screen = TRUE`: new rule `keep <- vi.group > 0` -- drop only covariates (groups)
  whose max split-frequency importance is exactly zero. Safety: if every group is
  zero (`sum(keep) == 0`), keep all and emit a message noting nothing was
  screened. Message lists the kept variables (as today).
- `screen = k` (integer top-k): **unchanged**.
- Screened-out groups still receive importance `0` in the assembled output (as today).

### Rationale
Zero split-frequency = the forest never used the covariate. Screening only those
is a conservative, defensible reduction in the number of expensive LOCO refits and
will not drop covariates the forest actually relied on.

## Change 2 — correlation diagnostic

### New argument
Add `verbose = TRUE` as the **last** formal:

```r
cf_loco <- function(c.forest, variable.groups = NULL, group.by.corr = FALSE,
                    corr.threshold = 0.5, normalize = FALSE, screen = FALSE,
                    stabilize = 1e-6, seed = 1995, verbose = TRUE)
```

### Behavior
Emitted **after** the `p + 1` refits, just before constructing the return object
(actionable-before-the-wait was considered and rejected; user wants it after).

When `verbose = TRUE` and `p > 1`:
1. Compute `cm <- stats::cor(X)` on the conditioning covariates (`c.forest$X.orig`).
   Constant columns yield `NA` and are tolerated.
2. Print the rounded matrix `round(cm, 2)` to the console, preceded by a short
   header (e.g. `message("Conditioning-variable correlation matrix:")`).
   Column/row labels fall back to `paste0("V", seq_len(p))` when `X` is unnamed.
3. Detect off-diagonal pairs with `abs(r) > 0.5` (fixed threshold, **independent**
   of `corr.threshold`; NA-safe). For each such pair, if grouping is **not**
   active, raise a single `warning()` naming the pairs and explaining that LOCO
   importance is divided among correlated covariates and may understate their
   individual relevance; suggest `group.by.corr = TRUE` or joint interpretation.

When `verbose = FALSE`: no matrix print and no correlation warning.

### Grouping interaction
When grouping is active -- `group.by.corr = TRUE` **or** `variable.groups` supplied --
the user has already addressed correlated covariates. In that case **suppress the
warning** but **still print the matrix** (if `verbose = TRUE`).

### Edge cases
- `p == 1`: skip the diagnostic entirely (no pairs, 1x1 matrix).
- Unnamed `X`: synthesize `V1..Vp` labels for printing and pair naming.
- Constant covariate: `cor` returns `NA`; excluded from the `> 0.5` check, shown as
  `NA` in the printed matrix.

### Warning copy (professional)
> cf_loco: the following conditioning variables are correlated above |r| = 0.5:
> X1 & X2 (r = 0.62); X3 & X5 (r = -0.71). LOCO importance is divided among
> correlated covariates and may understate their individual relevance. Consider
> grouping them via `group.by.corr = TRUE` or interpreting their importances
> jointly.

(`call. = FALSE`.)

## Backwards compatibility

- `verbose` added at the **end** of the signature -- signature, return structure,
  and S3 class are all unchanged -> not an API break per the package contract.
- **Behavior change flagged:** `verbose = TRUE` default means existing callers now
  see a printed correlation matrix and (when applicable) a new warning. This is a
  deliberate, Jack-approved behavior change, not a silent one.
- **API-stability guard** `T-API: cf_loco formals unchanged`
  (`tests/testthat/test-fixes-20260511.R:123`) must be updated **intentionally** to
  add `"verbose"` to the expected formals set.
- **NEWS.md:** add an entry under the development version documenting (a) the
  `screen = TRUE` rule change + removal of the interactive prompt, and (b) the new
  `verbose` correlation diagnostic.

## Tests

New/updated in `tests/testthat/` (extend `test-cf_loco-normalize.R` or add
`test-cf_loco-screen-corr.R`):

1. **screen = TRUE keeps only non-zero split-freq covs** -- fit a cf with an
   obviously-irrelevant covariate; assert output has `p` rows, screened-out covs
   have importance `0`, a clearly-relevant covariate is retained.
2. **screen = TRUE all-zero safety** -- degenerate case keeps all + emits message.
3. **screen = k** -- unchanged top-k still works (regression guard).
4. **no interactive prompt** -- `screen = FALSE` path issues no `readline`
   (structurally: the branch is gone).
5. **verbose = TRUE on correlated X** -- `expect_warning(.., "correlated")`; matrix
   printed (capture via `capture.output`).
6. **verbose = FALSE** -- `expect_no_warning()` and no matrix output on the same
   correlated X.
7. **grouping suppresses warning** -- `group.by.corr = TRUE` (or `variable.groups`)
   on correlated X with `verbose = TRUE` -> `expect_no_warning()`, matrix still printed.
8. **p == 1** -- no diagnostic, no error.
9. **API formals guard** -- updated to include `verbose`.

## Docs

- roxygen: add `@param verbose`; rewrite `@param screen` to describe the new
  zero-importance rule and drop the interactive-prompt language; update the
  `screen = TRUE` `\item` ("above the mean" -> "with non-zero split-frequency
  importance"). Run `devtools::document()` to regenerate `man/cf_loco.Rd`.
- `NEWS.md`: development-version entry (see Backwards compatibility).

## Out of scope

- `compute_vimp()` -- no change.
- The debiasing math, refit hyperparameter passthrough, normalization,
  `stabilize` floor -- all unchanged.
- `group.by.corr` grouping algorithm and its `corr.threshold` -- unchanged
  (the new diagnostic uses a separate fixed 0.5).
