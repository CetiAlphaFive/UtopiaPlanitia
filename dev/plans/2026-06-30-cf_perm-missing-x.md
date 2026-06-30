# cf_perm: missingness in X via observed-support conditional permutation

**Date:** 2026-06-30
**Feature slug:** `cf_perm-missing-x`
**Status:** plan — decisions locked (rev 2)
**Paired design:** `dev/specs/2026-06-30-cf_perm-missing-x-design.md`

## What

One new argument to `cf_perm()`, `allow.missing`, that confronts the user with an
explicit choice the moment `X` contains `NA`s:

- `allow.missing = FALSE` (default) — if `X` has `NA`s, **error** with a message
  telling the user to set `allow.missing` to `"observed"` or `"marginal"`, each
  explained. If `X` has no `NA`s, this is a no-op and the function runs the
  **current, unchanged** code path.
- `allow.missing = "observed"` — observed-support conditional permutation;
  average the per-row loss delta over observed rows only (importance conditional
  on `X_j` observed).
- `allow.missing = "marginal"` — observed-support conditional permutation;
  average over all `n` (auto-discounted by the missingness rate).

When (and only when) `X` has `NA`s and the user opts in, the conditional model in
`.cf_perm_cp_sample` switches `probability_forest -> regression_forest` (MIA-
friendly; grf's binary-nuisance default), and the printed output gains a small
per-covariate missingness table.

**No behavior change whatsoever when `X` is complete.** Default `FALSE` +
complete data = today's function, byte-for-byte (probability_forest and all).

## Why

`grf` causal forests handle missing covariates natively via MIA (Missingness
Incorporated in Attributes): an `NA` is a routable value, not a row to drop.
`cf_perm()` currently refuses any `NA` (`cf_perm.R:150-152`). Imputation is the
wrong escape — it discards the MIA handling the forest was fit with and biases
importances toward whatever the imputer fills in. Keep MIA; solve only the part
that cannot tolerate `NA`, and make the user choose the estimand consciously.

### The two NA problems (look like one, are not)

1. **`NA`s in conditioning features `X_{-j}`** (predictors of
   `nu_hat_j = E[X_j | X_{-j}]`). **Not a problem** — `nu_hat_j` is a `grf` forest,
   eats `NA` predictors via MIA. No imputation.
2. **`NA`s in `X_j` as the regression _label_.** **The real blocker** — no MIA for
   a response; cannot regress, residualize, or shuffle what you never observed.

### Minimal fix (observed-support)

Per covariate `j`: `obs <- !is.na(X[, j])`; fit `nu_hat_j` on observed-label rows
only (keep `X_{-j}` `NA`s, MIA handles them); residualize / draw on `obs` rows;
splice perturbed values back into `obs` rows; leave `NA` rows as `NA`; re-score the
CF on the full frame. For an `NA`-`X_j` row the whole feature vector is unchanged
pre/post permutation, so its loss delta is **exactly 0** — correct, you cannot
break a dependence you never observed.

## Confronting the user (the gate)

```
Error: cf_perm: X contains missing values.
  cf_perm scores variable importance on the observed support of each covariate and
  lets NA rows pass through to the forest's MIA routing. Choose how to average the
  per-row importance over rows with missing values by setting `allow.missing`:
    "observed" - average over observed rows only; importance is conditional on
                 X_j being observed (no missingness discount).
    "marginal" - average over all rows; NA rows contribute exactly 0, so the score
                 is auto-discounted by the covariate's missingness rate.
  See ?cf_perm. (Default allow.missing = FALSE preserves the no-missing behavior.)
```

## Conditional forest switch (only under missingness)

`probability_forest -> regression_forest` happens **only** on the observed-support
branch (i.e. `X` has `NA`s and the user opted in). With complete data the original
`.cf_perm_cp_sample` body runs unchanged. On the missingness branch:

- **Binary `X_j`** (2 observed levels): `regression_forest` predicts
  `p_hat = P(X_j = hi | X_{-j})`; draw `Bernoulli(clip(p_hat, 0, 1))` -> stays on
  `{lo,hi}` support; matches grf's binary `W.hat` default; sim-validated.
- **Continuous / multi-level discrete `X_j`**: `regression_forest`
  residual-shuffle.

Because the switch is scoped to the opt-in missingness branch, there is **no
numeric change for any existing complete-data call**.

## Surfacing missingness (a small table in the output)

The returned object gains a `miss.rate` element (named numeric, length `p`,
`colMeans(is.na(X))`). When any `> 0`, `print.cf_perm` / `summary.cf_perm` render a
small per-covariate missingness table (variable, missing rate, n missing) so a
reader sees which scores sit on thin support. **No new `vimp` column** -> the
existing `vimp` structure and its tests are untouched.

## Scope

- New arg `allow.missing = FALSE` accepting `FALSE | "observed" | "marginal"`.
- Gate rewritten to confront the user with the choice (message above).
- Observed-support conditional permutation in `.cf_perm_cp_sample`, gated by an
  `obs.support` flag; original body preserved verbatim for the no-missing path.
- `probability_forest -> regression_forest` **only** on the observed-support branch.
- `miss.rate` object element + missingness table in `print`/`summary`.
- Graceful degradation: all-`NA` / too-few-observed columns -> importance 0, p 1,
  warning.
- Both paths (light + `.cf_perm_cv` cross-fit). Tests, NEWS, roxygen, `man/`.

## Non-goals

- **Imputation.** Never.
- **Missingness-indicator covariate `M_j`** (the MNAR upgrade: add
  `M_j = is.na(X_j)`, permute the (value, indicator) pair so the importance of
  *being missing* is captured). Deferred; tracked separately.

## Decisions (locked 2026-06-30, rev 2)

1. **Confront on missingness** — gate errors with a message instructing the user
   to set `allow.missing` to `"observed"` or `"marginal"`, each explained.
2. **No NA -> old version** — default `FALSE` + complete data runs the current
   code path byte-for-byte (probability_forest included). The `regression_forest`
   switch is scoped to the opt-in missingness branch only.
3. **Scope merged into `allow.missing`** — values `"observed"` (default intent)
   and `"marginal"`; no separate `missing.scope` arg.
4. **Surface via a printed table**, driven by a `miss.rate` object element; no
   `vimp` column.

## Test impact (now minimal)

- `test-cf_perm.R:25` (binary discrete): **unchanged** (no-NA -> old
  probability_forest path).
- `test-cf_perm.R:39-40` (`expect_named` 7 cols): **unchanged** (no `vimp` column
  added).
- `test-cf_perm.R:54-58` (NA rejected at default): **update** to assert the new,
  more informative error message ("set `allow.missing`").
- New `tests/testthat/test-cf_perm-missing.R` for the opt-in branch, both scopes,
  `miss.rate`, degenerate columns, cross-fit, print table.

## Acceptance

- `allow.missing = FALSE` + `NA` -> error naming `"observed"`/`"marginal"`.
- `allow.missing = FALSE` + no `NA` -> identical to current release.
- `allow.missing = "observed"/"marginal"` + `NA`s -> runs; finite importances;
  `NA` rows contribute exactly 0; missingness table printed; `miss.rate` correct.
- Binary + continuous + multi-level discrete, light + cross-fit, all work.
- Pathological columns degrade with a warning, not an error.
- New `NEWS.md` entry; `man/` regenerated; `devtools::check()` clean.
