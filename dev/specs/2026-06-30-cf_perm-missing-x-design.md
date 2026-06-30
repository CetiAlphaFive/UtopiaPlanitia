# Design: cf_perm observed-support missingness handling (rev 2)

**Date:** 2026-06-30
**Feature slug:** `cf_perm-missing-x`
**Paired plan:** `dev/plans/2026-06-30-cf_perm-missing-x.md`
**Decisions:** locked rev 2 — confront-on-missingness; no-NA keeps old code path
verbatim; scope merged into `allow.missing`; surface via printed table.

## 1. Public API change

One appended argument (last position):

```r
cf_perm <- function(c.forest, loss = c("R", "AIPW"), n.perm = 50L,
                    cross.fit = FALSE, num.folds = 5L, screen = FALSE,
                    normalize = FALSE, conf.level = 0.95, seed = 1995,
                    verbose = TRUE, allow.missing = FALSE)
```

`allow.missing` accepts `FALSE` (default) or one of `c("observed", "marginal")`.
Validation:

```r
if (!identical(allow.missing, FALSE)) {
  allow.missing <- match.arg(allow.missing, c("observed", "marginal"))
}
```

(`FALSE` stays a logical sentinel; any non-`FALSE` value must match
`"observed"`/`"marginal"`, else a clear `match.arg` error.) No `missing.scope`
arg. No other signature/default/order change.

Define the active flags:

```r
have.na  <- anyNA(X)
use.obs  <- have.na && !identical(allow.missing, FALSE)   # observed-support active
scope    <- if (use.obs) allow.missing else NA_character_  # "observed"/"marginal"
```

## 2. Gate change (`cf_perm.R:150-152`) — confront the user

```r
if (anyNA(c.forest$X.orig) && identical(allow.missing, FALSE)) {
  stop(
    "cf_perm: X contains missing values.\n",
    "  cf_perm scores importance on the observed support of each covariate and lets\n",
    "  NA rows pass through to the forest's MIA routing. Set `allow.missing` to choose\n",
    "  how rows with missing X_j are averaged:\n",
    '    "observed" - average over observed rows only; importance conditional on X_j\n',
    "                 observed (no missingness discount).\n",
    '    "marginal" - average over all rows; NA rows contribute exactly 0, so the score\n',
    "                 is auto-discounted by the covariate's missingness rate.\n",
    "  See ?cf_perm.",
    call. = FALSE
  )
}
```

Existing test `test-cf_perm.R:54-58` must be updated: it currently matches
`"does not support missing values"`; change to match the new message (e.g.
`regexp = "allow.missing"`).

## 3. `.cf_perm_cp_sample` — gated observed-support body (cf_perm.R:19-48)

Add an `obs.support = FALSE` flag. When `FALSE`, run the **original body verbatim**
(probability_forest discrete branch included) — this preserves complete-data
behavior exactly. When `TRUE`, run the observed-support, regression_forest body.

```r
.cf_perm_cp_sample <- function(j, Xfit, Xapply, n.perm, disc.max = 10L,
                               seed = 1995, obs.support = FALSE, min.obs = 5L) {
  xj.fit <- Xfit[, j]; Xm.fit <- Xfit[, -j, drop = FALSE]
  xj.app <- Xapply[, j]; Xm.app <- Xapply[, -j, drop = FALSE]
  na <- nrow(Xapply)

  if (!obs.support) {
    ## ---- ORIGINAL PATH (unchanged): probability_forest for discrete ----
    is.disc <- all(xj.fit == round(xj.fit)) && length(unique(xj.fit)) <= disc.max
    out <- matrix(NA_real_, na, n.perm)
    if (!is.disc) {
      rf <- grf::regression_forest(Xm.fit, xj.fit, seed = seed)
      nu <- stats::predict(rf, Xm.app)$predictions
      e  <- xj.app - nu
      for (k in seq_len(n.perm)) out[, k] <- nu + e[sample.int(na)]
    } else {
      fy <- factor(xj.fit)
      pf <- grf::probability_forest(Xm.fit, fy, seed = seed)
      probs <- stats::predict(pf, Xm.app)$predictions
      lev <- as.numeric(colnames(probs))
      for (k in seq_len(n.perm))
        out[, k] <- lev[apply(probs, 1L, function(pr) sample.int(length(lev), 1L, prob = pr))]
    }
    return(out)
  }

  ## ---- OBSERVED-SUPPORT PATH: regression_forest, NA-aware ----
  obs.fit <- !is.na(xj.fit); obs.app <- !is.na(xj.app)
  out <- matrix(NA_real_, na, n.perm)            # NA rows stay NA by construction
  if (sum(obs.fit) < min.obs) return(out)        # degenerate -> caller sets j to 0

  xj.o    <- xj.fit[obs.fit]
  lev.o   <- sort(unique(xj.o))
  is.bin  <- length(lev.o) == 2L && all(lev.o == round(lev.o))
  idx.app <- which(obs.app)

  if (is.bin) {
    lo <- lev.o[1]; hi <- lev.o[2]
    rf <- grf::regression_forest(Xm.fit[obs.fit, , drop = FALSE],
                                 as.numeric(xj.o == hi), seed = seed)
    p  <- pmin(pmax(stats::predict(rf, Xm.app[obs.app, , drop = FALSE])$predictions, 0), 1)
    for (k in seq_len(n.perm))
      out[idx.app, k] <- ifelse(stats::runif(length(p)) < p, hi, lo)
  } else {
    rf <- grf::regression_forest(Xm.fit[obs.fit, , drop = FALSE], xj.o, seed = seed)
    nu <- stats::predict(rf, Xm.app[obs.app, , drop = FALSE])$predictions
    e  <- xj.app[obs.app] - nu
    no <- length(e)
    for (k in seq_len(n.perm)) out[idx.app, k] <- nu + e[sample.int(no)]
  }
  out
}
```

Notes:
- `obs.support = FALSE` reproduces the original function exactly (the no-missing
  default). `test-cf_perm.R:16-23` (continuous) and `:25-32` (binary discrete,
  probability_forest) pass unchanged.
- On the observed-support branch, `Xm.*` (`X_{-j}`) may carry `NA`; passed to
  `regression_forest`, routed via MIA. `probability_forest` is never reached on
  this branch.
- `min.obs` (default `5L`) is the fit-degeneracy threshold; documented in
  `@details`.

### 3.1 Pathological columns (observed-support branch only)

- All-`NA` or `< min.obs` observed `X_j`: returns an all-`NA` matrix. Caller
  detects `all(is.na(Xperm))`, short-circuits `j` -> importance `0`, p-value `1`,
  one-time warning naming the covariate.

## 4. Scoring paths

Call site passes the flag: `.cf_perm_cp_sample(j, ..., obs.support = use.obs)`.

### 4.1 Light path (`cf_perm.R:214-240`)

`predict(c.forest, Xk)` already routes `NA` via MIA — **predict call unchanged**.
For an `NA`-`X_j` row, `Xk` equals `X`, so `tau.k == tau.base` and `dbar[i]` is
**exactly 0**. Averaging respects `scope` (only matters when `use.obs`):

```r
obs <- if (use.obs) !is.na(X[, j]) else rep(TRUE, n)
sel <- if (identical(scope, "observed")) obs else rep(TRUE, n)
imp[j]  <- mean(dbar[sel])
se[j]   <- stats::sd(dbar[sel]) / sqrt(sum(sel))
z[j]    <- imp[j] / se[j]
pval[j] <- stats::pnorm(z[j], lower.tail = FALSE)
ci.lo[j] <- imp[j] - zc * se[j]; ci.hi[j] <- Inf
```

When `!use.obs`, `sel = all` and this is the original
`mean(dbar)` / `sd(dbar)/sqrt(n)`.

### 4.2 Cross-fit path (`.cf_perm_cv`, cf_perm.R:295-370)

- `causal_forest`/`regression_forest` refits on `X[tr, ]`: MIA-native. No change.
- Pass `obs.support = use.obs` to `.cf_perm_cp_sample(j, Xtr, Xte, ...)`.
- Per-fold importance honors `scope` over the test fold:
  `sel.te <- if (scope == "observed") !is.na(Xte[, j]) else TRUE`;
  `Psi[f, j] <- mean((dbar / n.perm)[sel.te])`.
- Degenerate `j` in a fold (`< min.obs` train labels or empty `sel.te`):
  `Psi[f, j] <- NA_real_`; aggregate with `na.rm = TRUE` (mean + var); a covariate
  degenerate in every fold -> importance 0, p 1, warn. Nadeau-Bengio `rho`, df,
  `pt`/`qt` unchanged. `use.obs` must be threaded into `.cf_perm_cv`'s signature.

`W` stratified fold guards (`cf_perm.R:177-184`) unchanged.

## 5. Surfacing missingness (object element + printed table)

In `cf_perm()`: `miss.rate <- colMeans(is.na(X))` (named by `vnames`). Add to the
returned object as a top-level element `miss.rate`. **No `vimp` column** -> `vimp`
structure and `test-cf_perm.R:39-40` untouched.

`R/cf_perm_methods.R`, `print.cf_perm` / `summary.cf_perm`: when
`any(x$miss.rate > 0)`, render a compact table for the affected covariates, e.g.

```
Missing covariate values (observed-support permutation; scope = observed):
  Variable   Miss.rate   n.missing
  X2            0.120          60
  X4            0.400         200
```

(`n.missing = round(miss.rate * n)`.) Suppressed entirely when no missingness, so
complete-data printing is unchanged. The header names the active `scope`.

## 6. Inference caveats to document (`@details`)

- Light-path SEs already approximate; under `"marginal"`, exact-zero `NA` rows
  shrink the SE -> p-values are importance-conditional-on-design.
- Cross-variable magnitude comparison is fair only after accounting for
  per-variable missingness; hence the printed table.
- Selected-sample bias under MNAR / MAR-on-CATE-drivers: observed-subpopulation
  residuals may not represent missing rows. Point to the `M_j` upgrade (Section 9).

## 7. Tests

`tests/testthat/test-cf_perm.R` (existing):
- **L16-23, L25-32:** unchanged (no-NA -> `obs.support = FALSE`, original path).
- **L39-40 (`expect_named`):** unchanged (no `vimp` column).
- **L54-58 (NA rejected):** update `regexp` to the new message (e.g.
  `"allow.missing"`); still expects an error at default `FALSE`.

New `tests/testthat/test-cf_perm-missing.R`:
1. **Gate message:** `NA` + `allow.missing = FALSE` -> error mentioning
   `"observed"` and `"marginal"`.
2. **Complete-data equivalence:** no `NA`; `allow.missing = "observed"` vs `FALSE`
   -> identical results (with no `NA`, `use.obs = FALSE`, so identical by
   construction).
3. **Runs with NA, continuous:** `NA` in an important continuous covariate ->
   finite importances, no error.
4. **Exact-zero NA rows:** white-box `.cf_perm_cp_sample(obs.support = TRUE)`
   returns `NA` for `NA` apply rows; manual `predict` comparison -> zero per-row
   delta.
5. **Binary with NA:** binary covariate with `NA` -> Bernoulli branch; non-NA
   emissions in `{lo,hi}`.
6. **Multi-level discrete with NA:** runs via residual-shuffle.
7. **All-NA / `< min.obs` covariate:** importance 0, p 1, warning.
8. **Cross-fit with NA:** `cross.fit = TRUE`, `loss = "R"` -> runs; fold-degenerate
   covariate handled.
9. **Scope direction:** `"marginal"` vs `"observed"` on a high-missingness
   covariate -> marginal magnitude `<=` observed (auto-discount); identical when no
   `NA`.
10. **`miss.rate`:** equals `colMeans(is.na(X))`; printed table appears iff any
    `> 0` (snapshot or text match on `print`).
11. **AIPW + missing** and **screen + missing:** run.

Gate slow Monte Carlo checks behind `UTOPIA_RUN_SLOW_TESTS=1` + `skip_on_cran()`.

## 8. API guard + docs + NEWS

- **API guard:** no `cf_perm` formals guard exists. **Add**
  `test_that("T-API: cf_perm formals unchanged", ...)` in
  `tests/testthat/test-fixes-20260511.R` pinning the arg vector incl.
  `allow.missing`.
- **roxygen:** `@param allow.missing` (document `FALSE`/`"observed"`/`"marginal"`
  and the estimand difference); `@details` on observed-support, the conditional
  `regression_forest` switch (missingness branch only), and the printed
  missingness table; `@return` note for the new `miss.rate` element. Run
  `devtools::document()` (no new exports).
- **NEWS.md** (development-version heading): new `allow.missing` arg
  (observed-support handling, user-confronting gate, `"observed"`/`"marginal"`
  scopes); printed missingness table; note that the `regression_forest` switch is
  scoped to the missingness branch so **complete-data results are unchanged**.

## 9. Deferred / future work (non-goal)

**Missingness-indicator `M_j`.** Add `M_j = is.na(X_j)` as its own covariate and
permute the `(value, indicator)` pair so the importance of *being missing* is
captured rather than discarded — the principled MNAR upgrade. Out of scope; track
separately.

## 10. Risk summary

- **Complete-data behavior:** provably unchanged — `use.obs = FALSE` routes to the
  original `.cf_perm_cp_sample` body; no `vimp` column; gate inert without `NA`.
- **Estimand drift under informative missingness:** mitigated by the explicit
  `"observed"`/`"marginal"` choice and the printed missingness table.
- **`regression_forest` switch:** confined to the opt-in missingness branch; no
  effect on existing calls.
- **Degenerate columns:** explicit degrade-to-zero + warning, tested.
