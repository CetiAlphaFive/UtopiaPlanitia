# plot_pdp(): subgroup ATE mode for discrete covariates

**Date:** 2026-06-25
**Function:** `plot_pdp()` in `R/plot_pdp.R` (new internal helpers; public API gains one trailing formal)
**Status:** approved design, ready for implementation plan

## Motivation

For a binary or low-cardinality integer covariate, the existing PDP draws a smooth
partial-dependence line across the covariate's range. That is misleading: it
interpolates model predictions between levels that do not exist (e.g. a "0.5" value
of a 0/1 treatment-eligibility flag) and it reports model-marginalized predictions
rather than design-honest subgroup effects.

The fix is an opt-in mode that, for each observed value of the covariate, reports the
doubly-robust (AIPW) subgroup average treatment effect computed on the actual subset
of units at that value, via `grf::average_treatment_effect(c.forest, subset = ...)`.
This surfaces real subgroup heterogeneity with valid uncertainty, instead of a smooth
curve through non-existent intermediate levels.

## Public API change

Add a single new formal at the **end** of the signature (backwards-compatible per the
package API contract; `subgroup = FALSE` reproduces current behavior exactly):

```r
plot_pdp <- function(c.forest, x_var, y_var = NULL,
                     grid_size = 50, n_max = 2000,
                     show_ate_region = TRUE, show_scatter = TRUE,
                     trim = TRUE,
                     x.limits = NULL, y.limits = NULL,
                     color.var = NULL, color.cat = NULL,
                     color.lab = NULL, xlab = NULL,
                     num.threads = NULL,
                     subgroup = FALSE)
```

`subgroup = FALSE` (default): existing PDP dispatch, unchanged.
`subgroup = TRUE`: discrete subgroup-ATE mode (1-way if `y_var = NULL`, else 2-way).

Reachable via the S3 dispatcher for free: `plot.causal_forest(type = "pdp", ...)`
forwards `...` to `plot_pdp()`, so `plot(cf, type = "pdp", subgroup = TRUE)` works
with no change to `plot.causal_forest.R`.

## Dispatch

In `plot_pdp()`, after the existing input checks, branch before the current
1-way/2-way dispatch:

```r
if (subgroup) {
  if (is.null(y_var)) {
    return(.plot_subgroup_1way(c.forest, x_var, show_ate_region,
                               y.limits, xlab))
  } else {
    return(.plot_subgroup_2way(c.forest, x_var, y_var, xlab))
  }
}
# ... existing PDP dispatch unchanged ...
```

The `color.var`/`y_var` mutual-exclusion check and the variable-existence checks
already in `plot_pdp()` run first and still apply.

## Level enumeration (no detection, no binning)

Per design decision: trust the values. Levels are `sort(unique(.col_vec(X, var)))`
on `c.forest$X.orig`, dropping `NA`. No cardinality cap, no auto-binning, no
integer/continuous detection. If a user passes a continuous covariate they get one
subgroup per distinct value (and most will fail the small-subset guard below, which
is acceptable and self-explaining via warnings).

## Computation — internal helper `.subgroup_ate()`

A single helper computes the per-subset estimates for both 1-way and 2-way callers:

- Input: the forest, a logical `subset` vector (or a list of them with labels).
- For each subset: `tryCatch(grf::average_treatment_effect(c.forest, subset = s))`,
  default `target.sample = "all"`, AIPW doubly-robust. Extract `estimate` and
  `std.err`.
- **Robustness:** any subset that errors (grf raises on too-few units / no treatment
  variation) or has fewer usable rows than grf requires is caught, a `warning()` is
  emitted naming the skipped level(s)/cell, and that level is dropped from the result.
- If **zero** levels/cells survive, `stop()` with an informative message (e.g.
  "no subgroup had enough units to estimate an ATE").

Returned per surviving level/cell: the level value(s), `estimate`, `std.err`,
`lower = estimate - 1.96*std.err`, `upper = estimate + 1.96*std.err`,
`signif = (lower > 0) | (upper < 0)`. CI level fixed at 95% (matches the existing
`show_ate_region` 1.96 convention). No estimator-tuning arguments are exposed
(YAGNI); `target.sample` may be added at the signature end in a future release.

## 1-way plot — `.plot_subgroup_1way()`

- One point per surviving level at its `estimate`; vertical error bars from `lower`
  to `upper`.
- x-axis discrete: level values as an ordered factor (sorted). x label defaults to
  `x_var`, overridable by `xlab`.
- y-axis "CATE"; `y.limits` honored if supplied.
- If `show_ate_region = TRUE`: draw the overall `grf::average_treatment_effect(cf)`
  estimate as a solid horizontal line plus dotted 95% band (same styling as the
  existing 1-way PDP ATE region).
- Reuse the existing serif / grey-panel theme block verbatim.
- Return value: a `utopia_plot` wrapping a plain `ggplot` (a classed object via the
  existing print path). **No ggMarginal** — marginal histograms are meaningless on a
  discrete subgroup axis.

## 2-way plot — `.plot_subgroup_2way()`

- Cells = cross of `sort(unique(x))` x `sort(unique(y))`; subset per cell is
  `X[,x]==vx & X[,y]==vy`. Empty / failed cells are simply absent.
- `geom_tile` with `fill = estimate` via the existing
  `scale_fill_gradient2(low = "#fd647c", mid = "#e6e6e6", high = "#3d900e",
  midpoint = 0)`.
- `geom_text` annotates each cell with the rounded `estimate`; cells where
  `signif` is `TRUE` (95% CI excludes 0) get a trailing `*` (or bold). CI **width**
  is not encoded on the tile (accepted tradeoff).
- Discrete x/y axes (factor levels); axis labels default to `x_var` / `y_var`,
  x label overridable by `xlab`. `y.limits` not applicable (discrete y); ignored.
- Return value: a `utopia_plot` wrapping a plain `ggplot`. No ggMarginal.

## Argument interaction under `subgroup = TRUE`

- **Honored:** `show_ate_region` (1-way reference band), `y.limits` (1-way CATE
  axis), `xlab`.
- **Ignored (documented in roxygen):** `grid_size`, `n_max`, `trim`, `show_scatter`,
  `color.var`, `color.cat`, `color.lab`, `x.limits`, `num.threads` — none apply to
  discrete subgroup ATEs (`grf::average_treatment_effect()` has no `num.threads`).
  Silently ignored (no warning).

## Out of scope

- No continuous-variable binning.
- No `target.sample` / estimator-method passthrough.
- No facet-based 2-way alternative (heatmap chosen).
- No changes to `plot.causal_forest.R` (free via `...`).

## Documentation / housekeeping

- roxygen: document `subgroup`; note ignored args; add an `@examplesIf` block for a
  binary covariate. Regenerate `man/plot_pdp.Rd` via `devtools::document()`.
- `NEWS.md`: development-version entry under a `plot_pdp()` heading.
- Tests: new `tests/testthat/test-pdp-subgroup.R` (1-way structure, 2-way structure,
  small-subset warning + skip, all-fail `stop()`, `subgroup = FALSE` unchanged,
  ggplot2/ggExtra skips when absent).
- Optional: add a `plot_pdp` formals guard to `test-fixes-20260511.R` (none exists
  today) pinning the new signature.

## Backwards compatibility

`subgroup` is a new trailing formal with default `FALSE` reproducing current output
exactly. No existing argument is renamed, reordered, or re-defaulted. No return-type
change for existing calls. No deprecation needed.
