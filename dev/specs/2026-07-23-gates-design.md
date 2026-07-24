# gates() and plot_gates() — Chernozhukov GATES for causal forests

**Date:** 2026-07-23  
**Status:** approved design

## Motivation

Chernozhukov et al. (2018/2020) Sorted Group Average Treatment Effects (GATES)
partition the sample by a machine-learning proxy of heterogeneous treatment
effects and estimate group-level average effects via a weighted regression.
UtopiaPlanitia already surfaces related ideas (`omni_hetero()` BLP calibration,
`plot_pdp(subgroup = TRUE)` subgroup ATEs), but there is no dedicated GATES
workflow tied to a fitted `grf::causal_forest`.

## Public API

```r
gates(c.forest,
      quantile.cutoffs = c(0.25, 0.5, 0.75),
      HT = FALSE,
      subtract.from = c("most", "least"),
      subtracted = 1,
      monotonize = TRUE,
      conf.level = 0.95,
      seed = 1995)

plot_gates(x,
           groups = "all",
           show.ate = TRUE,
           y.limits = NULL,
           title = NULL)
```

- **`gates()`** returns class `"cf_gates"`.
- **`plot_gates()`** is a standalone exported plotter (like `plot_pdp()`), not
  an S3 `plot()` method.
- **`print.cf_gates()` / `summary.cf_gates()`** mirror `cf_perm` ergonomics.

## Estimand and proxies (causal-forest path)

On an already-fitted `causal_forest`:

| GenericML input | UtopiaPlanitia source |
| --- | --- |
| `Y` | `c.forest$Y.orig` |
| `D` | `c.forest$W.orig` (binary only) |
| `propensity_scores` | `c.forest$W.hat` |
| `proxy_BCA` | `c.forest$Y.hat` |
| `proxy_CATE` | OOB `c.forest$predictions` |
| `membership` | `.gates_quantile_group(proxy_CATE, quantile.cutoffs, seed)` |

Regression follows GenericML `GATES.classic` / `GATES.HT` (weighted OLS on
`(D - p(Z)) * 1(G_k)` with `X_1` defaulting to `B` only). Standard errors use
homoskedastic `stats::vcov()` (GenericML default `vcovHC(type = "const")`).

**Scope note:** Full GenericML uses auxiliary/main sample splitting and
median-aggregates over many splits. This implementation conditions on a single
fitted forest and uses its OOB CATE proxy — the same “score an existing forest”
pattern as `cf_perm()` / `omni_hetero()` BLP payload. Document clearly.

## Return object (`cf_gates`)

- `$groups` — data frame, one row per `G1..GK`: `estimate`, `std.err`,
  `lower`, `upper`, one-sided p-values.
- `$diff` — differenced targets per `subtract.from` / `subtracted`.
- `$ate` — overall ATE from `grf::average_treatment_effect()` for plotting.
- `$membership`, `$quantile.cutoffs`, `$HT`, `$monotonize`, `$conf.level`, `$n`,
  `$K`, metadata scalars.

## Plot

`plot_gates()` draws point + interval estimates per group on the x-axis,
optional horizontal ATE reference from `$ate`, zero line, `.utopia_pdp_theme()`,
wrapped as `utopia_plot` via `.as_utopia_plot()`.

## Restrictions

- Binary treatment only (`length(unique(W)) == 2`).
- `quantile.cutoffs` strictly in `(0, 1)`, increasing; `K = length + 1 >= 2`.
- Propensities must stay in `(0, 1)` for classic weights (error if not).
- `ggplot2` required for `plot_gates()` (`requireNamespace` guard).

## Tests

- Smoke: `gates()` on toy forest returns `cf_gates`, correct `K`, finite estimates.
- Group labels `G1..G4` for default quartiles.
- `plot_gates()` returns `utopia_plot` when ggplot2 present.
- API guard in `test-fixes-20260511.R`.
