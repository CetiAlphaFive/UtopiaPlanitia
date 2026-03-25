# Package index

## Causal Forest Summary

S3 methods for summarizing and plotting causal forests.

- [`summary(`*`<causal_forest>`*`)`](https://cetialphafive.github.io/UtopiaPlanitia/reference/summary.causal_forest.md)
  : Summarize a Causal Forest
- [`print(`*`<summary.causal_forest>`*`)`](https://cetialphafive.github.io/UtopiaPlanitia/reference/print.summary.causal_forest.md)
  : Print Summary of a Causal Forest
- [`plot(`*`<causal_forest>`*`)`](https://cetialphafive.github.io/UtopiaPlanitia/reference/plot.causal_forest.md)
  : Plot a Causal Forest

## Variable Importance

Leave-one-covariate-out (LOCO) importance for causal forests and ranger
models.

- [`cf_loco()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/cf_loco.md)
  : Modified LOCO Variable Importance for Causal Forests
- [`compute_vimp()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/compute_vimp.md)
  : Compute Variable Importance (Internal)
- [`loco()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/loco.md)
  : LOCO Variable Importance for Outcome Models

## Heterogeneity Tests

Omnibus battery of treatment effect heterogeneity tests.

- [`omni_hetero()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/omni_hetero.md)
  : Omnibus Tests of Heterogeneity

## Visualization

Diagnostic, partial dependence, scatter, and ranked CATE plots.

- [`plot_diag()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/plot_diag.md)
  : Model Diagnostics for Causal Forest
- [`plot_pdp()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/plot_pdp.md)
  : Partial Dependence Plot for Causal Forest
- [`plot_scatter()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/plot_scatter.md)
  : CATE Scatter Plot for Causal Forest
- [`plot_rank()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/plot_rank.md)
  [`rank_plot()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/plot_rank.md)
  : Rank Plot of Individual CATE Estimates
- [`plot_inter()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/plot_inter.md)
  **\[deprecated\]** : Interaction Plot for Causal Forest (Deprecated)

## S3 Methods for cf_loco

Print, summary, and plot methods for LOCO results.

- [`summary(`*`<cf_loco>`*`)`](https://cetialphafive.github.io/UtopiaPlanitia/reference/summary.cf_loco.md)
  : Summarize LOCO Variable Importance
- [`print(`*`<cf_loco>`*`)`](https://cetialphafive.github.io/UtopiaPlanitia/reference/print.cf_loco.md)
  : Print LOCO Variable Importance
- [`plot(`*`<cf_loco>`*`)`](https://cetialphafive.github.io/UtopiaPlanitia/reference/plot.cf_loco.md)
  : Plot LOCO Variable Importance

## Utilities

- [`print(`*`<utopia_plot>`*`)`](https://cetialphafive.github.io/UtopiaPlanitia/reference/print.utopia_plot.md)
  : Print a UtopiaPlanitia plot
