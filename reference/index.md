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
- [`cf_perm()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/cf_perm.md)
  : PermuCATE Variable Importance for Causal Forests
- [`loco()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/loco.md)
  : LOCO Variable Importance for Outcome Models

## Heterogeneity Tests

Omnibus battery of treatment effect heterogeneity tests.

- [`omni_hetero()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/omni_hetero.md)
  : Omnibus Tests of Heterogeneity
- [`print(`*`<omni_hetero>`*`)`](https://cetialphafive.github.io/UtopiaPlanitia/reference/print.omni_hetero.md)
  : Print Omnibus Heterogeneity Tests
- [`plot(`*`<omni_hetero>`*`)`](https://cetialphafive.github.io/UtopiaPlanitia/reference/plot.omni_hetero.md)
  : Plot an Omnibus Heterogeneity Test Object

## Nuisance Estimation

Causal forest refits with alternative nuisance estimators (TabPFN,
cv.glmnet, autoML pool).

- [`tabcf()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/tabcf.md)
  : Causal Forest with TabPFN-Estimated Nuisances
- [`glmcf()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/glmcf.md)
  : Causal Forest with cv.glmnet-Estimated Nuisances
- [`autocf()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/autocf.md)
  : Auto-Selected Nuisance Causal Forest
- [`setup_tabpfn_token()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/setup_tabpfn_token.md)
  : Set up the TABPFN_TOKEN environment variable

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

## S3 Methods for cf_loco

Print, summary, and plot methods for LOCO results.

- [`summary(`*`<cf_loco>`*`)`](https://cetialphafive.github.io/UtopiaPlanitia/reference/summary.cf_loco.md)
  : Summarize LOCO Variable Importance
- [`print(`*`<cf_loco>`*`)`](https://cetialphafive.github.io/UtopiaPlanitia/reference/print.cf_loco.md)
  : Print LOCO Variable Importance
- [`plot(`*`<cf_loco>`*`)`](https://cetialphafive.github.io/UtopiaPlanitia/reference/plot.cf_loco.md)
  : Plot LOCO Variable Importance

## S3 Methods for cf_perm

Print, summary, and plot methods for PermuCATE results.

- [`summary(`*`<cf_perm>`*`)`](https://cetialphafive.github.io/UtopiaPlanitia/reference/summary.cf_perm.md)
  : Summarize PermuCATE Variable Importance
- [`print(`*`<cf_perm>`*`)`](https://cetialphafive.github.io/UtopiaPlanitia/reference/print.cf_perm.md)
  : Print PermuCATE Variable Importance
- [`plot(`*`<cf_perm>`*`)`](https://cetialphafive.github.io/UtopiaPlanitia/reference/plot.cf_perm.md)
  : Plot PermuCATE Variable Importance
