# Changelog

## UtopiaPlanitia 0.3.1

- [`autocf()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/autocf.md)
  gains a `"bart"` candidate via
  [`dbarts::bart()`](https://rdrr.io/pkg/dbarts/man/bart.html). Default
  pool is now `c("grf", "glmnet", "xgboost", "tabpfn", "bart")`. Adapter
  runs on dbarts defaults with parallel chains
  (`nchain = nthread = dbarts::guessNumCores()`) and a deterministic
  seed; user overrides via the new `bart_args` argument.

## UtopiaPlanitia 0.3.0

- New
  [`plot_pdp()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/plot_pdp.md):
  real Friedman (2001) partial dependence plots for 1-way and 2-way CATE
  surfaces.
- New
  [`plot_scatter()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/plot_scatter.md):
  renamed from the old
  [`plot_pdp()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/plot_pdp.md),
  which plotted individual OOB CATEs vs. a covariate.
- `plot_inter()` is deprecated in favor of
  `plot_pdp(c.forest, x_var, y_var)`.
- [`plot.causal_forest()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/plot.causal_forest.md)
  gains `type = "scatter"` for the new
  [`plot_scatter()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/plot_scatter.md).

## UtopiaPlanitia 0.2.0

- Added S3 [`summary()`](https://rdrr.io/r/base/summary.html) and
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) methods for
  `causal_forest` objects.
- Added S3 [`summary()`](https://rdrr.io/r/base/summary.html),
  [`print()`](https://rdrr.io/r/base/print.html), and
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) methods for
  `cf_loco` output.
- Added
  [`plot.causal_forest()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/plot.causal_forest.md)
  dispatcher for
  [`plot_diag()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/plot_diag.md),
  [`plot_pdp()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/plot_pdp.md),
  [`rank_plot()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/plot_rank.md),
  and `plot_inter()`.
- Fixed deprecated `aes_string()` usage across all plot functions.
- Fixed bare namespace references in
  [`plot_diag()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/plot_diag.md),
  [`plot_rank()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/plot_rank.md),
  `plot_inter()`.
- Replaced `size` with `linewidth` in `geom_smooth()` calls.
- Pruned from 13 to 8 core exported functions; archived extras in
  `R/old/`.
- Added GitHub Actions for R-CMD-check, pkgdown, test coverage, and
  linting.
- Switched license from MIT to GPL-3.

## UtopiaPlanitia 0.1.0

- Initial release with
  [`cf_loco()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/cf_loco.md),
  [`omni_hetero()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/omni_hetero.md),
  and plot functions.
