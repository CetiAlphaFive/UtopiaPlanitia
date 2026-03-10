# UtopiaPlanitia 0.3.0

* New `plot_pdp()`: real Friedman (2001) partial dependence plots for 1-way and 2-way CATE surfaces.
* New `plot_scatter()`: renamed from the old `plot_pdp()`, which plotted individual OOB CATEs vs. a covariate.
* `plot_inter()` is deprecated in favor of `plot_pdp(c.forest, x_var, y_var)`.
* `plot.causal_forest()` gains `type = "scatter"` for the new `plot_scatter()`.

# UtopiaPlanitia 0.2.0

* Added S3 `summary()` and `plot()` methods for `causal_forest` objects.
* Added S3 `summary()`, `print()`, and `plot()` methods for `cf_loco` output.
* Added `plot.causal_forest()` dispatcher for `plot_diag()`, `plot_pdp()`, `rank_plot()`, and `plot_inter()`.
* Fixed deprecated `aes_string()` usage across all plot functions.
* Fixed bare namespace references in `plot_diag()`, `plot_rank()`, `plot_inter()`.
* Replaced `size` with `linewidth` in `geom_smooth()` calls.
* Pruned from 13 to 8 core exported functions; archived extras in `R/old/`.
* Added GitHub Actions for R-CMD-check, pkgdown, test coverage, and linting.
* Switched license from MIT to GPL-3.

# UtopiaPlanitia 0.1.0

* Initial release with `cf_loco()`, `omni_hetero()`, and plot functions.
