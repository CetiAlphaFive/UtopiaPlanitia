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
