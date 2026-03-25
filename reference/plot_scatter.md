# CATE Scatter Plot for Causal Forest

Plots individual out-of-bag CATE estimates against a covariate with an
optional loess smoother and ATE confidence region. This is **not** a
Friedman (2001) partial dependence plot — see
[`plot_pdp()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/plot_pdp.md)
for that.

## Usage

``` r
plot_scatter(
  c.forest,
  x_var,
  curve_fitter = TRUE,
  method = "loess",
  show_ate_region = TRUE,
  x.limits = NULL,
  y.limits = NULL
)
```

## Arguments

- c.forest:

  A fitted causal forest model object from the `grf` package.

- x_var:

  The name of the variable to be plotted on the x-axis.

- curve_fitter:

  Logical indicating whether to include a curve fitter. Default is TRUE.

- method:

  The smoothing method to be used by `geom_smooth` if `curve_fitter` is
  TRUE. Default is "loess".

- show_ate_region:

  Logical indicating whether to show the ATE region (confidence
  interval). Default is TRUE.

- x.limits:

  x axis limits specified as c() vector. Defaults to range of X.

- y.limits:

  y axis limits specified as c() vector. Defaults to range of Y.

## Value

A `utopia_plot` object (a `ggExtraPlot` with marginal histograms).

## Details

Each point is one unit's out-of-bag CATE prediction plotted against its
observed covariate value. Unlike
[`plot_pdp()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/plot_pdp.md),
which marginalizes over the remaining covariates by averaging
predictions at each grid point, this plot shows raw individual
predictions — making it faster but more sensitive to confounding between
covariates.

The optional LOESS smoother (or other method via `method`) helps reveal
the conditional relationship between the covariate and the treatment
effect. When the smoother deviates from the ATE band, this suggests the
covariate modifies the treatment effect.

## References

Friedman, J. H. (2001). Greedy Function Approximation: A Gradient
Boosting Machine. *Annals of Statistics*, 29(5), 1189–1232.

## See also

[`plot_pdp()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/plot_pdp.md)
for the marginalized partial dependence version,
[`plot_rank()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/plot_rank.md)
for ranked CATEs with CIs,
[`plot.causal_forest()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/plot.causal_forest.md)
to call this via `plot(cf, type = "scatter", x_var = ...)`.

## Examples

``` r
library(grf)
set.seed(1995)
n <- 200; p <- 5
X <- matrix(rnorm(n * p), n, p)
colnames(X) <- paste0("X", seq_len(p))
W <- rbinom(n, 1, 0.5)
Y <- X[, 1] * W + rnorm(n)
cf <- causal_forest(X, Y, W, num.trees = 100)
plot_scatter(cf, x_var = "X1")
#> `geom_smooth()` using formula = 'y ~ x'
#> `geom_smooth()` using formula = 'y ~ x'
```
