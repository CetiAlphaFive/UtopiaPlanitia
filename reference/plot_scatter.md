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

A ggplot2 object with the scatter plot.

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
