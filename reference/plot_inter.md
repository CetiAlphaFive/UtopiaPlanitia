# Interaction Plot for Causal Forest (Deprecated)

**\[deprecated\]**

## Usage

``` r
plot_inter(
  c.forest,
  x_var,
  y_var,
  bin_count = 50,
  limits = c(min(c.forest$predictions), max(c.forest$predictions))
)
```

## Arguments

- c.forest:

  A fitted causal forest model object from the `grf` package.

- x_var:

  The name of the variable to be plotted on the x-axis.

- y_var:

  The name of the variable to be plotted on the y-axis.

- bin_count:

  Number of bins for the hexagonal binning. Default is 50.

- limits:

  The limits of the color scale, by default the minimum and maximum of
  the CATEs.

## Value

A ggplot2 object with the interaction plot.

## Details

`plot_inter()` plots raw OOB CATEs as hex-binned means. For a proper
two-way partial dependence plot, use
[`plot_pdp()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/plot_pdp.md)
with both `x_var` and `y_var`.

## See also

[`plot_pdp()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/plot_pdp.md)
for the recommended replacement.

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
plot_inter(cf, x_var = "X1", y_var = "X2")
#> Warning: plot_inter() is deprecated. Use plot_pdp(c.forest, x_var, y_var) for a 2-way partial dependence plot.
```
