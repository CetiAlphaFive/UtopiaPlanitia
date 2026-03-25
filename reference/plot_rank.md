# Rank Plot of Individual CATE Estimates

Generates a rank plot of individual CATE (Conditional Average Treatment
Effect) estimates from a fitted causal forest model.

## Usage

``` r
plot_rank(c.forest, show_ate_region = TRUE)

rank_plot(c.forest, show_ate_region = TRUE)
```

## Arguments

- c.forest:

  A fitted causal forest model object from the `grf` package.

- show_ate_region:

  Logical indicating whether to show the ATE region (confidence
  interval). Default is TRUE.

## Value

A ggplot2 object with the rank plot of individual CATE estimates.

## Examples

``` r
library(grf)
set.seed(1995)
n <- 200; p <- 5
X <- matrix(rnorm(n * p), n, p)
W <- rbinom(n, 1, 0.5)
Y <- X[, 1] * W + rnorm(n)
cf <- causal_forest(X, Y, W, num.trees = 100)
plot_rank(cf)
```
