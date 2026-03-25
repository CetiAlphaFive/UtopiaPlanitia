# Model Diagnostics for Causal Forest

Generates a multi-panel diagnostics figure for a causal forest model.

## Usage

``` r
plot_diag(c.forest)
```

## Arguments

- c.forest:

  A fitted causal forest model object from the `grf` package.

## Value

A list containing the following elements:

- out_check:

  A ggplot2 object showing the density plot comparing real Y and
  predicted Y.

- bal_perm_plot:

  A ggplot2 object showing the treatment propensity plot.

- cate_distro_plot:

  A ggplot2 object showing the density plot of the distribution of
  CATEs.

- rmse_out:

  The Root Mean Squared Error (RMSE) for the random forest model
  predictions.

## Examples

``` r
library(grf)
set.seed(1995)
n <- 200; p <- 5
X <- matrix(rnorm(n * p), n, p)
W <- rbinom(n, 1, 0.5)
Y <- X[, 1] * W + rnorm(n)
cf <- causal_forest(X, Y, W, num.trees = 100)
plot_diag(cf)
#> RMSE for outcome model: 1.136845 
#> No Simulated Assignment Vector Provided, Null Distribution Generated Using Permutated Treatment Assignment.

#> $outcome_check_plot

#> 
#> $treatment_propensity_plot

#> 
#> $cates_distribution_plot

#> 
#> $rmse_out
#> [1] 1.136845
#> 
```
