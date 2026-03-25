# Model Diagnostics for Causal Forest

Generates a multi-panel diagnostics figure for a causal forest model,
covering outcome model fit, treatment balance, and CATE distribution.

## Usage

``` r
plot_diag(c.forest)
```

## Arguments

- c.forest:

  A fitted causal forest object from the `grf` package.

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

  The Root Mean Squared Error (RMSE) for the outcome model predictions.
  Also printed to the console.

## Details

The figure contains three panels:

1.  **Outcome check** (top-left): Overlays the density of the observed
    outcome `Y` against the forest's outcome model predictions `Y.hat`.
    Good overlap indicates a well-specified outcome model. The RMSE is
    printed to the console.

2.  **Balance permutation test** (top-right): Uses
    [`MLbalance::random_check()`](https://rdrr.io/pkg/MLbalance/man/random_check.html)
    to test whether the treatment assignment `W` is predictable from the
    covariates `X`. A non-significant result (large p-value) is
    consistent with random or as-if-random assignment. See
    Gagnon-Bartsch and Shem-Tov (2019).

3.  **CATE distribution** (bottom): Histogram with interval summary of
    the out-of-bag CATE estimates. A tight distribution centered at zero
    suggests little heterogeneity; a spread distribution suggests
    meaningful variation in treatment effects across units.

## References

Gagnon-Bartsch, J. and Shem-Tov, Y. (2019). The Classification
Permutation Test: A Flexible Approach to Testing for Covariate Imbalance
in Observational and Experimental Studies.
[doi:10.48550/arXiv.1903.07841](https://doi.org/10.48550/arXiv.1903.07841)

## See also

[`summary.causal_forest()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/summary.causal_forest.md)
for a text-based summary,
[`plot.causal_forest()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/plot.causal_forest.md)
to call this via `plot(cf, type = "diag")`.

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
