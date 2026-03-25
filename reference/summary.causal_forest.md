# Summarize a Causal Forest

Produces a summary of heterogeneity tests, variable importance scores,
and the average treatment effect for a fitted causal forest.

## Usage

``` r
# S3 method for class 'causal_forest'
summary(object, seed = 1995, ...)
```

## Arguments

- object:

  A fitted causal forest object from the `grf` package.

- seed:

  Integer seed for reproducibility. Default is `1995`.

- ...:

  Additional arguments (currently unused).

## Value

An object of class `"summary.causal_forest"` (returned invisibly) with
components:

- heterogeneity:

  Data frame from
  [`omni_hetero()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/omni_hetero.md).

- vimp:

  Named numeric vector of variable importance scores.

- ate:

  Named numeric vector with `estimate` and `std.err`.

## Examples

``` r
# \donttest{
library(grf)
set.seed(1995)
n <- 200; p <- 5
X <- matrix(rnorm(n * p), n, p)
colnames(X) <- paste0("X", seq_len(p))
W <- rbinom(n, 1, 0.5)
Y <- X[, 1] * W + rnorm(n)
cf <- causal_forest(X, Y, W, num.trees = 100)
summary(cf)
#> Average Treatment Effect
#>   Estimate: -0.0927  SE: 0.1631 
#> 
#> Variable Importance (grf native)
#>  variable importance
#>        X1     0.6322
#>        X4     0.0975
#>        X5     0.0861
#>        X3     0.0715
#>        X2     0.0687
#> 
#> Heterogeneity Tests
#>                                  heterogeneity_test  estimate      p_value
#>        Calibration Test (Chernozhukov et al., 2018) 1.3212439 2.949514e-09
#>           High vs. Low CATE (Athey and Wager, 2019) 1.6997625 2.250167e-08
#>                       Sequential RATE (Wager, 2024)        NA          NaN
#>  OOB RATE, two-sided (heuristic, anti-conservative) 0.5295879 9.115004e-04
#>                     OOB RATE, one-sided (heuristic) 0.5295879 4.557502e-04
#>  hetero_detected
#>             TRUE
#>             TRUE
#>               NA
#>             TRUE
#>             TRUE
# }
```
