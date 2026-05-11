# Summarize a Causal Forest

Produces a one-stop summary of a fitted causal forest: the average
treatment effect (ATE), split-frequency variable importance, and a
battery of heterogeneity tests.

## Usage

``` r
# S3 method for class 'causal_forest'
summary(object, seed = 1995, ...)
```

## Arguments

- object:

  A fitted causal forest object from the `grf` package.

- seed:

  Integer seed for reproducibility. Default is `1995`. Passed to
  [`omni_hetero()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/omni_hetero.md)
  for the sequential RATE fold assignment.

- ...:

  Additional arguments (currently unused).

## Value

An object of class `"summary.causal_forest"` (returned invisibly) with
components:

- ate:

  Named numeric vector with `estimate` and `std.err` from
  [`grf::average_treatment_effect()`](https://rdrr.io/pkg/grf/man/average_treatment_effect.html).

- vimp:

  Named numeric vector of split-frequency variable importance scores
  from
  [`grf::variable_importance()`](https://rdrr.io/pkg/grf/man/variable_importance.html).
  These reflect how often each variable is used in tree splits — a fast
  heuristic, not the debiased LOCO scores from
  [`cf_loco()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/cf_loco.md).

- heterogeneity:

  Data frame from
  [`omni_hetero()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/omni_hetero.md)
  with five heterogeneity tests.

## Details

This method assembles three diagnostics:

1.  **ATE**: The doubly robust average treatment effect estimate from
    grf.

2.  **Variable importance**: grf's native split-frequency scores, which
    measure how often each covariate is selected for splitting. These
    are fast to compute but do not have a formal statistical
    interpretation. For debiased LOCO importance with proper
    methodology, use
    [`cf_loco()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/cf_loco.md).

3.  **Heterogeneity tests**: The full
    [`omni_hetero()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/omni_hetero.md)
    battery, including the calibration test, high/low CATE comparison,
    sequential RATE, and OOB RATE heuristics. See
    [`omni_hetero()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/omni_hetero.md)
    for details on each test.

## References

Athey, S., Tibshirani, J., and Wager, S. (2019). Generalized Random
Forests. *Annals of Statistics*, 47(2), 1148–1178.
[doi:10.1214/18-AOS1709](https://doi.org/10.1214/18-AOS1709)

## See also

[`print.summary.causal_forest()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/print.summary.causal_forest.md)
for formatted output,
[`omni_hetero()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/omni_hetero.md)
for the heterogeneity tests,
[`cf_loco()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/cf_loco.md)
for debiased variable importance,
[`plot.causal_forest()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/plot.causal_forest.md)
for visual diagnostics.

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
#> Warning: Sequential RATE may be unstable at this sample size (n = 200, n/num.folds = 40; min_fold_n = 100). Training folds may be too small for the per-fold CATE forest to detect heterogeneity, which can produce degenerate RATE statistics. Consider the Calibration test (Chernozhukov et al., 2018) or the OOB RATE heuristics instead.
#> Warning: Sequential RATE: dropped 1 of 4 folds due to degenerate fits (near-constant CATE predictions on test fold).
#> Sequential RATE: Sequential RATE: 3 of 4 folds usable after degeneracy filtering. Aggregation denominator sqrt(num.folds - 1) would deflate the t-statistic relative to the number of contributing folds. Returning NA p-value. Increase sample size or use the Calibration test for formal inference at this n.
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
#> Omnibus Heterogeneity Tests
#> ---------------------------------------- 
#> 
#> Preferred (valid size)
#> 
#>  heterogeneity_test                           estimate p_value hetero_detected
#>  Sequential RATE (Wager, 2024)                —        —       —              
#>  Calibration Test (Chernozhukov et al., 2018) 1.3212   0.0000  Yes            
#> 
#> Heuristic (screening only)
#> 
#>  heterogeneity_test                                   estimate p_value
#>  High vs. Low CATE, cross-fit (Athey and Wager, 2019) 2.0233   0.0000 
#>  OOB RATE, two-sided (heuristic, anti-conservative)   0.5296   0.0010 
#>  OOB RATE, one-sided (heuristic)                      0.5296   0.0005 
#>  hetero_detected
#>  Yes            
#>  Yes            
#>  Yes            
# }
```
