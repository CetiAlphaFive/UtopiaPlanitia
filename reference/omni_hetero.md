# Omnibus Tests of Heterogeneity

Performs various heterogeneity tests on a fitted causal forest model.
Combines the calibration test of Chernozhukov et al. (2018), a cross-fit
high/low CATE split (Athey and Wager, 2019; cf. grf PR \#1502), the
sequential RATE test (Wager, 2024), and OOB RATE heuristics into a
single summary table.

## Usage

``` r
omni_hetero(c.forest, seed = 1995, min_fold_n = 100)
```

## Arguments

- c.forest:

  A fitted causal forest object from the `grf` package.

- seed:

  An integer seed for reproducibility. Default is `1995`. Controls the
  fold assignment in the sequential RATE test and the half-sample split
  in the cross-fit High vs. Low test.

- min_fold_n:

  Minimum per-fold training sample size below which the Sequential RATE
  test is considered unstable. When `n < 400` or
  `n / num.folds < min_fold_n`, a warning is emitted at the start of the
  Sequential RATE computation and users are advised to prefer the
  Calibration test (Chernozhukov et al., 2018) or OOB RATE heuristics
  for small samples. Within the fold loop, any fold whose test-set CATE
  predictions are (near-)constant, whose RATE standard error is zero, or
  whose t-statistic is `NaN` is dropped from aggregation rather than
  allowed to propagate a silent `NaN` into the final p-value. If ANY
  fold is dropped (i.e. fewer than `num.folds - 1` t-statistics
  accumulate), the Sequential RATE p-value is returned as `NA_real_`
  with an explanatory `reason` attribute, because the
  `sqrt(num.folds - 1)` aggregation denominator would otherwise
  under-normalize and deflate the t-statistic. Default is `100`.

## Value

An object of class `"omni_hetero"` (a data frame) with one row per test
and the following columns:

- category:

  Character. `"Preferred"` for tests with valid Type I error control,
  `"Heuristic"` for screening tests.

- heterogeneity_test:

  Character. Name and citation of the test.

- estimate:

  Numeric. The test statistic or effect estimate. `NA` for the
  sequential RATE test (which only produces a p-value).

- p_value:

  Numeric. Two-sided p-value (or one-sided for the final row). Small
  values indicate evidence of treatment effect heterogeneity.

- hetero_detected:

  Logical. `TRUE` if `p_value <= 0.05`, `NA` when `p_value` is `NA`.

## Details

The function combines five tests of treatment effect heterogeneity,
grouped into **Preferred** (valid size) and **Heuristic** categories:

**Preferred tests** (valid Type I error control):

1.  **Sequential RATE** (Wager, 2024): A k-fold cross-validated test
    with correct size (valid Type I error). This is the most trustworthy
    test in the battery and should be preferred for formal inference.

2.  **Calibration test** (Chernozhukov et al., 2018): Regresses doubly
    robust scores on the forest's CATE predictions. The "differential
    forest prediction" coefficient tests whether the forest captures
    meaningful heterogeneity beyond the ATE.

**Heuristic tests** (useful for screening, not formal inference):

1.  **High vs. low CATE, cross-fit** (Athey and Wager, 2019): Splits the
    sample in half, predicts CATEs on each half using a forest trained
    on the other half, then median-splits and compares \\ATE\_{high}\\
    vs. \\ATE\_{low}\\ within each held-out half. Cross-fitting avoids
    the winner's curse that contaminates the naive same-data version
    (see grf PR \#1502).

2.  **OOB RATE, two-sided** (heuristic): Uses out-of-bag CATE
    predictions directly. Known to be anti-conservative (~30\\ null at
    nominal 5\\ screening tool, not for formal inference.

3.  **OOB RATE, one-sided** (heuristic): One-sided version of the above.
    Approximately valid when the direction of heterogeneity is
    pre-specified.

## References

Chernozhukov, V., Demirer, M., Duflo, E., and Fernandez-Val, I. (2018).
Generic Machine Learning Inference on Heterogeneous Treatment Effects in
Randomized Experiments. NBER Working Paper 24678.

Athey, S. and Wager, S. (2019). Estimating Treatment Effects with Causal
Forests: An Application. *Observational Studies*, 5, 37–51.

Wager, S. (2024). Sequential Validation of Treatment Heterogeneity.
[doi:10.48550/arXiv.2405.05534](https://doi.org/10.48550/arXiv.2405.05534)

## See also

[`summary.causal_forest()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/summary.causal_forest.md)
which calls this function as part of its output,
[`cf_loco()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/cf_loco.md)
for variable-level importance rather than an omnibus heterogeneity test.

## Examples

``` r
# \donttest{
library(grf)
set.seed(1995)
n <- 200; p <- 5
X <- matrix(rnorm(n * p), n, p)
W <- rbinom(n, 1, 0.5)
Y <- X[, 1] * W + rnorm(n)
cf <- causal_forest(X, Y, W, num.trees = 100)
omni_hetero(cf)
#> Warning: Sequential RATE may be unstable at this sample size (n = 200, n/num.folds = 40; min_fold_n = 100). Training folds may be too small for the per-fold CATE forest to detect heterogeneity, which can produce degenerate RATE statistics. Consider the Calibration test (Chernozhukov et al., 2018) or the OOB RATE heuristics instead.
#> Warning: Sequential RATE: dropped 1 of 4 folds due to degenerate fits (near-constant CATE predictions on test fold).
#> Sequential RATE: Sequential RATE: 3 of 4 folds usable after degeneracy filtering. Aggregation denominator sqrt(num.folds - 1) would deflate the t-statistic relative to the number of contributing folds. Returning NA p-value. Increase sample size or use the Calibration test for formal inference at this n.
#> Omnibus Heterogeneity Tests
#> ---------------------------------------- 
#> 
#> Preferred (valid size)
#> 
#>  heterogeneity_test                           estimate p_value hetero_detected
#>  Sequential RATE (Wager, 2024)                --       --      --             
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
