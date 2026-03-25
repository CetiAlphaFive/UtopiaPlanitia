# Omnibus Tests of Heterogeneity

Performs various heterogeneity tests on a fitted causal forest model.
Combines the calibration test of Chernozhukov et al. (2018), a naive
high/low CATE split (Athey and Wager, 2019), the sequential RATE test
(Wager, 2024), and OOB RATE heuristics into a single summary table.

## Usage

``` r
omni_hetero(c.forest, seed = 1995)
```

## Arguments

- c.forest:

  A fitted causal forest object from the `grf` package.

- seed:

  An integer seed for reproducibility. Default is `1995`. Controls the
  fold assignment in the sequential RATE test.

## Value

A data frame with one row per test and the following columns:

- heterogeneity_test:

  Character. Name and citation of the test.

- estimate:

  Numeric. The test statistic or effect estimate. `NA` for the
  sequential RATE test (which only produces a p-value).

- p_value:

  Numeric. Two-sided p-value (or one-sided for the final row). Small
  values indicate evidence of treatment effect heterogeneity.

- hetero_detected:

  Logical. `TRUE` if `p_value <= 0.05`.

## Details

The function combines five tests of treatment effect heterogeneity,
ranging from well-calibrated to heuristic:

1.  **Calibration test** (Chernozhukov et al., 2018): Regresses doubly
    robust scores on the forest's CATE predictions. The "differential
    forest prediction" coefficient tests whether the forest captures
    meaningful heterogeneity beyond the ATE.

2.  **High vs. low CATE** (Athey and Wager, 2019): Splits units at the
    median predicted CATE and compares the ATE in each half. A
    significant difference suggests the forest detects real variation.

3.  **Sequential RATE** (Wager, 2024): A k-fold cross-validated test
    with correct size (valid Type I error). This is the most trustworthy
    test in the battery and should be preferred for formal inference.

4.  **OOB RATE, two-sided** (heuristic): Uses out-of-bag CATE
    predictions directly. Known to be anti-conservative (~30\\ null at
    nominal 5\\ screening tool, not for formal inference.

5.  **OOB RATE, one-sided** (heuristic): One-sided version of the above.
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
#>                                   heterogeneity_test  estimate      p_value
#> 1       Calibration Test (Chernozhukov et al., 2018) 1.3212439 2.949514e-09
#> 2          High vs. Low CATE (Athey and Wager, 2019) 1.6997625 2.250167e-08
#> 3                      Sequential RATE (Wager, 2024)        NA          NaN
#> 4 OOB RATE, two-sided (heuristic, anti-conservative) 0.5295879 9.115004e-04
#> 5                    OOB RATE, one-sided (heuristic) 0.5295879 4.557502e-04
#>   hetero_detected
#> 1            TRUE
#> 2            TRUE
#> 3              NA
#> 4            TRUE
#> 5            TRUE
# }
```
