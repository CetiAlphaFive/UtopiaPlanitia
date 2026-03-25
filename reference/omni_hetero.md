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

  An integer seed for reproducibility. Default is `1995`.

## Value

A data frame summarizing the results of the heterogeneity tests,
including the test names, estimates, p-values, and whether heterogeneity
is detected at the 0.05 level.

## Details

The **OOB RATE two-sided test** is known to be anti-conservative
(roughly 30\\ vignette). The one-sided version is approximately valid
when the direction is pre-specified. The sequential RATE test has
correct size.

## References

Chernozhukov, V., Demirer, M., Duflo, E., and Fernandez-Val, I. (2018).
Generic Machine Learning Inference on Heterogeneous Treatment Effects in
Randomized Experiments. NBER Working Paper 24678.

Athey, S. and Wager, S. (2019). Estimating Treatment Effects with Causal
Forests: An Application. *Observational Studies*, 5, 37–51.

Wager, S. (2024). Sequential Validation of Treatment Heterogeneity.
arXiv:2405.05534.

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
