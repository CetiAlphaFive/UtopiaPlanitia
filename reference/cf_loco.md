# Modified LOCO Variable Importance for Causal Forests

Computes leave-one-covariate-out (LOCO) variable importance for causal
forests using the debiased method of Benard and Josse (2023).

## Usage

``` r
cf_loco(
  c.forest,
  variable.groups = NULL,
  group.by.corr = FALSE,
  corr.threshold = 0.5,
  normalize = FALSE,
  screen = FALSE,
  stabilize = 1e-06,
  seed = 1995
)
```

## Arguments

- c.forest:

  A fitted causal forest object from the `grf` package.

- variable.groups:

  A list of variable groups. Each element of the list should contain the
  variable names of a group.

- group.by.corr:

  Logical indicating whether to group variables by correlation. Default
  is `FALSE`.

- corr.threshold:

  A numeric value between 0 and 1 indicating the correlation threshold
  for grouping variables. Default is `0.5`.

- normalize:

  Logical. If `TRUE`, return VI scores normalized to sum to 1. Default
  is `FALSE`.

- screen:

  Controls optional pre-screening of variables via split-frequency
  importance
  ([`grf::variable_importance()`](https://rdrr.io/pkg/grf/man/variable_importance.html)),
  which is essentially free to compute. Screening reduces the number of
  expensive LOCO refits.

  `FALSE` (default)

  :   No screening. If estimated runtime is high and the session is
      interactive, the user is prompted to screen.

  `TRUE`

  :   Auto-screen. Keep variables with split-frequency importance above
      the mean.

  Integer `k`

  :   Keep the top-k variables by split-frequency importance.

  Screened-out variables receive importance = 0 in the output.

- stabilize:

  Numeric. Floor for the per-observation denominator
  `Var_alpha(W.centered)` in the CATE re-estimation step. Prevents
  division-by-zero when forest weights concentrate on units with
  near-identical propensity scores. Default `1e-6`. Set to `0` to
  disable.

- seed:

  An integer seed for reproducibility. Default is `1995`.

## Value

An object of class `"cf_loco"` with components:

- vimp:

  Data frame with columns `Variable` and `Importance`.

- normalized:

  Logical indicating whether scores are normalized.

- n:

  Number of observations.

- p:

  Number of covariates.

## Details

**What LOCO measures.** Leave-one-covariate-out (LOCO) importance drops
each covariate (or group of covariates) from the model, refits the
forest without it, and measures how much worse the individualized
predictions become. A large score means the variable is important for
predicting heterogeneous treatment effects; a score near zero means the
forest can recover the same CATEs without that variable.

**Debiasing.** Naive LOCO is biased because refitting a forest
introduces retrain variance. This implementation uses the debiased
estimator of Benard and Josse (2023): a full-data refit is used as a
bias correction term, and per-observation CATE re-estimation is
performed via forest weights rather than raw OOB predictions.

**Computational cost.** The function refits `p + 1` causal forests (one
per dropped variable plus one full-data refit), each with the same
hyperparameters as the original. For large datasets or many covariates,
consider using the `screen` argument to reduce the number of refits.

**Normalization.** When `normalize = TRUE`, negative importance scores
(which can occur when dropping a variable improves predictions) are set
to zero, and the remaining scores are rescaled to sum to one. This gives
a proportional importance interpretation but discards directional
information.

**Variable grouping.** Highly correlated covariates can be grouped so
they are dropped together. Use `group.by.corr = TRUE` for automatic
grouping via a correlation threshold, or supply a manual list via
`variable.groups`.

## References

Benard, C. and Josse, J. (2023). Variable Importance for Causal Forests:
Breaking Down the Heterogeneity of Treatment Effects.
[doi:10.48550/arXiv.2308.03369](https://doi.org/10.48550/arXiv.2308.03369)

Athey, S., Tibshirani, J., and Wager, S. (2019). Generalized Random
Forests. *Annals of Statistics*, 47(2), 1148–1178.
[doi:10.1214/18-AOS1709](https://doi.org/10.1214/18-AOS1709)

## See also

[`compute_vimp()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/compute_vimp.md)
for the internal per-refit importance calculation,
[`loco()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/loco.md)
for LOCO importance with ranger models,
[`plot.cf_loco()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/plot.cf_loco.md)
and
[`summary.cf_loco()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/summary.cf_loco.md)
for visualizing and summarizing results,
[`omni_hetero()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/omni_hetero.md)
for heterogeneity testing.

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
vi <- cf_loco(cf)
summary(vi)
#> LOCO Variable Importance (Benard and Josse, 2023)
#>   n = 200  p = 5 
#>   Normalized: FALSE 
#> 
#>  Variable Importance
#>        X1   0.976803
#>        X2  -0.008422
#>        X3  -0.008641
#>        X5  -0.012525
#>        X4  -0.013253
plot(vi)


# Normalized importance (sums to 1)
vi_norm <- cf_loco(cf, normalize = TRUE)
summary(vi_norm)
#> LOCO Variable Importance (Benard and Josse, 2023)
#>   n = 200  p = 5 
#>   Normalized: TRUE 
#> 
#>  Variable Importance
#>        X1          1
#>        X2          0
#>        X3          0
#>        X4          0
#>        X5          0
# }
```
