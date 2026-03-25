# LOCO Variable Importance for Outcome Models

Computes leave-one-covariate-out (LOCO) variable importance for ranger
random forests. Two modes are available: split-sample LOCO with
conformal inference (confidence intervals and p-values), or OOB-based
LOCO (point estimates only).

## Usage

``` r
loco(model, alpha = 0.1, split = TRUE, seed = 1995, verbose = FALSE)
```

## Arguments

- model:

  A fitted `ranger` object (must have been fit with `keep.inbag = TRUE`
  if using OOB mode, and the training data must be recoverable from the
  call).

- alpha:

  Significance level for conformal inference intervals. Default is `0.1`
  (90% intervals). Only used when `split = TRUE`.

- split:

  Logical. If `TRUE` (default), uses split-sample LOCO via
  [`conformalInference::loco()`](https://rdrr.io/pkg/conformalInference/man/loco.html).
  If `FALSE`, uses OOB prediction error differences.

- seed:

  Integer seed for reproducibility. Default is `1995`.

- verbose:

  Logical. Print progress from conformal inference? Default is `FALSE`.
  Only used when `split = TRUE`.

## Value

A data frame sorted by descending importance with columns:

- variable:

  Covariate name.

- importance:

  LOCO importance score (midpoint of CI when `split = TRUE`, increase in
  OOB error when `split = FALSE`).

- ci.lower:

  Lower confidence bound (only when `split = TRUE`).

- ci.upper:

  Upper confidence bound (only when `split = TRUE`).

- p.value:

  P-value from conformal Z-test (only when `split = TRUE`).

## Details

The split-sample method requires the `conformalInference` package, which
is available only from GitHub:

    devtools::install_github("ryantibs/conformal", subdir = "conformalInference")

The OOB method refits the original ranger model once per covariate, each
time dropping one predictor, and compares OOB prediction error to the
full-model baseline.

## References

Lei, J., G'Sell, M., Rinaldo, A., Tibshirani, R. J., & Wasserman, L.
(2018). Distribution-Free Predictive Inference for Regression. *Journal
of the American Statistical Association*, 113(523), 1094–1111.

## Examples

``` r
# \donttest{
if (requireNamespace("ranger", quietly = TRUE)) {
  set.seed(1995)
  dat <- data.frame(y = rnorm(100), x1 = rnorm(100), x2 = rnorm(100))
  mod <- ranger::ranger(y ~ ., data = dat, num.trees = 50)
  loco(mod, split = FALSE)
}
#>   variable importance
#> 1       x2  0.3322422
#> 2       x1  0.2607941
# }
```
