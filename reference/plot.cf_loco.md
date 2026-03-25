# Plot LOCO Variable Importance

Draws a horizontal lollipop chart of LOCO variable importance scores,
sorted from least to most important (bottom to top).

## Usage

``` r
# S3 method for class 'cf_loco'
plot(x, ...)
```

## Arguments

- x:

  An object of class `"cf_loco"` returned by
  [`cf_loco()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/cf_loco.md).

- ...:

  Additional arguments (currently unused).

## Value

A `ggplot` object.

## Details

Variables are displayed as horizontal segments ending in points, with
length proportional to importance. When `normalize = TRUE` was used in
[`cf_loco()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/cf_loco.md),
the y-axis label changes to "Importance (normalized)".

## See also

[`cf_loco()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/cf_loco.md)
to compute the scores,
[`summary.cf_loco()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/summary.cf_loco.md)
for tabular output.

## Examples

``` r
library(grf)
set.seed(1995)
n <- 200; p <- 5
X <- matrix(rnorm(n * p), n, p)
colnames(X) <- paste0("X", seq_len(p))
W <- rbinom(n, 1, 0.5)
Y <- X[, 1] * W + rnorm(n)
cf <- causal_forest(X, Y, W, num.trees = 100)
vi <- cf_loco(cf)
plot(vi)
```
