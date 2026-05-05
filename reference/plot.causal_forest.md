# Plot a Causal Forest

Dispatches to one of the package's plot functions based on `type`.

## Usage

``` r
# S3 method for class 'causal_forest'
plot(x, type = c("diag", "pdp", "scatter", "rank"), ...)
```

## Arguments

- x:

  A fitted causal forest object from the `grf` package.

- type:

  Character string specifying the plot type. One of:

  - `"diag"`: multi-panel diagnostics
    ([`plot_diag()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/plot_diag.md))

  - `"pdp"`: partial dependence plot
    ([`plot_pdp()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/plot_pdp.md))

  - `"scatter"`: individual CATE scatter
    ([`plot_scatter()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/plot_scatter.md))

  - `"rank"`: ranked CATEs with CIs
    ([`plot_rank()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/plot_rank.md))

- ...:

  Additional arguments passed to the underlying plot function (e.g.,
  `x_var` for `"pdp"`, `"scatter"`, and `"inter"`).

## Value

A plot object (the return value of the dispatched function).

## Details

This is a convenience dispatcher. Each type targets a different
diagnostic question:

- `"diag"`:

  Are the nuisance models (outcome, propensity) well calibrated? Is
  there treatment effect heterogeneity?

- `"pdp"`:

  How does the CATE change as a function of one or two covariates,
  marginalizing over the rest?

- `"scatter"`:

  What do the individual OOB CATE predictions look like against a single
  covariate?

- `"rank"`:

  What is the distribution of individual CATEs, sorted by magnitude,
  with confidence intervals?

## See also

[`plot_diag()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/plot_diag.md),
[`plot_pdp()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/plot_pdp.md),
[`plot_scatter()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/plot_scatter.md),
[`plot_rank()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/plot_rank.md)

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
plot(cf, type = "rank")
```
