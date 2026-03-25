# Rank Plot of Individual CATE Estimates

Generates a rank plot of individual CATE (Conditional Average Treatment
Effect) estimates from a fitted causal forest, sorted by magnitude with
95\\

## Usage

``` r
plot_rank(c.forest, show_ate_region = TRUE)

rank_plot(c.forest, show_ate_region = TRUE)
```

## Arguments

- c.forest:

  A fitted causal forest object from the `grf` package.

- show_ate_region:

  Logical. If `TRUE` (default), draws a horizontal band showing the ATE
  +/- 1.96 standard errors. Units whose CIs fall entirely outside this
  band have strong evidence of individual-level heterogeneity.

## Value

A `ggplot2` object. Individual CATEs are plotted on the y-axis, ranked
by magnitude on the x-axis, with color indicating effect direction and
magnitude (red = negative, green = positive).

## Details

Each point represents one unit's out-of-bag CATE estimate, with a 95\\
confidence interval derived from the forest's variance estimates
(`estimate.variance = TRUE` in
[`grf::predict.causal_forest()`](https://rdrr.io/pkg/grf/man/predict.causal_forest.html)).
Units are sorted from most negative to most positive treatment effect.

**Interpretation.** A flat rank plot (all CIs overlapping the ATE band)
suggests homogeneous effects. A steep S-curve with CIs separating from
the ATE band suggests meaningful heterogeneity — some units benefit much
more (or less) than average. The color gradient reinforces this: red
points are units harmed or unaffected by treatment, green points are
units that benefit most.

## References

Athey, S. and Wager, S. (2019). Estimating Treatment Effects with Causal
Forests: An Application. *Observational Studies*, 5, 37–51.

Wager, S. and Athey, S. (2018). Estimation and Inference of
Heterogeneous Treatment Effects Using Random Forests. *Journal of the
American Statistical Association*, 113(523), 1228–1242.
[doi:10.1080/01621459.2017.1319839](https://doi.org/10.1080/01621459.2017.1319839)

## See also

[`plot_scatter()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/plot_scatter.md)
for CATEs against a single covariate,
[`plot_pdp()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/plot_pdp.md)
for partial dependence,
[`plot.causal_forest()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/plot.causal_forest.md)
to call this via `plot(cf, type = "rank")`.

## Examples

``` r
library(grf)
set.seed(1995)
n <- 200; p <- 5
X <- matrix(rnorm(n * p), n, p)
W <- rbinom(n, 1, 0.5)
Y <- X[, 1] * W + rnorm(n)
cf <- causal_forest(X, Y, W, num.trees = 100)
plot_rank(cf)
```
