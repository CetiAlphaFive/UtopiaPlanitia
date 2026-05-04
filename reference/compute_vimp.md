# Compute Variable Importance (Internal)

Computes a single variable importance score from forest weights and
centered data. This is an internal helper called by
[`cf_loco()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/cf_loco.md)
for each dropped-variable refit; most users should call
[`cf_loco()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/cf_loco.md)
directly.

## Usage

``` r
compute_vimp(alpha, Y.centered, W.centered, tau.hat, stabilize = 0.000001)
```

## Arguments

- alpha:

  Sparse weight matrix from
  [`grf::get_forest_weights()`](https://rdrr.io/pkg/grf/man/get_forest_weights.html).
  Each row `i` gives the weights that forest observations receive when
  predicting unit `i`'s treatment effect.

- Y.centered:

  Numeric vector. Outcome residuals `Y - Y.hat`, where `Y.hat` is the
  forest's outcome model (marginal mean of Y given X).

- W.centered:

  Numeric vector. Treatment residuals `W - W.hat`, where `W.hat` is the
  estimated propensity score.

- tau.hat:

  Numeric vector. Original out-of-bag CATE predictions from the full
  (non-dropped) causal forest.

- stabilize:

  Numeric floor for the per-observation denominator
  `Var_alpha(W.centered)`. Prevents division-by-zero when forest weights
  concentrate on units with near-identical propensity scores. Default
  `1e-6`. Set to `0` to disable.

## Value

A single numeric value: the importance score for this refit. Higher
values indicate greater importance.

## Details

For each observation, the function re-estimates the CATE using the
weight-based regression:

\$\$\hat{\tau}\_i^{new} = \frac{Cov\_\alpha(W_i, Y_i) - \hat{\tau}\_i
\cdot Var\_\alpha(W_i)}{Var\_\alpha(W_i)}\$\$

where \\Cov\_\alpha\\ and \\Var\_\alpha\\ are weighted covariances and
variances using the forest weights \\\alpha\\. The importance score is
then the normalized squared difference between the original and
re-estimated CATEs, divided by the variance of the original predictions.

When `Var_alpha(W.centered)` is near zero for some observations (e.g.,
because the forest weights concentrate on units with similar propensity
scores), the `stabilize` parameter clamps the denominator to prevent
numerical instability.

## References

Benard, C. and Josse, J. (2023). Variable Importance for Causal Forests:
Breaking Down the Heterogeneity of Treatment Effects.
[doi:10.48550/arXiv.2308.03369](https://doi.org/10.48550/arXiv.2308.03369)

## See also

[`cf_loco()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/cf_loco.md)
which calls this function internally.
