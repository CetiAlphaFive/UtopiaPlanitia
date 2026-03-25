# Compute Variable Importance

Computes variable importance from coefficients alpha of the retrained
forest, centered outcome and treatment assignment, and original OOB
predictions. This is an internal helper used by
[`cf_loco()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/cf_loco.md).

## Usage

``` r
compute_vimp(alpha, Y.centered, W.centered, tau.hat, stabilize = 1e-06)
```

## Arguments

- alpha:

  Coefficients alpha of the retrained forest.

- Y.centered:

  Centered outcome.

- W.centered:

  Centered treatment assignment.

- tau.hat:

  Original out-of-bag predictions.

- stabilize:

  Numeric floor for the per-observation denominator
  `Var_alpha(W.centered)`. Prevents division-by-zero when forest weights
  concentrate on units with near-identical propensity scores. Default
  `1e-6`.

## Value

A numeric value representing variable importance.
