# Print Summary of a Causal Forest

Formats and prints the output of
[`summary.causal_forest()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/summary.causal_forest.md),
showing the ATE, variable importance scores (sorted descending), and the
heterogeneity test table.

## Usage

``` r
# S3 method for class 'summary.causal_forest'
print(x, ...)
```

## Arguments

- x:

  An object of class `"summary.causal_forest"`.

- ...:

  Additional arguments (currently unused).

## Value

The input object `x` (invisibly).

## See also

[`summary.causal_forest()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/summary.causal_forest.md)
