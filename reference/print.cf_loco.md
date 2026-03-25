# Print LOCO Variable Importance

Displays a table of LOCO variable importance scores sorted from most to
least important. Shows sample size, number of covariates, and whether
scores are normalized.

## Usage

``` r
# S3 method for class 'cf_loco'
print(x, ...)
```

## Arguments

- x:

  An object of class `"cf_loco"` returned by
  [`cf_loco()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/cf_loco.md).

- ...:

  Additional arguments (currently unused).

## Value

The `cf_loco` object (invisibly).

## See also

[`cf_loco()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/cf_loco.md),
[`summary.cf_loco()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/summary.cf_loco.md),
[`plot.cf_loco()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/plot.cf_loco.md)
