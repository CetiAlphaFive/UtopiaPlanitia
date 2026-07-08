# Print LOCO Variable Importance

Displays a table of
[`loco()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/loco.md)
variable importance scores sorted from most to least important. Shows
sample size, number of covariates, test method, loss, and whether the
result came from split-sample or OOB mode.

## Usage

``` r
# S3 method for class 'loco_vimp'
print(x, ...)
```

## Arguments

- x:

  An object of class `"loco_vimp"` returned by
  [`loco()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/loco.md).

- ...:

  Additional arguments (currently unused).

## Value

The `loco_vimp` object (invisibly).

## See also

[`loco()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/loco.md),
[`summary.loco_vimp()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/summary.loco_vimp.md),
[`plot.loco_vimp()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/plot.loco_vimp.md)
