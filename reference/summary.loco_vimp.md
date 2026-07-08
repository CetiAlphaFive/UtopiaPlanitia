# Summarize LOCO Variable Importance

Prints a formatted summary of
[`loco()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/loco.md)
results. Equivalent to calling
[`print()`](https://rdrr.io/r/base/print.html) on the object.

## Usage

``` r
# S3 method for class 'loco_vimp'
summary(object, ...)
```

## Arguments

- object:

  An object of class `"loco_vimp"` returned by
  [`loco()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/loco.md).

- ...:

  Additional arguments passed to
  [`print.loco_vimp()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/print.loco_vimp.md).

## Value

The `loco_vimp` object (invisibly).

## See also

[`loco()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/loco.md),
[`print.loco_vimp()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/print.loco_vimp.md),
[`plot.loco_vimp()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/plot.loco_vimp.md)
