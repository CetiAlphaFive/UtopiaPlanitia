# Print Omnibus Heterogeneity Tests

Formats the
[`omni_hetero()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/omni_hetero.md)
output as a clean table suitable for an appendix, grouped by Preferred
and Heuristic categories.

## Usage

``` r
# S3 method for class 'omni_hetero'
print(x, latex = FALSE, ...)
```

## Arguments

- x:

  An object of class `"omni_hetero"`.

- latex:

  Logical. If `TRUE`, prints a self-contained LaTeX `tabular`
  environment ready to copy-paste into a manuscript. Default is `FALSE`.

- ...:

  Additional arguments (currently unused).

## Value

The input object `x` (invisibly).

## See also

[`omni_hetero()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/omni_hetero.md)
