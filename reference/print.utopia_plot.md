# Print a UtopiaPlanitia plot

Ensures `grid.newpage()` is always called before drawing, which fixes
silent‐plot issues in VS Code and other environments where
[`grDevices::dev.interactive()`](https://rdrr.io/r/grDevices/dev.interactive.html)
returns `FALSE`.

## Usage

``` r
# S3 method for class 'utopia_plot'
print(x, ...)
```

## Arguments

- x:

  A `utopia_plot` object.

- ...:

  Ignored.

## Value

Invisibly returns `x`.

## See also

[`plot_pdp()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/plot_pdp.md)
and
[`plot_scatter()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/plot_scatter.md),
which return `utopia_plot` objects.
