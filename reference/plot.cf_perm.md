# Plot PermuCATE Variable Importance

Draws a horizontal lollipop chart of PermuCATE importance scores. When
confidence bounds are available (not normalized), a one-sided lower
confidence bound is drawn as a whisker (the upper bound is unbounded by
construction). Points are colored by significance at the object's
`conf.level` (a covariate is significant when its lower confidence bound
exceeds zero).

## Usage

``` r
# S3 method for class 'cf_perm'
plot(x, ...)
```

## Arguments

- x:

  An object of class `"cf_perm"` returned by
  [`cf_perm()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/cf_perm.md).

- ...:

  Additional arguments (currently unused).

## Value

A `ggplot` object.
