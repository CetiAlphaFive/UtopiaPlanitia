# Plot PermuCATE Variable Importance

Draws a horizontal variable-importance bar chart of PermuCATE scores,
sorted from most to least important (largest at the top).

## Usage

``` r
# S3 method for class 'cf_perm'
plot(x, fill.sig = "darkorange", ...)
```

## Arguments

- x:

  An object of class `"cf_perm"` returned by
  [`cf_perm()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/cf_perm.md).

- fill.sig:

  Bar fill for significant covariates (default `"darkorange"`).
  Non-significant covariates use `"gray70"`.

- ...:

  Additional arguments (currently unused).

## Value

A `ggplot` object.

## Details

Shares the package house styling with
[`plot_pdp()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/plot_pdp.md)
(serif type, gray panel, centered title, legend at the bottom). Colored
horizontal bars (`alpha = 0.75`) end in a black tip point over a gray
zero reference line, with the x-axis hugged to the data. Bars are filled
by significance at the object's `conf.level` (a covariate is significant
when its lower confidence bound exceeds zero): significant bars use
`fill.sig`, others `"gray70"`. When confidence bounds are available (not
normalized), a one-sided lower confidence bound is drawn as a horizontal
whisker from `CI.lower` to the importance tip (the upper bound is `+Inf`
by construction, so there is no upper whisker). For normalized objects
the bounds are `NA`, so no whisker is drawn, all bars take the
non-significant fill, and the title gains "(normalized)".

## See also

[`cf_perm()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/cf_perm.md)
to compute the scores,
[`summary.cf_perm()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/summary.cf_perm.md)
for tabular output.
