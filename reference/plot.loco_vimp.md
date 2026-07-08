# Plot LOCO Variable Importance

Draws a horizontal variable-importance bar chart of
[`loco()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/loco.md)
scores, sorted from most to least important (largest at the top).

## Usage

``` r
# S3 method for class 'loco_vimp'
plot(x, fill.sig = "darkorange", ...)
```

## Arguments

- x:

  An object of class `"loco_vimp"` returned by
  [`loco()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/loco.md).

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
horizontal bars (`alpha = 0.75`) end in a black diamond tip point over a
gray zero reference line, with the x-axis hugged to the data. Bars are
filled by significance: a covariate is significant when its (one-sided)
`p.value` is below `alpha` (the value supplied to
[`loco()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/loco.md));
significant bars use `fill.sig`, others `"gray70"`. In split-sample mode
a two-sided confidence-interval whisker is drawn from `CI.lower` to
`CI.upper` (the diamond tip sits at `Importance`, which is the interval
midpoint by construction). In OOB mode (`split = FALSE` in the original
[`loco()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/loco.md)
call) there is no confidence interval or p-value, so no whisker is
drawn, every bar takes the non-significant fill, and the title gains
"(OOB)".

## See also

[`loco()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/loco.md)
to compute the scores,
[`summary.loco_vimp()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/summary.loco_vimp.md)
for tabular output.

## Examples

``` r
set.seed(1995)
dat <- data.frame(y = rnorm(150), x1 = rnorm(150), x2 = rnorm(150),
                  x3 = rnorm(150))
dat$y <- dat$x1 + 0.5 * dat$x2 + rnorm(150, sd = 0.5)
mod <- ranger::ranger(y ~ ., data = dat, num.trees = 100)
vi <- loco(mod, split = FALSE)
plot(vi)

summary(vi)
#> LOCO Variable Importance
#>   n = 150  p = 3  method = oob  loss = abs 
#>   Mode: OOB (no inference) 
#> 
#>  Variable Importance CI.lower CI.upper p.value
#>        x1   0.430840       NA       NA      NA
#>        x2   0.094112       NA       NA      NA
#>        x3  -0.045574       NA       NA      NA
```
