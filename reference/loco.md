# LOCO Variable Importance for Outcome Models

Measures how much each covariate contributes to an outcome (non-causal)
forest by dropping it and seeing how much prediction error grows. Works
with `ranger` regression, probability, and classification forests, and
with `grf` regression, boosted regression, and probability forests. For
a
[`grf::causal_forest()`](https://rdrr.io/pkg/grf/man/causal_forest.html)
use
[`cf_loco()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/cf_loco.md)
instead; other model types are rejected.

## Usage

``` r
loco(
  model,
  data = NULL,
  alpha = 0.1,
  split = TRUE,
  method = c("z", "wilcox"),
  loss = c("auto", "abs", "mse", "brier", "zero_one", "log"),
  groups = NULL,
  bonf.correct = TRUE,
  seed = 1995,
  verbose = FALSE
)
```

## Arguments

- model:

  A fitted outcome forest. Supported classes:
  [`ranger::ranger()`](http://imbs-hl.github.io/ranger/reference/ranger.md)
  (regression, probability, or classification),
  [`grf::regression_forest()`](https://rdrr.io/pkg/grf/man/regression_forest.html),
  [`grf::boosted_regression_forest()`](https://rdrr.io/pkg/grf/man/boosted_regression_forest.html),
  or
  [`grf::probability_forest()`](https://rdrr.io/pkg/grf/man/probability_forest.html).
  Survival, causal, quantile, instrumental, multi-arm, and `lm_forest`
  objects are rejected.

- data:

  Optional. A data frame containing the variables used to fit `model`.
  Only meaningful for ranger models. If `NULL` (default), `loco()` tries
  to recover the training data from `model$call` by evaluating in
  [`parent.frame()`](https://rdrr.io/r/base/sys.parent.html). Pass
  `data` explicitly when calling `loco()` from a different scope than
  the original fit (e.g. inside another function). For grf models the
  training data is read from `model$X.orig` / `model$Y.orig` (or, for
  boosted forests, `model$forests[[1]]$X.orig` /
  `model$forests[[1]]$Y.orig`); any user-supplied `data` is ignored with
  a warning.

- alpha:

  Significance level for split-mode confidence intervals. Default is
  `0.1` (90% intervals). Only used when `split = TRUE`.

- split:

  Logical. If `TRUE` (default), uses split-sample LOCO. If `FALSE`, uses
  OOB prediction-error differences (no inference).

- method:

  One of `"z"` (default) or `"wilcox"`. Only used when `split = TRUE`.
  `"z"` uses a normal-theory Z-test on the mean of loss-residual
  differences; `"wilcox"` uses a Wilcoxon signed-rank test on the same
  differences.

- loss:

  One of `"auto"` (default), `"abs"`, `"mse"`, `"brier"`, `"zero_one"`,
  or `"log"`. Loss function used to define the per-observation residual.
  `"auto"` resolves at runtime based on the forest type:

  - Regression (ranger or grf) -\> `"abs"` (absolute deviation).

  - Probability estimation (ranger or grf) -\> `"brier"` (multi-class
    Brier score).

  - Classification (ranger only) -\> `"zero_one"` (misclassification).

  Disallowed combinations raise an error. In particular, `"brier"` and
  `"log"` require a probability forest.

- groups:

  Optional. Specifies group-LOCO. If `NULL` (default), per-variable LOCO
  is performed (one row per predictor). If a character vector, treated
  as a single (unnamed) group dropped jointly. If a named list of
  character vectors, each element is one group dropped jointly. Groups
  must not overlap, must contain only known predictor names, must be
  non-empty, and must leave at least one predictor remaining after the
  drop.

- bonf.correct:

  Logical. If `TRUE` (default), p-values and confidence intervals are
  Bonferroni-corrected across all tested variables / groups.

- seed:

  Integer seed for reproducibility. Default is `1995`. Honored in both
  split and OOB modes.

- verbose:

  Logical. Retained for backward compatibility; no longer used. Default
  is `FALSE`. Formerly controlled progress printing from a
  conformal-inference code path that has been removed; any value is
  accepted and has no effect on the computation or the return value.

## Value

An object of class `"loco_vimp"` with components:

- vimp:

  Data frame sorted by descending importance, with columns `Variable`
  (covariate or group name), `Importance` (LOCO importance score),
  `CI.lower`, `CI.upper` (confidence interval bounds; `NA` in OOB mode),
  `p.value` (one-sided p-value testing H0: importance \\\le\\ 0; `NA` in
  OOB mode), and, only when `groups` is supplied, `Members` (list-column
  of character vectors naming each group's members).

- n:

  Sample size.

- p:

  Number of covariates in the fitted model (not the number of groups,
  when `groups` is supplied).

- method:

  `"z"`, `"wilcox"`, or `"oob"`.

- loss:

  Loss function used (`"abs"`, `"mse"`, `"brier"`, `"zero_one"`, or
  `"log"`).

- split:

  Logical; whether split-sample mode was used.

- alpha:

  The `alpha` argument value.

- bonf.correct:

  The `bonf.correct` argument value.

- backend:

  The detected model backend (`"ranger"`, `"grf_reg"`, `"grf_brf"`, or
  `"grf_prob"`).

- group:

  Logical; whether group-LOCO was used.

## Details

Two modes:

- **Split-sample** (`split = TRUE`, default): splits the data, refits,
  and reports importance scores with confidence intervals and one-sided
  p-values. Slower, but gives valid inference.

- **OOB** (`split = FALSE`): refits once per covariate (or group) and
  compares out-of-bag error to the full model. Faster, but returns point
  estimates only – no confidence intervals or p-values.

**One-sided p-values.** The split-mode tests are one-sided against the
null that the covariate / group has no incremental predictive value.
Small p-values indicate the covariate helps prediction.

**Loss / forest-type compatibility.**

- Regression (ranger or grf):

  `"abs"` (default) or `"mse"`.

- Probability estimation (ranger or grf):

  `"brier"` (default), `"zero_one"` (argmax then 0/1), or `"log"`
  (negative log-likelihood, clipped at 1e-12).

- Classification (ranger only):

  `"zero_one"` only. To use Brier or log-loss, refit with
  `probability = TRUE` (ranger) or use
  [`grf::probability_forest()`](https://rdrr.io/pkg/grf/man/probability_forest.html).

**Group LOCO.** When `groups` is supplied, importance is computed
jointly for each group: the reduced model omits all members of the group
simultaneously. Useful when (a) factor predictors have been
one-hot-encoded into multiple columns whose individual importances are
not the quantity of interest, (b) an index or scale is represented by
several items, or (c) interest is in the contribution of a theoretically
motivated block. Group names appear in the `variable` column; member
lists appear in the `members` column.

**Why no OOB inference?** A naive Wald-style Z-test on per-observation
OOB error differences is anti-conservative: OOB residuals are positively
dependent across trees because each unit appears out-of-bag for an
overlapping set of trees. Small simulation checks (n=200, p=4, 20 reps)
at alpha=0.10 give roughly 30% Type-I error.

**Hyperparameter replay (ranger).** Core hyperparameters (`num.trees`,
`mtry`, `min.node.size`, `splitrule`, `replace`, `max.depth`) are read
directly from the fitted model object. Less-common arguments (e.g.
`sample.fraction`, `respect.unordered.factors`) are pulled from the
original call by evaluating in
[`parent.frame()`](https://rdrr.io/r/base/sys.parent.html); if any
cannot be resolved they are silently dropped with a single warning and
ranger's defaults are used in their place.

**Hyperparameter replay (grf).** Replay reads from
`model$tunable.params` plus stored scalars: `num.trees`,
`sample.fraction`, `mtry`, `min.node.size`, `honesty.fraction`,
`honesty.prune.leaves`, `alpha`, `imbalance.penalty`, `ci.group.size`,
`clusters`, and `equalize.cluster.weights`. The `honesty` flag itself is
not stored on a fitted grf forest; refits use grf's default (`TRUE`).
`tune.parameters` is forced to `"none"` on refit (no re-tuning). For
[`grf::boosted_regression_forest()`](https://rdrr.io/pkg/grf/man/boosted_regression_forest.html),
`boost.steps` is replayed as `length(model$forests)` and
`boost.error.reduction` reverts to grf's default (`0.97`); exact replay
is therefore approximate. `sample.weights` are not preserved.

**Edge cases.**

- Survival forests are rejected (both backends).

- Causal / quantile / instrumental / multi-arm / lm_forest (grf) are
  rejected; use
  [`cf_loco()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/cf_loco.md)
  for `causal_forest`.

- Factor predictors are allowed in ranger OOB mode but rejected in split
  mode because split-LOCO requires a numeric matrix. grf forests already
  require numeric X.

- Single-predictor models are rejected: LOCO requires \\p \ge 2\\.

- Groups that would leave zero remaining predictors are rejected.

## References

Lei, J., G'Sell, M., Rinaldo, A., Tibshirani, R. J., & Wasserman, L.
(2018). Distribution-Free Predictive Inference for Regression. *Journal
of the American Statistical Association*, 113(523), 1094–1111.
[doi:10.1080/01621459.2017.1307116](https://doi.org/10.1080/01621459.2017.1307116)

Rinaldo, A., Wasserman, L., & G'Sell, M. (2019). Bootstrapping and
Sample Splitting for High-Dimensional, Assumption-Lean Inference.
*Annals of Statistics*, 47(6), 3438–3469.
[doi:10.1214/18-AOS1820](https://doi.org/10.1214/18-AOS1820)

Williamson, B. D., Gilbert, P. B., Carone, M., & Simon, N. (2021).
Nonparametric variable importance assessment based on generalizations of
R^2. *Journal of the American Statistical Association*, 116(536),
1574–1587.
[doi:10.1080/01621459.2020.1812596](https://doi.org/10.1080/01621459.2020.1812596)

## See also

[`cf_loco()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/cf_loco.md)
for LOCO importance tailored to causal forests;
[`print.loco_vimp()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/print.loco_vimp.md),
[`summary.loco_vimp()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/summary.loco_vimp.md),
[`plot.loco_vimp()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/plot.loco_vimp.md)
for the object's S3 methods;
[`ranger::ranger()`](http://imbs-hl.github.io/ranger/reference/ranger.md),
[`grf::regression_forest()`](https://rdrr.io/pkg/grf/man/regression_forest.html),
[`grf::boosted_regression_forest()`](https://rdrr.io/pkg/grf/man/boosted_regression_forest.html),
[`grf::probability_forest()`](https://rdrr.io/pkg/grf/man/probability_forest.html)
for supported model fitters.

## Examples

``` r
# \donttest{
if (requireNamespace("ranger", quietly = TRUE)) {
  set.seed(1995)
  dat <- data.frame(y = rnorm(100), x1 = rnorm(100), x2 = rnorm(100),
                    x3 = rnorm(100))
  mod <- ranger::ranger(y ~ ., data = dat, num.trees = 50)
  loco(mod, split = FALSE)
  loco(mod, split = FALSE,
       groups = list(g1 = c("x1","x2"), g2 = "x3"))
  set.seed(2026)
  dat2 <- data.frame(y = factor(sample(0:1, 100, replace = TRUE)),
                     x1 = rnorm(100), x2 = rnorm(100), x3 = rnorm(100))
  mod_prob <- ranger::ranger(y ~ ., data = dat2, num.trees = 50,
                             probability = TRUE)
  loco(mod_prob, split = FALSE)            # auto -> "brier"
  loco(mod_prob, split = FALSE, loss = "log")
}
#> LOCO Variable Importance
#>   n = 100  p = 3  method = oob  loss = log 
#>   Mode: OOB (no inference) 
#> 
#>  Variable Importance CI.lower CI.upper p.value
#>        x2   0.097885       NA       NA      NA
#>        x3   0.073533       NA       NA      NA
#>        x1   0.028903       NA       NA      NA
if (requireNamespace("grf", quietly = TRUE)) {
  set.seed(1995)
  X <- matrix(rnorm(100 * 3), 100, 3)
  colnames(X) <- c("x1", "x2", "x3")
  Y <- X[, 1] + 0.5 * X[, 2] + rnorm(100, sd = 0.5)
  rf <- grf::regression_forest(X, Y, num.trees = 100)
  vi <- loco(rf, split = FALSE)            # auto -> "abs"
  summary(vi)
  if (requireNamespace("ggplot2", quietly = TRUE)) plot(vi)
  Yf <- factor(rbinom(100, 1, plogis(X[, 1])))
  pf <- grf::probability_forest(X, Yf, num.trees = 100)
  loco(pf, split = FALSE)                  # auto -> "brier"
}
#> LOCO Variable Importance
#>   n = 100  p = 3  method = oob  loss = abs 
#>   Mode: OOB (no inference) 
#> 
#>  Variable Importance CI.lower CI.upper p.value
#>        x1   0.435066       NA       NA      NA
#>        x2   0.056702       NA       NA      NA
#>        x3   0.002080       NA       NA      NA
#> LOCO Variable Importance
#>   n = 100  p = 3  method = oob  loss = brier 
#>   Mode: OOB (no inference) 
#> 
#>  Variable Importance CI.lower CI.upper p.value
#>        x1   0.063157       NA       NA      NA
#>        x2   0.005360       NA       NA      NA
#>        x3   0.005282       NA       NA      NA
# }
```
