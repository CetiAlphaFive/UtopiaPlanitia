# Causal Forest with cv.glmnet-Estimated Nuisances

Refits a
[`grf::causal_forest()`](https://rdrr.io/pkg/grf/man/causal_forest.html)
after replacing the conditional outcome `m(x) = E[Y | X = x]` and
propensity `e(x) = E[W | X = x]` with cross-fitted predictions from
[`glmnet::cv.glmnet()`](https://glmnet.stanford.edu/reference/cv.glmnet.html)
(penalized elastic-net regression / classification, optionally relaxed).
The CATE estimator and identification assumptions of `grf` are unchanged
— only the Robinson-residualisation nuisances are swapped.

## Usage

``` r
glmcf(
  c.forest,
  X = NULL,
  Y = NULL,
  W = NULL,
  K = 5L,
  seed = 1995L,
  eps = 0.001,
  tuning = c("orig", "cf.default", "cf.autotune"),
  alpha = c(0, 0.25, 0.5, 0.75, 1),
  s = "lambda.min",
  relax = TRUE,
  glmnet_args = list(),
  verbose = FALSE,
  ...
)
```

## Arguments

- c.forest:

  A fitted causal forest object from the grf package, used both to
  recover `(X, Y, W)` (via `c.forest$X.orig`, `c.forest$Y.orig`,
  `c.forest$W.orig`) and to inherit tunable hyperparameters for the
  refit.

- X, Y, W:

  Optional overrides for the design matrix, outcome vector, and
  treatment vector. If `NULL` (default) they are recovered from
  `c.forest`.

- K:

  Integer \>= 2. Number of folds for cross-fitting the cv.glmnet
  nuisances. Default `5`.

- seed:

  Integer seed controlling fold assignment, the inner cv.glmnet fold
  ids, and the downstream
  [`grf::causal_forest()`](https://rdrr.io/pkg/grf/man/causal_forest.html)
  call. Default `1995`.

- eps:

  Numeric in `(0, 0.5)`. Propensity clipping bound for binary `W`:
  cv.glmnet binomial predictions are clipped to `[eps, 1 - eps]` to
  preserve overlap. Default `1e-3`.

- tuning:

  Character scalar controlling how
  [`grf::causal_forest()`](https://rdrr.io/pkg/grf/man/causal_forest.html)
  tunable hyperparameters are handled in the refit. One of `"orig"`
  (default, inherit verbatim from `c.forest$tunable.params`),
  `"cf.default"` (let grf use its hardcoded defaults), or
  `"cf.autotune"` (set `tune.parameters = "all"` and re-tune). User
  `...` overrides win.

- alpha:

  Numeric vector of elastic-net mixing parameters in `[0, 1]`. If length
  \> 1 (default `c(0, 0.25, 0.5, 0.75, 1)`), each candidate is fit with
  `cv.glmnet()` using a shared fold id (so CV scores are comparable),
  and the alpha with the lowest minimum cross-validated deviance is
  selected per fold and per nuisance. If length 1, no alpha tuning.

- s:

  Lambda value passed to `predict.cv.glmnet()`. Default `"lambda.min"`
  (CV minimum). `"lambda.1se"` is the sparser one-standard-error rule.

- relax:

  Logical. If `TRUE` (default), `cv.glmnet()` is called with
  `relax = TRUE`, fitting an unpenalized model on the active set at each
  lambda and CV-tuning a blending parameter `gamma` in `[0, 1]`.
  Improves prediction when the true signal is sparse but coefficients
  are biased by the lasso shrinkage.

- glmnet_args:

  Named list of additional arguments forwarded to
  [`glmnet::cv.glmnet()`](https://glmnet.stanford.edu/reference/cv.glmnet.html)
  (e.g. `list(nfolds = 5L, nlambda = 50L)`). Note: `nfolds`/`foldid`
  supplied here are overridden by the internal shared `foldid` used for
  alpha tuning.

- verbose:

  Logical. If `TRUE`, print fold-by-fold progress. Default `FALSE`.

- ...:

  Additional arguments forwarded to
  [`grf::causal_forest()`](https://rdrr.io/pkg/grf/man/causal_forest.html),
  overriding the values inherited from `c.forest` and the values implied
  by `tuning`.

## Value

A [`grf::causal_forest`](https://rdrr.io/pkg/grf/man/causal_forest.html)
object with cv.glmnet-derived `Y.hat` and `W.hat`. The S3 class is
unchanged. An extra attribute `"glmcf_meta"` records the cross-fit
configuration:

- K:

  Number of cross-fit folds.

- seed:

  Seed used.

- eps:

  Propensity clipping bound used (binary `W` only).

- w_type:

  `"binary"` or `"continuous"`.

- clipped:

  Number of propensity predictions clipped (binary `W` only).

- tuning:

  The `tuning` mode used.

- alpha_grid:

  The alpha grid searched.

- alpha_y, alpha_w:

  Selected alpha per fold for Y and W.

- relax:

  Whether `relax = TRUE` was used.

- s:

  The lambda spec used at predict time.

## Details

**What changes vs.
[`grf::causal_forest()`](https://rdrr.io/pkg/grf/man/causal_forest.html)?**
Only the nuisance predictions used to centre `Y` and `W`. The CATE
estimator, honesty splits, and identification assumptions are inherited
from grf.

**Why cv.glmnet?** Penalized regression is a fast, well-understood
baseline for nuisance estimation in Robinson-style residualisation
(Chernozhukov et al., 2018, treat lasso/elastic-net nuisances as the
canonical DML first stage).

**Linearity-in-X assumption (load-bearing).** `glmcf()` fits cv.glmnet
on `X` as supplied, so `Y.hat` and `W.hat` are linear in the columns of
`X` (with elastic-net penalty + optional relax). If the true
`m(x) = E[Y | X]` or `e(x) = E[W | X]` is non-linear in `X`, the lasso
residuals carry residual confounding correlated with `X`, which biases
the Robinson centering and propagates into the CATE estimates
downstream. grf's residual-on-residual splits absorb some of this but
not all. **For non-linear nuisances, pre-transform `X` (interactions,
splines, polynomials) before passing to `glmcf()`, or use
[`tabcf()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/tabcf.md)
which handles non-linearity natively.**

**DML rate requirement for downstream inference.** For valid sqrt(n)
inference from
[`grf::average_treatment_effect()`](https://rdrr.io/pkg/grf/man/average_treatment_effect.html)
(and similar), DML theory requires both nuisances to converge at
`o(n^{-1/4})`. Lasso achieves `s_n * sqrt(log(p) / n)` under approximate
sparsity (effective sparsity `s_n`, ambient dimension `p`). At small `p`
and sparse signal this is satisfied; at moderate `p` (e.g.
`p > sqrt(n)`) or non-sparse signal it is **not**, and ATE confidence
intervals from `grf` will under-cover. If you are in a wide-X /
non-sparse regime, prefer
[`tabcf()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/tabcf.md)
or random-forest nuisances.

**Cross-fitting.** Predictions are produced via K-fold cross-fitting:
for each fold, `cv.glmnet()` is fit on the other `K - 1` folds and
predicts on the held-out fold. If `c.forest$clusters` is non-trivial,
folds are built so that all units in a cluster end up in the same fold
(preserving grf's cluster-aware inference).

**Treatment type.** `W` is treated as binary if it is `{0, 1}`-valued
(numeric or integer with both levels present), a logical, or a 2-level
factor; in that case `family = "binomial"` is used and predictions are
clipped to `[eps, 1 - eps]` to preserve overlap. Otherwise `W` is
treated as continuous and `family = "gaussian"` is used. If `W` is
numeric with exactly 2 unique values that are not `{0, 1}` (e.g.
`{1, 2}` or `{-1, 1}`), `glmcf()` emits a warning and falls back to the
gaussian regressor; encode such treatments as `0/1` to use the
classifier.

**Alpha tuning and reproducibility.** Within each cross-fit fold, the
inner CV is run with a single shared `foldid` across all candidate
alphas so the minimum-CV-deviance comparison across alphas is on the
same splits. The selected alpha may differ across folds and between Y
and W. Per-fold reselection is consistent in rate but data-dependent: at
small `n_train`, differences in CV deviance across alphas are within
sampling error, so the selected alpha can vary under resampling (the
actual selections are recorded in `attr(out, "glmcf_meta")$alpha_y` and
`$alpha_w` for diagnostics). For studies that need reproducible CIs
under resampling (bootstrap, Monte Carlo), prefer a single fixed alpha
(e.g. `alpha = 1` for pure lasso) over the default grid.

**Sample weights.** `c.forest$sample.weights` are passed to
`cv.glmnet()` via the `weights` argument so that weighted training is
consistent between the nuisance fits and the downstream causal forest.

**Dependencies.** The `glmnet` package is gated as a Suggests
dependency. `glmcf()` calls `rlang::check_installed("glmnet")` at the
top and raises an informative error if missing.

## References

Friedman, J., Hastie, T., and Tibshirani, R. (2010). Regularization
Paths for Generalized Linear Models via Coordinate Descent. *Journal of
Statistical Software*, 33(1), 1–22.

Hastie, T., Tibshirani, R., and Tibshirani, R. (2020). Best Subset,
Forward Stepwise, or Lasso? Analysis and Recommendations Based on
Extensive Comparisons. *Statistical Science*, 35(4), 579–592.

Robinson, P. M. (1988). Root-N-Consistent Semiparametric Regression.
*Econometrica*, 56(4), 931–954.

Athey, S., Tibshirani, J., and Wager, S. (2019). Generalized Random
Forests. *Annals of Statistics*, 47(2), 1148–1178.
[doi:10.1214/18-AOS1709](https://doi.org/10.1214/18-AOS1709)

## See also

[`grf::causal_forest()`](https://rdrr.io/pkg/grf/man/causal_forest.html)
for the underlying estimator,
[`tabcf()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/tabcf.md)
for the TabPFN-nuisance analogue,
[`cf_loco()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/cf_loco.md)
and
[`omni_hetero()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/omni_hetero.md)
for downstream analysis.

## Examples

``` r
if (FALSE) { # \dontrun{
library(grf)
set.seed(1995)
n <- 200; p <- 5
X <- matrix(rnorm(n * p), n, p)
colnames(X) <- paste0("X", seq_len(p))
W <- rbinom(n, 1, 0.5)
Y <- X[, 1] * W + rnorm(n)
cf  <- causal_forest(X, Y, W, num.trees = 200)

# default: tune alpha over {0, .25, .5, .75, 1}, relax = TRUE
cf2 <- glmcf(cf, K = 5)

# pure lasso, no relax
cf3 <- glmcf(cf, K = 5, alpha = 1, relax = FALSE)
} # }
```
