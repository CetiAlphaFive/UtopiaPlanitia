# Auto-Selected Nuisance Causal Forest

Refits a
[`grf::causal_forest()`](https://rdrr.io/pkg/grf/man/causal_forest.html)
after replacing the conditional outcome `m(x) = E[Y | X = x]` and
propensity `e(x) = E[W | X = x]` with the best-performing nuisance
estimator from a candidate pool, scored by shared K-fold cross-fit
weighted CV loss. The CATE estimator and identification assumptions of
`grf` are unchanged — only the Robinson-residualisation nuisances are
swapped (and only if the winner beats grf's own baseline by a
configurable margin).

## Usage

``` r
autocf(
  c.forest,
  X = NULL,
  Y = NULL,
  W = NULL,
  K = 5L,
  seed = 1995L,
  eps = 0.001,
  tuning = c("orig", "cf.default", "cf.autotune"),
  pool = c("grf", "glmnet", "xgboost", "tabpfn"),
  min_improvement = "1se",
  term_evals = 10L,
  tabpfn_args = list(),
  glmnet_args = list(),
  xgboost_args = list(),
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

  Integer \>= 2. Number of cross-fit folds shared across all candidates.
  Default `5`.

- seed:

  Integer seed for fold assignment, per-candidate RNG, and the
  downstream
  [`grf::causal_forest()`](https://rdrr.io/pkg/grf/man/causal_forest.html)
  call. Default `1995`.

- eps:

  Numeric in `(0, 0.5)`. Propensity clipping bound for binary `W`
  (winner's predictions clipped to `[eps, 1 - eps]` to preserve
  overlap). Default `1e-3`.

- tuning:

  Character scalar controlling how
  [`grf::causal_forest()`](https://rdrr.io/pkg/grf/man/causal_forest.html)
  tunable hyperparameters are handled in the refit. One of `"orig"`
  (default, inherit verbatim from `c.forest$tunable.params`),
  `"cf.default"`, or `"cf.autotune"`. User `...` overrides win.

- pool:

  Character vector of candidate nuisance estimators to compare. Default
  `c("grf", "glmnet", "xgboost", "tabpfn")`. Any candidate whose
  required dependency is missing (or which is incompatible with
  non-trivial `sample.weights`) is dropped with a warning. The "grf"
  baseline is **always** refit under the same K-fold cross-fit protocol
  as the other candidates (using
  [`grf::regression_forest()`](https://rdrr.io/pkg/grf/man/regression_forest.html)
  on the held-out fold), so all CV losses are apples-to-apples.

- min_improvement:

  Either the character string `"1se"` (default), the character string
  `"0"`, or a non-negative numeric. Controls the margin by which a
  candidate must beat the grf baseline to trigger a nuisance swap.
  `"1se"` requires the mean fold-wise loss reduction to exceed the
  paired standard error of the fold differences (the 1-SE rule). `"0"`
  swaps on any improvement. A numeric value swaps when the absolute loss
  reduction exceeds it.

- term_evals:

  Integer \>= 0. Random-search budget for the xgboost AutoTuner (per
  outer fold per nuisance). `0` disables tuning and uses mlr3's default
  xgboost hyperparameters. Default `10L`.

- tabpfn_args, glmnet_args, xgboost_args:

  Optional named lists of per-candidate extra arguments. `tabpfn_args`
  is forwarded to
  [`tabpfn::tab_pfn()`](https://tabpfn.tidymodels.org/reference/tab_pfn.html);
  `glmnet_args` to
  [`glmnet::cv.glmnet()`](https://glmnet.stanford.edu/reference/cv.glmnet.html);
  `xgboost_args` is forwarded to the mlr3 `xgboost` learner's
  `param_set$values` (use to set, e.g., `nthread = 4` for multi-core CPU
  or `device = "cuda"` to enable GPU on systems with a CUDA xgboost
  build; defaults to mlr3's reproducibility-oriented `nthread = 1` and
  CPU). Tuned hyperparameters override matching keys. All default
  [`list()`](https://rdrr.io/r/base/list.html).

- verbose:

  Logical. Print per-candidate / per-fold progress. Default `FALSE`.

- ...:

  Additional arguments forwarded to
  [`grf::causal_forest()`](https://rdrr.io/pkg/grf/man/causal_forest.html)
  in the refit, overriding the values inherited from `c.forest` and the
  values implied by `tuning`.

## Value

A [`grf::causal_forest`](https://rdrr.io/pkg/grf/man/causal_forest.html)
object with auto-selected `Y.hat` and `W.hat`. The S3 class is
unchanged. An extra attribute `"autocf_meta"` records the comparison and
selection:

- K, seed, eps, w_type, clipped, tuning:

  Same as
  [`tabcf()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/tabcf.md)
  /
  [`glmcf()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/glmcf.md).

- pool:

  Pool requested by the caller.

- pool_run:

  Pool actually compared after dependency filtering.

- scores:

  Data frame of mean weighted CV losses per candidate (`y_loss`, `y_se`,
  `w_loss`, `w_se`).

- fold_losses:

  Long-format data frame of per-fold per-candidate weighted losses for
  diagnostics and re-derivation of the 1-SE rule.

- winner_y, winner_w:

  Selected nuisance estimator per role.

- swap_y, swap_w:

  Logical flags indicating whether the swap fired (TRUE) or grf's
  baseline was retained (FALSE).

- gap_y, gap_w:

  Mean fold-wise loss reduction (positive = candidate beats grf).

- min_improvement:

  The threshold used.

## Details

**What this function is for.** A diagnostic + drop-in replacement. grf's
default nuisance estimators (regression forests on Y and W) are usually
fine but not always best. `autocf()` runs a small candidate pool under a
shared K-fold cross-fit protocol with weighted CV loss (mean-squared
error for Y and continuous W; Brier score for binary W, which is
identical to MSE on 0/1 outcomes), reports the comparison table, and
only swaps in a competitor if it beats the grf baseline by a margin you
choose.

**Apples-to-apples grf baseline.** The "grf" candidate in the pool is
**not** scored using `c.forest$Y.hat` / `W.hat`, because those come from
grf's internal honest-split subsampling protocol — not strict K-fold
cross-fitting — and would be unfair to compare against true K-fold OOF
predictions from the other candidates. Instead, `autocf()` re-trains a
[`grf::regression_forest`](https://rdrr.io/pkg/grf/man/regression_forest.html)
on each held-out fold's training set and predicts on the held-out fold,
producing strict K-fold OOF predictions for the "grf" candidate
identically to every other candidate.

**1-SE rule for the swap.** With finite-sample CV noise any non-trivial
candidate will produce some random improvement on a particular seed.
Default `min_improvement = "1se"` requires the mean fold-wise loss
reduction to exceed the paired-fold standard error of that reduction:
\$\$\text{swap} \iff \bar d_K - \widehat{SE}(\bar d_K) \> 0, \quad d_k =
\text{loss}^{\text{grf}}\_k - \text{loss}^{\text{cand}}\_k.\$\$ This is
the same kind of conservativism that `glmnet`'s `lambda.1se` uses,
generalized to model selection. Use `min_improvement = "0"` to force any
improvement to fire, or supply a numeric absolute threshold.

**Weighted CV losses.** When `c.forest$sample.weights` is non-trivial,
both training and per-fold loss computation are weighted, so the loss
being minimised matches how the chosen nuisances will actually be used
downstream by grf. Candidates that cannot accept training weights
(currently `tabpfn`) are dropped from the pool with a warning when
weights are non-trivial.

**Selection-induced inference.** Picking the best of M candidates by CV
loss is a model-selection step. It is benign here because
[`grf::causal_forest()`](https://rdrr.io/pkg/grf/man/causal_forest.html)
uses a Neyman-orthogonal R-learner score for the second stage; small
perturbations in the chosen nuisance do not invalidate sqrt(n) inference
(Chernozhukov et al., 2018; Foster & Syrgkanis, 2019). Without
orthogonality this would not be safe.

**Convergence-rate caveat.** CV-loss minimization picks the lowest
finite-sample loss, not the best convergence rate. For valid sqrt(n)
downstream inference (e.g.
[`grf::average_treatment_effect()`](https://rdrr.io/pkg/grf/man/average_treatment_effect.html)),
chosen nuisances must converge at `o(n^{-1/4})`. The default pool
members (grf reg-forest, glmnet under sparsity, xgboost under boosting
consistency) are believed adequate at typical n; tabpfn has no formal
rate proof and should be treated as exploratory when downstream
confidence intervals matter.

**Reproducibility under `future` plans.** xgboost's mlr3 AutoTuner inner
CV runs through the `future` framework. To prevent non-deterministic
seeding under user-set `future::plan(multisession)` or similar,
`autocf()` forces `future::plan("sequential")` for the duration of its
call and restores the previous plan on exit.

## References

Chernozhukov, V., Chetverikov, D., Demirer, M., Duflo, E., Hansen, C.,
Newey, W., and Robins, J. (2018). Double/Debiased Machine Learning for
Treatment and Structural Parameters. *The Econometrics Journal*, 21(1),
C1–C68.

Foster, D. J. and Syrgkanis, V. (2019). Orthogonal Statistical Learning.
[doi:10.48550/arXiv.1901.09036](https://doi.org/10.48550/arXiv.1901.09036)

Robinson, P. M. (1988). Root-N-Consistent Semiparametric Regression.
*Econometrica*, 56(4), 931–954.

Athey, S., Tibshirani, J., and Wager, S. (2019). Generalized Random
Forests. *Annals of Statistics*, 47(2), 1148–1178.

## See also

[`grf::causal_forest()`](https://rdrr.io/pkg/grf/man/causal_forest.html)
for the underlying estimator,
[`tabcf()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/tabcf.md)
/
[`glmcf()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/glmcf.md)
for fixed-nuisance variants,
[`cf_loco()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/cf_loco.md)
and
[`omni_hetero()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/omni_hetero.md)
for downstream analysis.

## Examples

``` r
if (FALSE) { # \dontrun{
library(grf)
set.seed(1995)
n <- 400; p <- 5
X <- matrix(rnorm(n * p), n, p)
colnames(X) <- paste0("X", seq_len(p))
W <- rbinom(n, 1, 0.5)
Y <- X[, 1] * W + rnorm(n)
cf <- causal_forest(X, Y, W, num.trees = 200)

# Default: compare grf, glmnet, xgboost, tabpfn (whatever is installed)
cf2 <- autocf(cf, K = 5)
attr(cf2, "autocf_meta")$scores
attr(cf2, "autocf_meta")$winner_y
attr(cf2, "autocf_meta")$winner_w

# Smaller pool, force any improvement to swap
cf3 <- autocf(cf, pool = c("grf", "glmnet"), min_improvement = "0")
} # }
```
