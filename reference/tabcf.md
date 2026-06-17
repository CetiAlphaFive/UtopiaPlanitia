# Causal Forest with TabPFN-Estimated Nuisances

Refits a
[`grf::causal_forest()`](https://rdrr.io/pkg/grf/man/causal_forest.html)
after replacing the conditional outcome `m(x) = E[Y | X = x]` and
propensity `e(x) = E[W | X = x]` with cross-fitted predictions from
TabPFN (Hollmann et al., 2023). The CATE estimator and identification
assumptions of `grf` are unchanged — only the Robinson-residualisation
nuisances are swapped.

## Usage

``` r
tabcf(
  c.forest,
  X = NULL,
  Y = NULL,
  W = NULL,
  K = 5L,
  R = 1L,
  seed = 1995L,
  clip = FALSE,
  eps = NULL,
  tuning = c("orig", "cf.default", "cf.autotune"),
  tabpfn_args = list(),
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
  treatment vector. Useful when `c.forest` was stripped (no `X.orig`
  slot). If `NULL` (default) they are recovered from `c.forest`.

- K:

  Integer \>= 2. Number of folds for cross-fitting the TabPFN nuisances.
  Default `5`.

- R:

  Integer \>= 1. Number of repeated K-fold cross-fits. Each repeat uses
  a distinct fold partition (seeded `seed + (r - 1)`) and the per-unit
  nuisance predictions are averaged across repeats to reduce the
  Monte-Carlo variance of a single partition. `R = 1` (default)
  reproduces the original single cross-fit exactly. Cost scales as
  `R * K * 2` TabPFN fits.

- seed:

  Integer seed controlling fold assignment and the downstream
  [`grf::causal_forest()`](https://rdrr.io/pkg/grf/man/causal_forest.html)
  call. Default `1995`.

- clip:

  Propensity clipping control for binary `W`. One of:

  - `FALSE` (default): no clipping. If any averaged propensity falls
    outside `[0.01, 0.99]`, a warning reports the count and min/max.

  - `TRUE`: clip to `[1e-3, 1 - 1e-3]`.

  - `c(lo, hi)`: clip to `[lo, hi]` with `0 < lo < hi < 1`.

  When clipping is active it is applied within each repeat before
  averaging. Ignored for continuous `W`.

- eps:

  Deprecated; use `clip`. If supplied, maps to `clip = c(eps, 1 - eps)`
  with a deprecation warning. Passing both `eps` and `clip` is an error.
  Default `NULL`.

- tuning:

  Character scalar controlling how
  [`grf::causal_forest()`](https://rdrr.io/pkg/grf/man/causal_forest.html)
  tunable hyperparameters (`min.node.size`, `mtry`, `sample.fraction`,
  `honesty.fraction`, `honesty.prune.leaves`, `alpha`,
  `imbalance.penalty`) are handled in the refit. One of:

  - `"orig"` (default): inherit tunable params verbatim from
    `c.forest$tunable.params`. No re-tuning.

  - `"cf.default"`: do not pass any tunable params; let
    [`grf::causal_forest()`](https://rdrr.io/pkg/grf/man/causal_forest.html)
    use its hardcoded defaults. No re-tuning.

  - `"cf.autotune"`: do not pass inherited tunable params and set
    `tune.parameters = "all"` so
    [`grf::causal_forest()`](https://rdrr.io/pkg/grf/man/causal_forest.html)
    re-tunes under the new TabPFN nuisances.

  In all three modes, user-supplied arguments via `...` still override.

- tabpfn_args:

  Named list of additional arguments forwarded to
  [`tabpfn::tab_pfn()`](https://tabpfn.tidymodels.org/reference/tab_pfn.html)
  (e.g. `list(num_estimators = 4L, softmax_temperature = 0.9)`). To
  control the device or precision, pass a `control` element built with
  [`tabpfn::control_tab_pfn()`](https://tabpfn.tidymodels.org/reference/control_tab_pfn.html)
  (e.g. `list(control = tabpfn::control_tab_pfn(device = "cpu"))`); if
  no `control` is supplied, `tabcf()` builds one per fold with
  `random_state = seed + k`. Default
  [`list()`](https://rdrr.io/r/base/list.html).

- verbose:

  Logical. If `TRUE`, print fold-by-fold progress. Default `FALSE`.

- ...:

  Additional arguments forwarded to
  [`grf::causal_forest()`](https://rdrr.io/pkg/grf/man/causal_forest.html),
  overriding the values inherited from `c.forest` and the values implied
  by `tuning`.

## Value

A [`grf::causal_forest`](https://rdrr.io/pkg/grf/man/causal_forest.html)
object with TabPFN-derived `Y.hat` and `W.hat`. The S3 class is
unchanged, so [`summary()`](https://rdrr.io/r/base/summary.html),
[`plot()`](https://rdrr.io/r/graphics/plot.default.html),
[`cf_loco()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/cf_loco.md),
and
[`omni_hetero()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/omni_hetero.md)
work on it transparently. An extra attribute `"tabcf_meta"` records the
cross-fit configuration:

- K:

  Number of folds.

- R:

  Number of repeated cross-fits.

- seed:

  Seed used.

- clip:

  The resolved clip setting: `FALSE` (no clipping) or the numeric
  `c(lo, hi)` bounds used.

- w_type:

  `"binary"` or `"continuous"`.

- clipped:

  Number of propensity predictions clipped to the active `clip` bounds
  (binary `W` only; `0` when `clip = FALSE`).

- tuning:

  The `tuning` mode used (`"orig"`, `"cf.default"`, or `"cf.autotune"`).

## Details

**What changes vs.
[`grf::causal_forest()`](https://rdrr.io/pkg/grf/man/causal_forest.html)?**
Only the nuisance predictions used to centre `Y` and `W`. The CATE
estimator, honesty splits, and identification assumptions are inherited
from grf.

**Why TabPFN?** TabPFN is a transformer pre-trained on synthetic tabular
generative processes and frequently outperforms gradient boosting on
small-to-medium tabular regression and binary classification tasks.
Better nuisance fits reduce bias in the Robinson (1988) residualisation
step that underlies
[`grf::causal_forest()`](https://rdrr.io/pkg/grf/man/causal_forest.html).

**Cross-fitting and repeats.** For each of `R` repeats, TabPFN nuisance
predictions are produced via K-fold cross-fitting under a distinct fold
partition; the per-unit predictions are then averaged across repeats.
With `R = 1` this is a single cross-fit identical to earlier versions.
If `c.forest$clusters` is non-trivial, folds keep each cluster intact.

**Treatment type.** `W` is treated as binary if it is `{0, 1}`-valued
(numeric or integer with both levels present), a logical, or a 2-level
factor; in that case TabPFN's classifier is fit and propensity scores
are optionally clipped per the `clip` argument (default no clipping,
with an overlap warning if any averaged propensity leaves
`[0.01, 0.99]`). Otherwise `W` is treated as continuous and TabPFN's
regressor is fit. If `W` is numeric with exactly 2 unique values that
are not `{0, 1}` (e.g. `{1, 2}` or `{-1, 1}`), `tabcf()` emits a warning
and falls back to the regressor; encode such treatments as `0/1` to use
the classifier.

**Tuning.** The `tuning` argument controls how
[`grf::causal_forest()`](https://rdrr.io/pkg/grf/man/causal_forest.html)
tunable hyperparameters (`min.node.size`, `mtry`, `sample.fraction`,
`honesty.fraction`, `honesty.prune.leaves`, `alpha`,
`imbalance.penalty`) are handled in the refit:

- `"orig"` (default): inherit verbatim from `c.forest$tunable.params`.
  This preserves whatever grf chose (or the user supplied) for the
  original forest. No re-tuning.

- `"cf.default"`: omit those keys entirely so
  [`grf::causal_forest()`](https://rdrr.io/pkg/grf/man/causal_forest.html)
  falls back on its hardcoded defaults. Use this when the original
  forest's tuning was idiosyncratic and you want a clean baseline. No
  re-tuning.

- `"cf.autotune"`: omit the inherited keys and set
  `tune.parameters = "all"` so
  [`grf::causal_forest()`](https://rdrr.io/pkg/grf/man/causal_forest.html)
  re-tunes from scratch under the new TabPFN nuisances. The most
  expensive option but the only one that lets the tuner respond to the
  new nuisance fit. Pass `tune.num.trees`, `tune.num.reps`,
  `tune.num.draws` via `...` if you need non-default tuner controls.

In all three modes, anything the caller passes via `...` overrides the
value chosen by `tuning`.

**TabPFN seed passthrough.** Reproducibility for TabPFN is governed by
`random_state` inside the `control` argument of
[`tabpfn::tab_pfn()`](https://tabpfn.tidymodels.org/reference/tab_pfn.html).
If `tabpfn_args` does not already contain a `control` element, `tabcf()`
builds one per fold via
`tabpfn::control_tab_pfn(random_state = seed + k)` so each fold gets a
deterministic but distinct TabPFN seed derived from the user-provided
`seed`. If the user supplies `tabpfn_args$control`, that control object
is passed through verbatim for every fold (so the user is responsible
for any seeding).

**Dependencies and TabPFN backend.** The `tabpfn` R package
(tidymodels-style wrapper around the Python TabPFN backend, v0.1.0+) is
gated as a Suggests dependency. `tabcf()` calls
`rlang::check_installed("tabpfn")` at the top and raises an informative
error if missing. The Python backend additionally requires a one-time
license acceptance: register at <https://ux.priorlabs.ai>, accept the
license, copy your API key, and set `TABPFN_TOKEN` in the environment
before calling `tabcf()`. Without it, the underlying Python call raises
`TabPFNLicenseError`. The easiest way to set the token from inside R is
to call
[`setup_tabpfn_token()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/setup_tabpfn_token.md),
which checks for the variable, walks you through registering at
<https://ux.priorlabs.ai/account>, and optionally appends
`TABPFN_TOKEN=...` to your `~/.Renviron` so future sessions pick it up
automatically. `tabcf()` itself calls
[`setup_tabpfn_token()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/setup_tabpfn_token.md)
when the variable is missing in an interactive session.

## References

Hollmann, N., Müller, S., Eggensperger, K., and Hutter, F. (2023).
TabPFN: A Transformer That Solves Small Tabular Classification Problems
in a Second. *ICLR*.
[doi:10.48550/arXiv.2207.01848](https://doi.org/10.48550/arXiv.2207.01848)

Hollmann, N., Müller, S., Purucker, L., Krishnakumar, A., Körfer, M.,
Hoo, S. B., Schirrmeister, R. T., and Hutter, F. (2025). Accurate
Predictions on Small Data with a Tabular Foundation Model. *Nature*.

Robinson, P. M. (1988). Root-N-Consistent Semiparametric Regression.
*Econometrica*, 56(4), 931–954.

Athey, S., Tibshirani, J., and Wager, S. (2019). Generalized Random
Forests. *Annals of Statistics*, 47(2), 1148–1178.
[doi:10.1214/18-AOS1709](https://doi.org/10.1214/18-AOS1709)

## See also

[`grf::causal_forest()`](https://rdrr.io/pkg/grf/man/causal_forest.html)
for the underlying estimator,
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

# default: inherit grf's tuned params from cf
cf2 <- tabcf(cf, K = 5)

# use grf's hardcoded defaults
cf3 <- tabcf(cf, K = 5, tuning = "cf.default")

# re-tune from scratch under TabPFN nuisances
cf4 <- tabcf(cf, K = 5, tuning = "cf.autotune")

# repeated 10x 5-fold cross-fit, clip propensities to [0.01, 0.99]
cf5 <- tabcf(cf, K = 5, R = 10, clip = c(0.01, 0.99))
} # }
```
