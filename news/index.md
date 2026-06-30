# Changelog

## UtopiaPlanitia (development version)

### `cf_perm()` gains opt-in missing-covariate handling

- **New `allow.missing` argument (default `FALSE`, appended last).**
  With complete data it is inert and results are byte-for-byte
  unchanged. When `X` contains `NA`, `allow.missing = FALSE` now errors
  with a user-confronting message that names the two scopes and explains
  the estimand choice (previously a terse “does not support missing
  values”). Setting `allow.missing = "observed"` or `"marginal"` opts
  into **observed-support conditional permutation**: the
  conditional-permutation nuisance is fit on observed-label rows only
  (`X_{-j}` missingness is still routed via grf’s MIA, never imputed),
  perturbed values are spliced back into observed rows, and rows with
  `NA` in `X_j` are left unchanged so their per-row loss delta is
  exactly zero. `"observed"` averages over observed rows (importance
  conditional on `X_j` observed); `"marginal"` averages over all rows
  (auto-discounted by the missingness rate).
- **The discrete conditional model switches `probability_forest` -\>
  `regression_forest` only on the opt-in missingness branch** (binary
  covariates draw `Bernoulli(p_hat)` on their two observed levels; other
  supports residual-shuffle). This switch is scoped entirely to the
  missingness branch, so **complete-data results are unchanged**.
  Covariates with too few observed values degrade to importance 0 /
  p-value 1 with a warning.
- **New `miss.rate` element on the returned object** (named numeric,
  length `p`) and a per-covariate **missingness table** in
  `print`/`summary`, shown only when some covariate has missing values
  (the header names the active scope). The `vimp` data frame structure
  is unchanged.

### New function `cf_perm()` — PermuCATE variable importance

- **[`cf_perm()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/cf_perm.md)
  implements PermuCATE** (Paillard et al., 2025):
  conditional-permutation variable importance for a fitted `grf` causal
  forest, returning importance scores with one-sided p-values and
  confidence bounds. It complements
  [`cf_loco()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/cf_loco.md)
  — it permutes rather than refits, has lower variance, and reports
  inference. `loss = "R"` (default, Robinson residual) uses the forest’s
  `Y.hat`/`W.hat`; `loss = "AIPW"` uses
  [`grf::get_scores()`](https://rdrr.io/pkg/grf/man/get_scores.html).
  The default light path scores the supplied forest in place
  (approximate, conservative SEs); `cross.fit = TRUE` opts into K-fold
  refitting with Nadeau-Bengio inference (R-loss only). Ships with
  `print`/`summary`/`plot` methods.

### `plot_pdp()` gains a discrete subgroup ATE mode

- **New `subgroup` argument (default `FALSE`).** For a binary or
  low-cardinality integer covariate, `subgroup = TRUE` plots the
  doubly-robust (AIPW) subgroup average treatment effect at each
  observed value of `x_var` — points with 95% CIs (1-way), or a heatmap
  of subgroup ATEs with significance asterisks over the `x_var` x
  `y_var` cross (2-way) — instead of a smooth partial-dependence curve.
  Subgroup ATEs come from
  `grf::average_treatment_effect(cf, subset = ...)`. `subgroup = FALSE`
  is the default and reproduces existing behavior exactly.

### `cf_loco()` screening + correlation diagnostic

- **`screen = TRUE` now drops only zero-importance covariates.**
  Auto-screening keeps every variable with non-zero split-frequency
  importance (previously: above the mean, which over-selected). The
  interactive runtime prompt was removed.
- **New `verbose` argument (default `TRUE`)** prints the
  conditioning-variable correlation matrix after the refits and warns
  when any covariate pair is correlated above `|r| = 0.5`. Set
  `verbose = FALSE` to silence. The warning is suppressed when variables
  are grouped via `group.by.corr` or `variable.groups`. Note: with the
  default `verbose = TRUE`, existing calls now emit this matrix and
  (when applicable) the new warning.

### `omni_hetero()` exposes the Sequential RATE fold count

- **New `num.folds` argument** (default `5`) controls the number of
  folds K in the Sequential RATE test (Wager, 2024). Must be an integer
  `>= 3` and no greater than the sample size; `5` or more is
  recommended. The upfront sample-size warning now scales with the
  chosen fold count.
- **[`summary.causal_forest()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/summary.causal_forest.md)
  forwards `...`** to
  [`omni_hetero()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/omni_hetero.md),
  so `summary(cf, num.folds = 10)` works.

### `tabcf()` gains repeated cross-fitting and clip control

- **New `R` argument for repeated K-fold cross-fitting.** Nuisance
  predictions are averaged over `R` independent fold partitions to cut
  Monte-Carlo variance. `R = 1` (default) is identical to previous
  behavior.
- **New `clip` argument** controls binary-propensity clipping: `FALSE`
  (default; no clipping, with an overlap warning), `TRUE` (clip to
  `[1e-3, 1 - 1e-3]`), or `c(lo, hi)` for custom bounds.
- **Behavior change:** binary propensities are **no longer clipped by
  default**. Previous versions always clipped at `1e-3`; the default is
  now warn-only. Pass `clip = TRUE` to restore the old clipping.
- **`eps` is deprecated** in favor of `clip`. It maps to
  `clip = c(eps, 1 - eps)` with a deprecation warning.

### `loco()` gains grf backend support

- **[`loco()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/loco.md)
  now accepts `grf` outcome forests** in addition to `ranger`. Supported
  classes:
  [`grf::regression_forest()`](https://rdrr.io/pkg/grf/man/regression_forest.html),
  [`grf::boosted_regression_forest()`](https://rdrr.io/pkg/grf/man/boosted_regression_forest.html),
  and
  [`grf::probability_forest()`](https://rdrr.io/pkg/grf/man/probability_forest.html).
  Causal, survival, quantile, instrumental, multi-arm, and `lm_forest`
  objects are rejected with informative error messages; for
  [`causal_forest()`](https://rdrr.io/pkg/grf/man/causal_forest.html)
  the error points users to
  [`cf_loco()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/cf_loco.md).
- Backend is **auto-detected from the model class** — there is no new
  argument and no behavior change for existing ranger users.
- Hyperparameters are replayed off the fitted grf object
  (`tunable.params` plus `num.trees`, `ci.group.size`, `clusters`,
  `equalize.cluster.weights`). For
  [`grf::boosted_regression_forest()`](https://rdrr.io/pkg/grf/man/boosted_regression_forest.html),
  `boost.steps` is replayed as `length(model$forests)`;
  `boost.error.reduction` reverts to grf’s default. `sample.weights` and
  `honesty` flags are not preserved.
- `data = ...` is ignored for grf models with a one-shot warning; grf
  forests store their training data internally.
- Split-mode for grf models always uses the internal custom split loop
  (the
  [`conformalInference::loco()`](https://rdrr.io/pkg/conformalInference/man/loco.html)
  fast path remains ranger-only).
- New tests in `tests/testthat/test-loco-grf.R`.

### `loco()` gains classification DV support and group-LOCO

- **Classification / probability forests are now first-class.** The
  `treetype` guard has been relaxed: regression, probability estimation,
  and (hard) classification ranger forests are all supported. Survival
  forests remain rejected.
- **New `loss` argument** (`"auto"`, `"abs"`, `"mse"`, `"brier"`,
  `"zero_one"`, `"log"`). Defaults to `"auto"` which resolves to `"abs"`
  for regression (back-compat with the historic conformal-LOCO
  convention), `"brier"` for probability forests, and `"zero_one"` for
  classification forests. Disallowed combinations (e.g. `"brier"` on a
  hard-classification forest) raise an informative error pointing the
  user to refit with `probability = TRUE`.
- **New `groups` argument** for group-LOCO. Pass `NULL` (default) for
  per-variable behavior; pass a character vector for a single unnamed
  group; pass a named list of character vectors for multiple groups
  dropped jointly. Validates non-overlap, known names, non-emptiness,
  and that at least one predictor remains after the drop.
- **New `members` column** appears in the output when `groups` is
  supplied; it is a list-column of character vectors naming each group’s
  members.
- **New `loss` column** is added unconditionally to the output so
  downstream consumers can tell which residual flavor produced the row.
- **Split mode dispatch:** when `groups = NULL` and the model is a
  regression forest with `loss = "abs"`, we still delegate to
  \[conformalInference::loco()\] (bit-for-bit back-compat with the prior
  split-mode results). Every other split-mode configuration
  (classification, probability, custom loss, or any group LOCO) is
  handled by a new in-package custom split loop that mirrors
  conformalInference’s sample-splitting + one-sided Z and Wilcoxon
  inference but drops *sets* of columns and computes user-defined
  per-observation loss residuals.

#### Backward compatibility for the classification + group features

The default-argument call signature `loco(model)` is unchanged. Existing
per-variable regression callers see the same `variable`, `importance`,
(`ci.lower`, `ci.upper`, `p.value`,) `method` columns; the only output
diff is the new `loss` column (always `"abs"` on existing regression
calls).

#### Small-sim evidence (classification + group)

- Brier-LOCO Type-I rate on classification null variables (n=250,
  trees=200, 20 reps, alpha=0.10, uncorrected): roughly 10-35%,
  comparable to and modestly better than the previously-documented
  regression over-rejection.
- Brier-LOCO power on the true signal in the same DGP: 0.95.
- Group-LOCO sanity: signal-group importance exceeds noise-group
  importance in 20/20 reps under a regression DGP with three signal and
  three noise predictors.

#### Tests added

- `test-loco.R` cases: probability-forest auto-loss (brier),
  probability-forest with `zero_one` and `log` losses,
  classification-forest auto-loss (zero_one), classification rejects
  `brier`/`log`, regression rejects `brier`/`zero_one`/`log`,
  probability rejects `abs`/`mse`, split-mode classification and
  probability paths, multi-class probability across all loss choices,
  MSE vs ABS loss differ, survival-forest rejection.
- Group-LOCO cases: OOB and split output shape, singleton groups in
  pred-name order equal per-variable LOCO, character vector becomes
  single group, validation errors (unknown names, overlap, empty group,
  drop-all, duplicated group names), group with classification forest,
  group + split + classification.

### `loco()` overhaul (correctness audit + p-values)

- **New `method = c("z", "wilcox")` argument** for `split = TRUE`. Both
  tests are computed by
  [`conformalInference::loco()`](https://rdrr.io/pkg/conformalInference/man/loco.html)
  already; the Wilcoxon signed-rank variant is more robust to
  heavy-tailed residual distributions than the default normal-theory
  Z-test.
- **New `bonf.correct` argument** (default `TRUE` to match the old
  hidden behavior) exposes Bonferroni correction explicitly.
- **New `data` argument** lets callers pass the training frame directly.
  Use this when the model was fit in a scope where `model$call$data` no
  longer resolves.
- **Output gains a `method` column** (`"z"`, `"wilcox"`, or `"oob"`) so
  downstream code can tell which inference path produced the row.
- **One-sided p-value semantics documented.** The split-mode p-value
  tests `H0: importance <= 0` vs `H1: importance > 0`. Values near 1
  mean “no evidence of importance”, not “evidence of zero importance”.

### `loco()` bug fixes

- **Hyperparameters now recovered from the fitted model object.**
  Previously
  [`loco()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/loco.md)
  parsed `model$call` and tried to evaluate symbols in
  [`parent.frame()`](https://rdrr.io/r/base/sys.parent.html), which
  crashed when the model was fit inside a function or with
  hyperparameters bound to local variables (a very common pattern). Core
  hyperparameters (`num.trees`, `mtry`, `min.node.size`, `splitrule`,
  `replace`, `max.depth`) are now read directly from the model. Uncommon
  arguments still come from the call with a graceful warning fallback if
  they can’t be resolved.
- **OOB mode no longer crashes when the training frame has columns
  beyond the predictors and response.** It now refits via ranger’s `x` /
  `y` interface and ignores extra columns.
- **OOB mode now honors the `seed` argument.** Previously it was
  silently ignored, so OOB importance was non-reproducible across calls.
  The original `.Random.seed` is restored on exit.
- **Regression-only guard.** Classification, probability, and survival
  forests are now rejected with an informative error rather than
  silently returning meaningless residual differences.
- **Single-predictor models are rejected early** with a clear message
  instead of crashing inside ranger.
- **Factor predictors:** allowed in OOB mode (ranger handles them
  natively); rejected in split mode with a clear message because
  [`conformalInference::loco()`](https://rdrr.io/pkg/conformalInference/man/loco.html)
  requires a numeric matrix.
- **x / y interface guard.** Models fit with `ranger(x = X, y = y)` but
  no `dependent.variable.name` previously silently failed deep inside
  `train.data[[""]]`. They now error early with a workaround pointer.

### `loco()` caveats added to documentation

- `cf_loco` already noted the issue;
  [`loco()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/loco.md)
  docs now also note that a naive Wald-style Z-test on per-observation
  OOB squared-error differences is **anti-conservative** (Type-I roughly
  30% at nominal 10% in our checks) because OOB residuals are positively
  dependent across trees. Consequently we do **not** add an inference
  mode for `split = FALSE`; users wanting p-values should set
  `split = TRUE`.
- Documented finite-sample over-rejection of the split-mode Z and
  Wilcoxon tests when applied to random forests: in our checks (n = 200,
  p = 4, 20 reps, x4 noise) the empirical Type-I rate at alpha = 0.10 is
  roughly 50-75 percent, because random forests can exploit a noise
  feature for slightly better generalization in finite samples. The
  tests should be read as exploratory screens rather than confirmatory
  inference.

### Tests

- New `tests/testthat/test-loco.R` covering: data-frame shape and sort
  order, seed reproducibility, extra-column handling, factor predictor
  handling, regression-only guard, single-predictor guard,
  hyperparameter recovery, x/y interface guard, split-mode column
  contract, Wilcoxon method, Bonferroni monotonicity, CI-width
  monotonicity in `alpha`, and `match.arg` / `alpha` input validation.

## UtopiaPlanitia 0.3.1

- [`autocf()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/autocf.md)
  gains a `"bart"` candidate via
  [`dbarts::bart()`](https://rdrr.io/pkg/dbarts/man/bart.html). Default
  pool is now `c("grf", "glmnet", "xgboost", "tabpfn", "bart")`. Adapter
  runs on dbarts defaults with parallel chains
  (`nchain = nthread = dbarts::guessNumCores()`) and a deterministic
  seed; user overrides via the new `bart_args` argument.

## UtopiaPlanitia 0.3.0

- New
  [`plot_pdp()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/plot_pdp.md):
  real Friedman (2001) partial dependence plots for 1-way and 2-way CATE
  surfaces.
- New
  [`plot_scatter()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/plot_scatter.md):
  renamed from the old
  [`plot_pdp()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/plot_pdp.md),
  which plotted individual OOB CATEs vs. a covariate.
- `plot_inter()` is deprecated in favor of
  `plot_pdp(c.forest, x_var, y_var)`.
- [`plot.causal_forest()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/plot.causal_forest.md)
  gains `type = "scatter"` for the new
  [`plot_scatter()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/plot_scatter.md).

## UtopiaPlanitia 0.2.0

- Added S3 [`summary()`](https://rdrr.io/r/base/summary.html) and
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) methods for
  `causal_forest` objects.
- Added S3 [`summary()`](https://rdrr.io/r/base/summary.html),
  [`print()`](https://rdrr.io/r/base/print.html), and
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) methods for
  `cf_loco` output.
- Added
  [`plot.causal_forest()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/plot.causal_forest.md)
  dispatcher for
  [`plot_diag()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/plot_diag.md),
  [`plot_pdp()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/plot_pdp.md),
  [`rank_plot()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/plot_rank.md),
  and `plot_inter()`.
- Fixed deprecated `aes_string()` usage across all plot functions.
- Fixed bare namespace references in
  [`plot_diag()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/plot_diag.md),
  [`plot_rank()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/plot_rank.md),
  `plot_inter()`.
- Replaced `size` with `linewidth` in `geom_smooth()` calls.
- Pruned from 13 to 8 core exported functions; archived extras in
  `R/old/`.
- Added GitHub Actions for R-CMD-check, pkgdown, test coverage, and
  linting.
- Switched license from MIT to GPL-3.

## UtopiaPlanitia 0.1.0

- Initial release with
  [`cf_loco()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/cf_loco.md),
  [`omni_hetero()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/omni_hetero.md),
  and plot functions.
