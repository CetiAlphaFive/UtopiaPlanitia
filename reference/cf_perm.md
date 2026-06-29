# PermuCATE Variable Importance for Causal Forests

Computes conditional-permutation variable importance (PermuCATE;
Paillard et al., 2025) for a fitted `grf` causal forest. Unlike
[`cf_loco`](https://cetialphafive.github.io/UtopiaPlanitia/reference/cf_loco.md),
which drops each covariate and refits, PermuCATE conditionally permutes
each covariate and re-scores the fixed forest, yielding importance
scores together with p-values and confidence intervals.

## Usage

``` r
cf_perm(
  c.forest,
  loss = c("R", "AIPW"),
  n.perm = 50L,
  cross.fit = FALSE,
  num.folds = 5L,
  screen = FALSE,
  normalize = FALSE,
  conf.level = 0.95,
  seed = 1995,
  verbose = TRUE
)
```

## Arguments

- c.forest:

  A fitted causal forest object from the `grf` package.

- loss:

  Character; the CATE risk used to score importance. `"R"` (default) is
  the Robinson residual loss \\((Y-m)-(W-\pi)\tau)^2\\, using the
  forest's `Y.hat` and `W.hat`. `"AIPW"` is the pseudo-outcome risk
  \\(\psi-\tau)^2\\ with \\\psi\\ from
  [`grf::get_scores()`](https://rdrr.io/pkg/grf/man/get_scores.html).

- n.perm:

  Integer number of conditional permutations per covariate. Default
  `50`.

- cross.fit:

  Logical. If `FALSE` (default), the light path scores the passed forest
  in place using its out-of-bag predictions as the risk baseline and
  derives influence-function SEs. This baseline keeps the light-path
  inference conservative (it controls false positives at some cost to
  power for weak effect modifiers). If `TRUE`, refit per fold and
  aggregate with the Nadeau-Bengio correction for unbiased cross-fitted
  inference (R-loss only).

- num.folds:

  Integer number of folds when `cross.fit = TRUE`. Default `5`.

- screen:

  Optional split-frequency pre-screening, identical in meaning to the
  `screen` argument of
  [`cf_loco`](https://cetialphafive.github.io/UtopiaPlanitia/reference/cf_loco.md):
  `FALSE` (default), `TRUE` (drop zero-importance covariates), or an
  integer `k` (keep top-k). Screened-out covariates receive importance 0
  and p-value 1.

- normalize:

  Logical. If `TRUE`, clip negative importances to 0 and rescale to sum
  to 1; SE/z/p/CI are set to `NA` (raw-scale quantities do not survive
  normalization). Default `FALSE`.

- conf.level:

  Confidence level for the reported intervals. Default `0.95`.

- seed:

  Integer seed for reproducibility. Default `1995`.

- verbose:

  Logical; if `TRUE` (default) emit progress/screening messages.

## Value

An object of class `"cf_perm"` with components `vimp` (a data frame with
columns `Variable`, `Importance`, `SE`, `z`, `p.value`, `CI.lower`,
`CI.upper`), `loss`, `cross.fit`, `n.perm`, `num.folds`, `normalized`,
`conf.level`, `n`, and `p`.

## Details

**Light path vs. cross-fitting.** With `cross.fit = FALSE` (default),
importance is scored on the supplied forest using its out-of-bag
predictions as the risk baseline. This keeps the test conservative, but
the standard errors are an influence-function approximation that assumes
an independence the in-sample re-scoring does not strictly provide, so
the light-path p-values should be read as approximate. For calibrated
inference use `cross.fit = TRUE`, which refits the nuisances and CATE
per fold and applies the Nadeau-Bengio corrected \\t\\ test (with
`num.folds - 1` degrees of freedom; use `num.folds = 5` or more for a
less conservative reference).

**Unsupported designs.** Clustered causal forests are not yet supported
(an error is raised); `sample.weights` are ignored and importances are
computed unweighted (a warning is raised).

## Note

AIPW importance magnitudes (`loss = "AIPW"`) are not on a scale
comparable to the R-loss and should be read ordinally (ranking and
significance), because
[`grf::get_scores()`](https://rdrr.io/pkg/grf/man/get_scores.html) bakes
the forest's own CATE estimate into the pseudo-outcome. The default
`loss = "R"` is recommended when magnitudes are to be interpreted. AIPW
importances are also systematically larger than the R-loss and penalize
noise covariates more heavily, so AIPW and R-loss magnitudes are not
directly comparable.

## References

Paillard, J., Reyero Lobo, A. D., Kolodyazhniy, V., Thirion, B., and
Engemann, D.-A. (2025). Measuring Variable Importance in Heterogeneous
Treatment Effects with Confidence. ICML 2025.
[doi:10.48550/arXiv.2408.13002](https://doi.org/10.48550/arXiv.2408.13002)

Chamma, A., Engemann, D.-A., and Thirion, B. (2023). Statistically Valid
Variable Importance Assessment through Conditional Permutations.
[doi:10.48550/arXiv.2309.07593](https://doi.org/10.48550/arXiv.2309.07593)

## See also

[`cf_loco()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/cf_loco.md)
for the LOCO alternative,
[`omni_hetero()`](https://cetialphafive.github.io/UtopiaPlanitia/reference/omni_hetero.md)
for heterogeneity testing.

## Examples

``` r
# \donttest{
library(grf)
set.seed(1995)
n <- 300; p <- 4
X <- matrix(rnorm(n * p), n, p); colnames(X) <- paste0("X", seq_len(p))
W <- rbinom(n, 1, 0.5)
Y <- X[, 1] * W + rnorm(n)
cf <- causal_forest(X, Y, W, num.trees = 300)
cf_perm(cf, n.perm = 20)
#> PermuCATE Variable Importance (Paillard et al., 2025)
#>   n = 300  p = 4  loss = R  cross.fit = FALSE  n.perm = 20 
#>   Normalized: FALSE 
#> 
#>  Variable Importance          SE          z  p.value    CI.lower CI.upper sig
#>        X1   0.112263 0.017558730   6.393580 8.10e-11  0.08338161      Inf ***
#>        X2  -0.008752 0.004770902  -1.834465 9.67e-01 -0.01659949      Inf    
#>        X4  -0.025255 0.002309717 -10.934303 1.00e+00 -0.02905429      Inf    
#>        X3  -0.025663 0.002483305 -10.334395 1.00e+00 -0.02974813      Inf    
# }
```
