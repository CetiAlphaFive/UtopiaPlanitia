# cf_perm() (PermuCATE) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add an exported `cf_perm()` function implementing the PermuCATE conditional-permutation variable-importance algorithm for grf causal forests, with p-values/CIs and S3 print/summary/plot methods.

**Architecture:** Operate on an already-fitted `grf::causal_forest`. For each covariate, conditionally permute it (fit `nu_hat_j = E[X_j|X_-j]` with a grf forest, residualize, shuffle) and re-score the *fixed* forest via the R-loss (default) or AIPW PO-risk. Importance = half the risk increase. Light path (default) gets influence-function SEs from per-observation contributions; opt-in `cross.fit=TRUE` refits per fold and aggregates with Nadeau-Bengio. Returns a classed `cf_perm` object mirroring `cf_loco`.

**Tech Stack:** R, grf (hard dep), testthat ed.3, roxygen2/devtools, ggplot2 (Suggests, for plot).

**Companion spec:** `dev/specs/2026-06-29-cf_perm-permucate-design.md`

**Conventions:** Match `cf_loco` (dotted arg names, `seed = 1995`, classed return + S3). grf and rlang are the only hard Imports; guard ggplot2 with `requireNamespace`. Never hand-edit NAMESPACE — run `devtools::document()`. New export = no back-compat break.

---

## File Structure

| Path | Responsibility |
|------|----------------|
| `R/cf_perm.R` | `cf_perm()` + internals `.cf_perm_risk`, `.cf_perm_cp_sample`, `.cf_perm_cv` |
| `R/cf_perm_methods.R` | `print.cf_perm`, `summary.cf_perm`, `plot.cf_perm` |
| `tests/testthat/helper-cf_perm.R` | shared toy-DGP forest builder |
| `tests/testthat/test-cf_perm.R` | unit + correctness tests |
| `NEWS.md` | dev-version entry (modify) |
| `_pkgdown.yml` | reference entry (modify) |

Run all R commands from the package root. Quick test loop: `devtools::load_all(); devtools::test(filter = "cf_perm")`.

---

## Task 1: Internal risk helper `.cf_perm_risk()`

**Files:**
- Create: `R/cf_perm.R`
- Create: `tests/testthat/helper-cf_perm.R`
- Create: `tests/testthat/test-cf_perm.R`

- [ ] **Step 1: Write the shared test helper**

Create `tests/testthat/helper-cf_perm.R`:

```r
# Shared toy DGP: X1 drives the CATE (signal), X2 a main effect, X3/X4 noise.
make_test_cf <- function(n = 300, p = 4, seed = 1) {
  set.seed(seed)
  X <- matrix(stats::rnorm(n * p), n, p)
  colnames(X) <- paste0("X", seq_len(p))
  W <- stats::rbinom(n, 1, 0.5)
  Y <- X[, 1] * W + X[, 2] + stats::rnorm(n)
  grf::causal_forest(X, Y, W, num.trees = 300, seed = seed)
}
```

- [ ] **Step 2: Write the failing test**

Create `tests/testthat/test-cf_perm.R`:

```r
test_that(".cf_perm_risk computes R-loss and AIPW per-observation risk", {
  tau <- c(0, 1, 2)
  Y   <- c(1, 1, 1)
  m   <- c(0, 0, 0)
  W   <- c(1, 0, 1)
  pi  <- c(0.5, 0.5, 0.5)
  psi <- c(0.5, 0.5, 0.5)

  r_expected <- ((Y - m) - (W - pi) * tau)^2
  expect_equal(.cf_perm_risk(tau, Y, m, W, pi, NULL, "R"), r_expected)

  a_expected <- (psi - tau)^2
  expect_equal(.cf_perm_risk(tau, Y, m, W, pi, psi, "AIPW"), a_expected)
})
```

- [ ] **Step 3: Run test to verify it fails**

Run: `Rscript -e 'devtools::load_all(); devtools::test(filter = "cf_perm")'`
Expected: FAIL — `could not find function ".cf_perm_risk"`.

- [ ] **Step 4: Write minimal implementation**

Create `R/cf_perm.R`:

```r
# Per-observation CATE risk. Only `tau` varies between baseline and permuted;
# the nuisances (m, pi, psi, Y, W) stay fixed at the original X.
#
# loss = "R":    ((Y - m) - (W - pi) * tau)^2   (Robinson residual loss)
# loss = "AIPW": (psi - tau)^2                  (psi from grf::get_scores())
.cf_perm_risk <- function(tau, Y, m, W, pi, psi, loss) {
  if (loss == "R") {
    ((Y - m) - (W - pi) * tau)^2
  } else {
    (psi - tau)^2
  }
}
```

- [ ] **Step 5: Run test to verify it passes**

Run: `Rscript -e 'devtools::load_all(); devtools::test(filter = "cf_perm")'`
Expected: PASS (1 test).

- [ ] **Step 6: Commit**

```bash
git add R/cf_perm.R tests/testthat/helper-cf_perm.R tests/testthat/test-cf_perm.R
git commit -m "feat(cf_perm): add internal per-observation risk helper"
```

---

## Task 2: Conditional-permutation sampler `.cf_perm_cp_sample()`

**Files:**
- Modify: `R/cf_perm.R`
- Modify: `tests/testthat/test-cf_perm.R`

- [ ] **Step 1: Write the failing test**

Append to `tests/testthat/test-cf_perm.R`:

```r
test_that(".cf_perm_cp_sample returns an n x n.perm matrix (continuous)", {
  set.seed(1)
  X <- matrix(stats::rnorm(200 * 3), 200, 3)
  colnames(X) <- paste0("X", 1:3)
  out <- .cf_perm_cp_sample(1, X, X, n.perm = 10, seed = 1)
  expect_equal(dim(out), c(200L, 10L))
  expect_true(all(is.finite(out)))
})

test_that(".cf_perm_cp_sample only emits observed levels (discrete)", {
  set.seed(2)
  X <- cbind(stats::rbinom(200, 1, 0.4),
             stats::rnorm(200), stats::rnorm(200))
  colnames(X) <- paste0("X", 1:3)
  out <- .cf_perm_cp_sample(1, X, X, n.perm = 5, seed = 2)
  expect_true(all(out %in% c(0, 1)))
})
```

- [ ] **Step 2: Run test to verify it fails**

Run: `Rscript -e 'devtools::load_all(); devtools::test(filter = "cf_perm")'`
Expected: FAIL — `could not find function ".cf_perm_cp_sample"`.

- [ ] **Step 3: Write minimal implementation**

Append to `R/cf_perm.R`:

```r
# Conditionally permute covariate `j`. Fit nu_hat_j = E[X_j | X_-j] on `Xfit`,
# apply on `Xapply`. Continuous: shuffle residuals of the apply set around its
# fitted conditional mean. Discrete (integer-coded, <= disc.max levels): draw a
# new level per row from the predicted class probabilities.
# Returns an nrow(Xapply) x n.perm matrix of permuted X_j columns.
.cf_perm_cp_sample <- function(j, Xfit, Xapply, n.perm, disc.max = 10L, seed = 1995) {
  xj.fit <- Xfit[, j]
  Xm.fit <- Xfit[, -j, drop = FALSE]
  xj.app <- Xapply[, j]
  Xm.app <- Xapply[, -j, drop = FALSE]
  na <- nrow(Xapply)

  is.disc <- all(xj.fit == round(xj.fit)) &&
    length(unique(xj.fit)) <= disc.max

  out <- matrix(NA_real_, na, n.perm)

  if (!is.disc) {
    rf <- grf::regression_forest(Xm.fit, xj.fit, seed = seed)
    nu <- stats::predict(rf, Xm.app)$predictions
    e  <- xj.app - nu
    for (k in seq_len(n.perm)) {
      out[, k] <- nu + e[sample.int(na)]
    }
  } else {
    fy    <- factor(xj.fit)
    pf    <- grf::probability_forest(Xm.fit, fy, seed = seed)
    probs <- stats::predict(pf, Xm.app)$predictions
    lev   <- as.numeric(colnames(probs))
    for (k in seq_len(n.perm)) {
      out[, k] <- lev[apply(probs, 1L, function(pr) sample.int(length(lev), 1L, prob = pr))]
    }
  }
  out
}
```

- [ ] **Step 4: Run test to verify it passes**

Run: `Rscript -e 'devtools::load_all(); devtools::test(filter = "cf_perm")'`
Expected: PASS (3 tests).

- [ ] **Step 5: Commit**

```bash
git add R/cf_perm.R tests/testthat/test-cf_perm.R
git commit -m "feat(cf_perm): add conditional-permutation sampler"
```

---

## Task 3: `cf_perm()` light path (R-loss, default)

**Files:**
- Modify: `R/cf_perm.R`
- Modify: `tests/testthat/test-cf_perm.R`

- [ ] **Step 1: Write the failing test**

Append to `tests/testthat/test-cf_perm.R`:

```r
test_that("cf_perm light path returns a cf_perm object with signal detected", {
  cf <- make_test_cf()
  res <- cf_perm(cf, n.perm = 20, seed = 1, verbose = FALSE)

  expect_s3_class(res, "cf_perm")
  expect_named(res$vimp,
    c("Variable", "Importance", "SE", "z", "p.value", "CI.lower", "CI.upper"))
  expect_equal(nrow(res$vimp), 4L)
  expect_false(res$cross.fit)
  expect_identical(res$loss, "R")

  imp <- stats::setNames(res$vimp$Importance, res$vimp$Variable)
  p   <- stats::setNames(res$vimp$p.value,    res$vimp$Variable)
  # X1 drives the CATE: largest importance, significant.
  expect_equal(names(which.max(imp)), "X1")
  expect_lt(p[["X1"]], 0.05)
  # X3 is pure noise: not significant.
  expect_gt(p[["X3"]], 0.05)
})
```

- [ ] **Step 2: Run test to verify it fails**

Run: `Rscript -e 'devtools::load_all(); devtools::test(filter = "cf_perm")'`
Expected: FAIL — `could not find function "cf_perm"`.

- [ ] **Step 3: Write minimal implementation**

Append to `R/cf_perm.R` (roxygen block + function). The light path uses `predict()`
on the fixed forest for BOTH the baseline and the permuted covariates so the two
risks are comparable (do not mix in OOB `c.forest$predictions` here):

```r
#' PermuCATE Variable Importance for Causal Forests
#'
#' Computes conditional-permutation variable importance (PermuCATE; Paillard et
#' al., 2025) for a fitted \code{grf} causal forest. Unlike \code{\link{cf_loco}},
#' which drops each covariate and refits, PermuCATE conditionally permutes each
#' covariate and re-scores the fixed forest, yielding importance scores together
#' with p-values and confidence intervals.
#'
#' @param c.forest A fitted causal forest object from the \code{grf} package.
#' @param loss Character; the CATE risk used to score importance. \code{"R"}
#'   (default) is the Robinson residual loss \eqn{((Y-m)-(W-\pi)\tau)^2}, using
#'   the forest's \code{Y.hat} and \code{W.hat}. \code{"AIPW"} is the
#'   pseudo-outcome risk \eqn{(\psi-\tau)^2} with \eqn{\psi} from
#'   \code{grf::get_scores()}.
#' @param n.perm Integer number of conditional permutations per covariate.
#'   Default \code{50}.
#' @param cross.fit Logical. If \code{FALSE} (default), the light path scores the
#'   passed forest in place and derives influence-function SEs. If \code{TRUE},
#'   refit per fold and aggregate with the Nadeau-Bengio correction (R-loss only).
#' @param num.folds Integer number of folds when \code{cross.fit = TRUE}. Default
#'   \code{5}.
#' @param screen Optional split-frequency pre-screening, identical in meaning to
#'   the \code{screen} argument of \code{\link{cf_loco}}: \code{FALSE} (default),
#'   \code{TRUE} (drop zero-importance covariates), or an integer \code{k} (keep
#'   top-k). Screened-out covariates receive importance 0 and p-value 1.
#' @param normalize Logical. If \code{TRUE}, clip negative importances to 0 and
#'   rescale to sum to 1; SE/z/p/CI are set to \code{NA} (raw-scale quantities do
#'   not survive normalization). Default \code{FALSE}.
#' @param conf.level Confidence level for the reported intervals. Default
#'   \code{0.95}.
#' @param seed Integer seed for reproducibility. Default \code{1995}.
#' @param verbose Logical; if \code{TRUE} (default) emit progress/screening
#'   messages.
#'
#' @return An object of class \code{"cf_perm"} with components \code{vimp} (a data
#'   frame with columns \code{Variable}, \code{Importance}, \code{SE}, \code{z},
#'   \code{p.value}, \code{CI.lower}, \code{CI.upper}), \code{loss},
#'   \code{cross.fit}, \code{n.perm}, \code{num.folds}, \code{normalized},
#'   \code{conf.level}, \code{n}, and \code{p}.
#'
#' @references
#' Paillard, J., Reyero Lobo, A. D., Kolodyazhniy, V., Thirion, B., and
#' Engemann, D.-A. (2025). Measuring Variable Importance in Heterogeneous
#' Treatment Effects with Confidence. ICML 2025. \doi{10.48550/arXiv.2408.13002}
#'
#' Chamma, A., Engemann, D.-A., and Thirion, B. (2023). Statistically Valid
#' Variable Importance Assessment through Conditional Permutations.
#' \doi{10.48550/arXiv.2309.07593}
#'
#' @seealso [cf_loco()] for the LOCO alternative, [omni_hetero()] for
#'   heterogeneity testing.
#'
#' @export
#' @examples
#' \donttest{
#' library(grf)
#' set.seed(1995)
#' n <- 300; p <- 4
#' X <- matrix(rnorm(n * p), n, p); colnames(X) <- paste0("X", seq_len(p))
#' W <- rbinom(n, 1, 0.5)
#' Y <- X[, 1] * W + rnorm(n)
#' cf <- causal_forest(X, Y, W, num.trees = 300)
#' cf_perm(cf, n.perm = 20)
#' }
cf_perm <- function(c.forest, loss = c("R", "AIPW"), n.perm = 50L,
                    cross.fit = FALSE, num.folds = 5L, screen = FALSE,
                    normalize = FALSE, conf.level = 0.95, seed = 1995,
                    verbose = TRUE) {

  loss <- match.arg(loss)
  if (!inherits(c.forest, "causal_forest")) {
    stop("c.forest must be a grf causal forest.")
  }
  if (cross.fit && loss == "AIPW") {
    stop("cross.fit = TRUE is only supported with loss = \"R\" in this version.")
  }
  set.seed(seed)

  X <- c.forest$X.orig
  Y <- c.forest$Y.orig
  W <- c.forest$W.orig
  m  <- c.forest$Y.hat
  pi <- c.forest$W.hat
  n  <- nrow(X)
  p  <- ncol(X)
  vnames <- colnames(X)
  if (is.null(vnames)) vnames <- paste0("V", seq_len(p))

  # --- screening (split-frequency importance) ---
  keep <- rep(TRUE, p)
  if (!isFALSE(screen)) {
    if (!isTRUE(screen) && !(is.numeric(screen) && length(screen) == 1L &&
                             screen == as.integer(screen) && screen >= 1L)) {
      stop("screen must be FALSE, TRUE, or a positive integer.")
    }
    if (is.numeric(screen) && screen >= p) {
      stop("screen must be less than the number of covariates (", p, ").")
    }
    vi.split <- as.numeric(grf::variable_importance(c.forest))
    if (isTRUE(screen)) {
      keep <- vi.split > 0
      if (!any(keep)) keep <- rep(TRUE, p)
    } else {
      ord  <- order(vi.split, decreasing = TRUE)
      keep <- seq_len(p) %in% ord[seq_len(as.integer(screen))]
    }
    if (verbose) {
      message("Screening: PermuCATE on ", sum(keep), " of ", p, " covariates (",
              paste(vnames[keep], collapse = ", "), ")")
    }
  }

  imp <- se <- z <- pval <- ci.lo <- ci.hi <- rep(NA_real_, p)

  if (!cross.fit) {
    psi <- if (loss == "AIPW") as.numeric(grf::get_scores(c.forest)) else NULL
    tau.base <- stats::predict(c.forest, X)$predictions
    L0 <- .cf_perm_risk(tau.base, Y, m, W, pi, psi, loss)
    zc <- stats::qnorm(1 - (1 - conf.level) / 2)

    for (j in which(keep)) {
      Xperm <- .cf_perm_cp_sample(j, X, X, n.perm, seed = seed)
      dbar  <- numeric(n)
      for (k in seq_len(n.perm)) {
        Xk <- X
        Xk[, j] <- Xperm[, k]
        tau.k <- stats::predict(c.forest, Xk)$predictions
        Lk    <- .cf_perm_risk(tau.k, Y, m, W, pi, psi, loss)
        dbar  <- dbar + (Lk - L0) / 2
      }
      dbar    <- dbar / n.perm
      imp[j]  <- mean(dbar)
      se[j]   <- stats::sd(dbar) / sqrt(n)
      z[j]    <- imp[j] / se[j]
      pval[j] <- stats::pnorm(z[j], lower.tail = FALSE)
      ci.lo[j] <- imp[j] - zc * se[j]
      ci.hi[j] <- imp[j] + zc * se[j]
    }
  } else {
    cv <- .cf_perm_cv(c.forest, X, Y, W, keep, n.perm, num.folds, conf.level, seed)
    imp <- cv$imp; se <- cv$se; z <- cv$z
    pval <- cv$pval; ci.lo <- cv$ci.lo; ci.hi <- cv$ci.hi
  }

  # screened-out covariates: zero importance, non-significant
  imp[!keep]  <- 0
  pval[!keep] <- 1

  if (normalize) {
    impc <- ifelse(imp >= 0, imp, 0)
    imp  <- if (sum(impc) == 0) rep(1 / p, p) else impc / sum(impc)
    se[] <- z[] <- pval[] <- ci.lo[] <- ci.hi[] <- NA_real_
  }

  vimp <- data.frame(
    Variable = vnames, Importance = imp, SE = se, z = z,
    p.value = pval, CI.lower = ci.lo, CI.upper = ci.hi,
    stringsAsFactors = FALSE
  )

  structure(
    list(vimp = vimp, loss = loss, cross.fit = cross.fit, n.perm = n.perm,
         num.folds = if (cross.fit) num.folds else NA_integer_,
         normalized = normalize, conf.level = conf.level, n = n, p = p),
    class = "cf_perm"
  )
}
```

- [ ] **Step 4: Run test to verify it passes**

Run: `Rscript -e 'devtools::load_all(); devtools::test(filter = "cf_perm")'`
Expected: PASS. (The `.cf_perm_cv` reference is only reached when `cross.fit=TRUE`, added in Task 6; the light-path test does not hit it.)

- [ ] **Step 5: Commit**

```bash
git add R/cf_perm.R tests/testthat/test-cf_perm.R
git commit -m "feat(cf_perm): add light-path R-loss importance with influence-function SEs"
```

---

## Task 4: AIPW loss path

**Files:**
- Modify: `tests/testthat/test-cf_perm.R`

(No new source needed — Task 3 already wires `loss = "AIPW"` through `get_scores()`. This task adds coverage and confirms behavior.)

- [ ] **Step 1: Write the failing test**

Append to `tests/testthat/test-cf_perm.R`:

```r
test_that("cf_perm AIPW loss runs and agrees in sign on the signal variable", {
  cf  <- make_test_cf()
  rR  <- cf_perm(cf, loss = "R",    n.perm = 20, seed = 1, verbose = FALSE)
  rA  <- cf_perm(cf, loss = "AIPW", n.perm = 20, seed = 1, verbose = FALSE)

  expect_identical(rA$loss, "AIPW")
  x1_R <- rR$vimp$Importance[rR$vimp$Variable == "X1"]
  x1_A <- rA$vimp$Importance[rA$vimp$Variable == "X1"]
  expect_gt(x1_R, 0)
  expect_gt(x1_A, 0)
})
```

- [ ] **Step 2: Run test to verify it passes** (it should already pass — confirms the AIPW wiring)

Run: `Rscript -e 'devtools::load_all(); devtools::test(filter = "cf_perm")'`
Expected: PASS. If `x1_A` is not > 0, treat as the "AIPW baseline self-favoring" open check from the spec — record the empirical comparison in the PR description; do not silently relax the test.

- [ ] **Step 3: Commit**

```bash
git add tests/testthat/test-cf_perm.R
git commit -m "test(cf_perm): cover AIPW loss path"
```

---

## Task 5: Screening, normalize, and input validation

**Files:**
- Modify: `tests/testthat/test-cf_perm.R`

(No new source — Task 3 implements screening, normalize, and the input check. This task locks the behavior with tests.)

- [ ] **Step 1: Write the failing test**

Append to `tests/testthat/test-cf_perm.R`:

```r
test_that("cf_perm rejects non-causal_forest input", {
  expect_error(cf_perm(list(), verbose = FALSE),
               "must be a grf causal forest")
})

test_that("cf_perm screen = integer keeps only top-k; rest get 0 importance, p=1", {
  cf  <- make_test_cf()
  res <- cf_perm(cf, n.perm = 10, screen = 2L, seed = 1, verbose = FALSE)
  scored   <- res$vimp$Importance != 0 | res$vimp$p.value < 1
  expect_lte(sum(scored), 2L)
  dropped <- res$vimp[res$vimp$Importance == 0 & res$vimp$p.value == 1, ]
  expect_true(nrow(dropped) >= 2L)
})

test_that("cf_perm normalize rescales importance to sum 1 and NAs the inference cols", {
  cf  <- make_test_cf()
  res <- cf_perm(cf, n.perm = 10, normalize = TRUE, seed = 1, verbose = FALSE)
  expect_true(res$normalized)
  expect_equal(sum(res$vimp$Importance), 1, tolerance = 1e-8)
  expect_true(all(is.na(res$vimp$SE)))
  expect_true(all(is.na(res$vimp$p.value)))
})
```

- [ ] **Step 2: Run test to verify it passes**

Run: `Rscript -e 'devtools::load_all(); devtools::test(filter = "cf_perm")'`
Expected: PASS.

- [ ] **Step 3: Commit**

```bash
git add tests/testthat/test-cf_perm.R
git commit -m "test(cf_perm): cover screening, normalize, input validation"
```

---

## Task 6: Cross-fit path (`cross.fit = TRUE`, R-loss) with Nadeau-Bengio

**Files:**
- Modify: `R/cf_perm.R`
- Modify: `tests/testthat/test-cf_perm.R`

- [ ] **Step 1: Write the failing test**

Append to `tests/testthat/test-cf_perm.R`:

```r
test_that("cf_perm cross-fit path returns fold-based inference", {
  cf  <- make_test_cf(n = 400)
  res <- cf_perm(cf, n.perm = 10, cross.fit = TRUE, num.folds = 3,
                 seed = 1, verbose = FALSE)
  expect_true(res$cross.fit)
  expect_equal(res$num.folds, 3)
  imp <- stats::setNames(res$vimp$Importance, res$vimp$Variable)
  expect_equal(names(which.max(imp)), "X1")
  expect_false(any(is.na(res$vimp$p.value)))
})

test_that("cf_perm cross-fit rejects AIPW in this version", {
  cf <- make_test_cf()
  expect_error(cf_perm(cf, cross.fit = TRUE, loss = "AIPW", verbose = FALSE),
               "only supported with loss")
})
```

- [ ] **Step 2: Run test to verify it fails**

Run: `Rscript -e 'devtools::load_all(); devtools::test(filter = "cf_perm")'`
Expected: FAIL — `could not find function ".cf_perm_cv"` (first test). The AIPW-guard test already passes (Task 3 added the guard).

- [ ] **Step 3: Write minimal implementation**

Append to `R/cf_perm.R`. The cross-fit path refits a causal forest per fold (reusing
the passed forest's hyperparameters), estimates `m` and `pi` on the held-out rows with
their own regression forests, scores conditional-permutation importance per test fold,
then aggregates across folds with the Nadeau-Bengio corrected variance:

```r
# Stratified fold ids on the binary treatment W.
.cf_perm_folds <- function(W, num.folds) {
  fold <- integer(length(W))
  for (lev in unique(W)) {
    idx <- which(W == lev)
    fold[idx] <- sample(rep_len(seq_len(num.folds), length(idx)))
  }
  fold
}

# Cross-fit PermuCATE (R-loss). Returns named list of per-covariate vectors.
.cf_perm_cv <- function(c.forest, X, Y, W, keep, n.perm, num.folds,
                        conf.level, seed) {
  n <- nrow(X)
  p <- ncol(X)
  zc <- stats::qnorm(1 - (1 - conf.level) / 2)
  fold <- .cf_perm_folds(W, num.folds)

  Psi <- matrix(NA_real_, num.folds, p)   # per-fold, per-covariate importance
  n1  <- numeric(num.folds)               # train sizes
  n2  <- numeric(num.folds)               # test sizes

  for (f in seq_len(num.folds)) {
    tr <- which(fold != f)
    te <- which(fold == f)
    n1[f] <- length(tr)
    n2[f] <- length(te)

    cf.k <- grf::causal_forest(
      X[tr, , drop = FALSE], Y[tr], W[tr],
      num.trees             = c.forest$`_num_trees`,
      sample.fraction       = c.forest$tunable.params$sample.fraction,
      mtry                  = c.forest$tunable.params$mtry,
      min.node.size         = c.forest$tunable.params$min.node.size,
      honesty.fraction      = c.forest$tunable.params$honesty.fraction,
      honesty.prune.leaves  = c.forest$tunable.params$honesty.prune.leaves,
      alpha                 = c.forest$tunable.params$alpha,
      imbalance.penalty     = c.forest$tunable.params$imbalance.penalty,
      ci.group.size         = c.forest$ci.group.size,
      seed                  = seed
    )
    m.te  <- stats::predict(grf::regression_forest(X[tr, , drop = FALSE], Y[tr], seed = seed),
                            X[te, , drop = FALSE])$predictions
    pi.te <- stats::predict(grf::regression_forest(X[tr, , drop = FALSE], W[tr], seed = seed),
                            X[te, , drop = FALSE])$predictions
    tau.te <- stats::predict(cf.k, X[te, , drop = FALSE])$predictions
    L0.te  <- .cf_perm_risk(tau.te, Y[te], m.te, W[te], pi.te, NULL, "R")

    Xte <- X[te, , drop = FALSE]
    Xtr <- X[tr, , drop = FALSE]
    for (j in which(keep)) {
      Xperm <- .cf_perm_cp_sample(j, Xtr, Xte, n.perm, seed = seed)
      dbar  <- numeric(length(te))
      for (k in seq_len(n.perm)) {
        Xk <- Xte
        Xk[, j] <- Xperm[, k]
        tau.k <- stats::predict(cf.k, Xk)$predictions
        Lk    <- .cf_perm_risk(tau.k, Y[te], m.te, W[te], pi.te, NULL, "R")
        dbar  <- dbar + (Lk - L0.te) / 2
      }
      Psi[f, j] <- mean(dbar / n.perm)
    }
  }

  imp <- colMeans(Psi)
  # Nadeau-Bengio corrected variance of the cross-validated mean.
  rho <- mean(n2) / mean(n1)
  s2  <- apply(Psi, 2L, stats::var)
  se  <- sqrt((1 / num.folds + rho) * s2)
  z   <- imp / se
  pval  <- stats::pnorm(z, lower.tail = FALSE)
  ci.lo <- imp - zc * se
  ci.hi <- imp + zc * se

  list(imp = imp, se = se, z = z, pval = pval, ci.lo = ci.lo, ci.hi = ci.hi)
}
```

- [ ] **Step 4: Run test to verify it passes**

Run: `Rscript -e 'devtools::load_all(); devtools::test(filter = "cf_perm")'`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add R/cf_perm.R tests/testthat/test-cf_perm.R
git commit -m "feat(cf_perm): add cross-fit path with Nadeau-Bengio inference"
```

---

## Task 7: S3 methods — print, summary, plot

**Files:**
- Create: `R/cf_perm_methods.R`
- Modify: `tests/testthat/test-cf_perm.R`

- [ ] **Step 1: Write the failing test**

Append to `tests/testthat/test-cf_perm.R`:

```r
test_that("cf_perm S3 methods behave", {
  cf  <- make_test_cf()
  res <- cf_perm(cf, n.perm = 10, seed = 1, verbose = FALSE)

  expect_output(print(res), "PermuCATE Variable Importance")
  expect_invisible(print(res))
  expect_identical(summary(res), invisible(res))

  skip_if_not_installed("ggplot2")
  g <- plot(res)
  expect_s3_class(g, "ggplot")
})
```

- [ ] **Step 2: Run test to verify it fails**

Run: `Rscript -e 'devtools::load_all(); devtools::test(filter = "cf_perm")'`
Expected: FAIL — no `print.cf_perm` method; default print does not emit the header.

- [ ] **Step 3: Write minimal implementation**

Create `R/cf_perm_methods.R`:

```r
#' Print PermuCATE Variable Importance
#'
#' @param x An object of class `"cf_perm"` returned by [cf_perm()].
#' @param ... Additional arguments (currently unused).
#' @return The `cf_perm` object (invisibly).
#' @seealso [cf_perm()], [plot.cf_perm()]
#' @method print cf_perm
#' @export
print.cf_perm <- function(x, ...) {
  cat("PermuCATE Variable Importance (Paillard et al., 2025)\n")
  cat("  n =", x$n, " p =", x$p, " loss =", x$loss,
      " cross.fit =", x$cross.fit, " n.perm =", x$n.perm, "\n")
  cat("  Normalized:", x$normalized, "\n\n")

  v <- x$vimp[order(-x$vimp$Importance), ]
  rownames(v) <- NULL
  if (!all(is.na(v$p.value))) {
    stars <- as.character(cut(v$p.value,
      breaks = c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf),
      labels = c("***", "**", "*", ".", "")))
    stars[is.na(stars)] <- ""
    v$sig <- stars
    v$p.value <- signif(v$p.value, 3)
  }
  v$Importance <- round(v$Importance, 6)
  print(v, row.names = FALSE)
  invisible(x)
}

#' Summarize PermuCATE Variable Importance
#'
#' @param object An object of class `"cf_perm"` returned by [cf_perm()].
#' @param ... Additional arguments passed to [print.cf_perm()].
#' @return The `cf_perm` object (invisibly).
#' @seealso [cf_perm()]
#' @method summary cf_perm
#' @export
summary.cf_perm <- function(object, ...) {
  print(object, ...)
}

#' Plot PermuCATE Variable Importance
#'
#' Draws a horizontal lollipop chart of PermuCATE importance scores with
#' confidence intervals (when available), colored by significance at the
#' object's `conf.level`.
#'
#' @param x An object of class `"cf_perm"` returned by [cf_perm()].
#' @param ... Additional arguments (currently unused).
#' @return A `ggplot` object.
#' @importFrom rlang .data
#' @method plot cf_perm
#' @export
plot.cf_perm <- function(x, ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required but not installed.")
  }
  v <- x$vimp
  v$Variable <- factor(v$Variable, levels = v$Variable[order(v$Importance)])
  v$Significant <- !is.na(v$p.value) & v$p.value < (1 - x$conf.level)

  g <- ggplot2::ggplot(v, ggplot2::aes(x = .data[["Variable"]],
                                       y = .data[["Importance"]])) +
    ggplot2::geom_segment(ggplot2::aes(xend = .data[["Variable"]], y = 0,
                                       yend = .data[["Importance"]]),
                          linewidth = 0.8, color = "grey50") +
    ggplot2::geom_point(ggplot2::aes(color = .data[["Significant"]]), size = 3) +
    ggplot2::coord_flip() +
    ggplot2::labs(x = NULL, y = "PermuCATE importance",
                  title = "PermuCATE Variable Importance") +
    ggplot2::theme(
      text = ggplot2::element_text(size = 12, family = "serif"),
      panel.background = ggplot2::element_rect(fill = "#e6e6e6"),
      plot.title = ggplot2::element_text(hjust = 0.5),
      panel.border = ggplot2::element_blank(),
      complete = TRUE
    )

  if (!x$normalized && all(is.finite(c(v$CI.lower, v$CI.upper)))) {
    g <- g + ggplot2::geom_errorbar(
      ggplot2::aes(ymin = .data[["CI.lower"]], ymax = .data[["CI.upper"]]),
      width = 0.2, color = "grey40")
  }
  g
}
```

- [ ] **Step 4: Run test to verify it passes**

Run: `Rscript -e 'devtools::load_all(); devtools::test(filter = "cf_perm")'`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add R/cf_perm_methods.R tests/testthat/test-cf_perm.R
git commit -m "feat(cf_perm): add print/summary/plot S3 methods"
```

---

## Task 8: Documentation, NEWS, pkgdown, and full check

**Files:**
- Modify: `NEWS.md`
- Modify: `_pkgdown.yml`
- Regenerated: `man/cf_perm.Rd`, `man/print.cf_perm.Rd`, `man/summary.cf_perm.Rd`, `man/plot.cf_perm.Rd`, `NAMESPACE`

- [ ] **Step 1: Regenerate docs + NAMESPACE**

Run: `Rscript -e 'devtools::document()'`
Expected: writes `man/cf_perm.Rd` and the three method `.Rd` files; adds
`export(cf_perm)`, `S3method(print, cf_perm)`, `S3method(summary, cf_perm)`,
`S3method(plot, cf_perm)` to `NAMESPACE`. Confirm:

Run: `grep -n "cf_perm" NAMESPACE`
Expected: four lines (one export + three S3method).

- [ ] **Step 2: Add a NEWS entry**

Insert at the top of `NEWS.md`, immediately under the `# UtopiaPlanitia (development version)` heading:

```markdown
## New function `cf_perm()` — PermuCATE variable importance

* **`cf_perm()` implements PermuCATE** (Paillard et al., 2025): conditional-permutation
  variable importance for a fitted `grf` causal forest, returning importance scores with
  p-values and confidence intervals. Complements `cf_loco()`: it permutes rather than
  refits, has lower variance, and reports inference. The default `loss = "R"` (Robinson
  residual) uses the forest's `Y.hat`/`W.hat`; `loss = "AIPW"` uses `grf::get_scores()`.
  `cross.fit = TRUE` opts into K-fold refitting with Nadeau-Bengio inference (R-loss only).
  Ships with `print`/`summary`/`plot` methods.
```

- [ ] **Step 3: Register the function on the pkgdown reference index**

In `_pkgdown.yml`, add `cf_perm` to the reference section that already lists `cf_loco`
(diagnostics/tests group). Confirm the file still parses:

Run: `Rscript -e 'yaml::read_yaml("_pkgdown.yml"); cat("ok\n")'`
Expected: `ok`.

- [ ] **Step 4: Run the full cf_perm test file**

Run: `Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-cf_perm.R")'`
Expected: all tests PASS, 0 failures.

- [ ] **Step 5: Run R CMD check**

Run: `Rscript -e 'devtools::check(args = "--no-manual", error_on = "warning")'`
Expected: 0 errors, 0 warnings. Resolve any new NOTEs about undocumented args or
unstated dependencies before proceeding.

- [ ] **Step 6: Commit**

```bash
git add R/cf_perm.R R/cf_perm_methods.R man/ NAMESPACE NEWS.md _pkgdown.yml \
  tests/testthat/helper-cf_perm.R tests/testthat/test-cf_perm.R
git commit -m "docs(cf_perm): document, add NEWS + pkgdown entry, regenerate man/"
```

---

## Optional Task 9: Slow correctness test (Type-I behavior)

**Files:**
- Modify: `tests/testthat/test-cf_perm.R`

- [ ] **Step 1: Add a gated null-DGP test**

Append to `tests/testthat/test-cf_perm.R`:

```r
test_that("cf_perm controls false positives under a null DGP (slow)", {
  skip_on_cran()
  skip_if(Sys.getenv("UTOPIA_RUN_SLOW_TESTS") != "1",
          "set UTOPIA_RUN_SLOW_TESTS=1 to run")
  set.seed(7)
  reject <- replicate(50, {
    n <- 400; p <- 4
    X <- matrix(stats::rnorm(n * p), n, p); colnames(X) <- paste0("X", 1:p)
    W <- stats::rbinom(n, 1, 0.5)
    Y <- X[, 2] + stats::rnorm(n)              # no treatment-effect heterogeneity
    cf <- grf::causal_forest(X, Y, W, num.trees = 300)
    res <- cf_perm(cf, n.perm = 20, seed = 1, verbose = FALSE)
    any(res$vimp$p.value < 0.05, na.rm = TRUE)
  })
  expect_lt(mean(reject), 0.30)   # generous bound for the approximate light path
})
```

- [ ] **Step 2: Run locally to verify**

Run: `UTOPIA_RUN_SLOW_TESTS=1 Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-cf_perm.R")'`
Expected: PASS (rejection rate below the bound). If it exceeds the bound, that is the
light-path inference being anti-conservative — document it and prefer `cross.fit = TRUE`
for inference-critical use; do not loosen the bound without justification.

- [ ] **Step 3: Commit**

```bash
git add tests/testthat/test-cf_perm.R
git commit -m "test(cf_perm): add gated null-DGP false-positive check"
```

---

## Notes for the implementer

- **Prediction mode (amended):** the light path scores the BASELINE risk with the forest's out-of-bag predictions (`c.forest$predictions`) for conservative null calibration, and the PERMUTED covariates with in-sample `predict(c.forest, X_perm)` (no OOB available for new data). cross.fit = TRUE gives unbiased cross-fitted inference.
- **AIPW caveat (spec open-check #1):** `grf::get_scores()` bakes the forest's own `tau.hat`
  into `psi`, making the AIPW baseline slightly self-favoring. Compare AIPW vs R-loss on the toy
  DGP and note the result in the PR; R-loss is the recommended default.
- **`disc.max = 10`** is the integer-cardinality cutoff for the discrete CP sampler. It is an
  internal constant in `.cf_perm_cp_sample`; promote it to an argument only if a real use case needs it.
- **Cross-fit AIPW** is intentionally unsupported in v1 (guarded with a clear error). Implementing
  it requires assembling DR scores on held-out rows from train-fold nuisances — future work.
- **API-stability test:** once the signature settles, consider adding a `T-API` formals guard for
  `cf_perm` in `tests/testthat/test-fixes-20260511.R`, matching the existing exported-function pins.
