# cf_loco() screen + correlation diagnostic — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Simplify `cf_loco()` screening to drop only zero-split-frequency covariates, and add a `verbose` correlation diagnostic for conditioning variables.

**Architecture:** Single-function change in `R/cf_loco.R` (helper `compute_vimp()` untouched). Two independent edits — screen-rule rewrite and a post-refit correlation block — plus the trailing `verbose` formal, roxygen, NEWS, and the API-stability guard.

**Tech Stack:** R, grf, testthat (edition 3), roxygen2/devtools.

> **Commit policy:** Commit steps are written per TDD convention, but Jack commits manually — do NOT push or commit unless Jack confirms. Stage and show diffs instead if unsure.

> **Spec:** `dev/specs/2026-06-23-cf_loco-screen-and-correlation-design.md`

---

## File Structure

- Modify: `R/cf_loco.R` — signature (`verbose`), screen block rewrite, correlation diagnostic, roxygen.
- Modify: `tests/testthat/test-fixes-20260511.R:123-129` — add `"verbose"` to formals guard.
- Create: `tests/testthat/test-cf_loco-screen-corr.R` — screen + correlation tests.
- Modify: `NEWS.md` — development-version entry.
- Regenerate: `man/cf_loco.Rd` via `devtools::document()`.

---

### Task 1: Add `verbose` formal + update API guard

**Files:**
- Modify: `R/cf_loco.R:102` (signature)
- Modify: `tests/testthat/test-fixes-20260511.R:123-129`

- [ ] **Step 1: Update the failing API guard test**

In `tests/testthat/test-fixes-20260511.R`, replace the expected formals set:

```r
test_that("T-API: cf_loco formals unchanged", {
  fn <- names(formals(cf_loco))
  expect_setequal(fn,
                  c("c.forest", "variable.groups", "group.by.corr",
                    "corr.threshold", "normalize", "screen",
                    "stabilize", "seed", "verbose"))
})
```

- [ ] **Step 2: Run to verify it fails**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-fixes-20260511.R", filter=NULL)'`
Expected: FAIL — `verbose` not in formals.

- [ ] **Step 3: Add the formal**

In `R/cf_loco.R`, change the signature (line 102) to:

```r
cf_loco <- function(c.forest, variable.groups = NULL, group.by.corr = FALSE, corr.threshold = 0.5, normalize = FALSE, screen = FALSE, stabilize = 1e-6, seed = 1995, verbose = TRUE) {
```

- [ ] **Step 4: Run to verify it passes**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-fixes-20260511.R")'`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add R/cf_loco.R tests/testthat/test-fixes-20260511.R
git commit -m "feat(cf_loco): add verbose argument (signature + API guard)"
```

---

### Task 2: Screen rule — drop only zero split-frequency covariates

**Files:**
- Modify: `R/cf_loco.R:182-233` (screen block)
- Create: `tests/testthat/test-cf_loco-screen-corr.R`

- [ ] **Step 1: Write the failing test**

Create `tests/testthat/test-cf_loco-screen-corr.R`:

```r
test_that("screen=TRUE drops only zero split-frequency covariates", {
  skip_if_not_installed("grf")
  set.seed(1995)
  n <- 300
  X <- matrix(rnorm(n * 4), n, 4)
  colnames(X) <- paste0("X", 1:4)
  X[, 4] <- 1                       # constant -> zero split frequency
  W <- rbinom(n, 1, 0.5)
  Y <- 2 * X[, 1] * W + rnorm(n)
  cf <- grf::causal_forest(X, Y, W, num.trees = 200)

  vi <- suppressMessages(cf_loco(cf, screen = TRUE, verbose = FALSE))
  expect_equal(nrow(vi$vimp), 4L)                                  # all vars present
  expect_equal(vi$vimp$Importance[vi$vimp$Variable == "X4"], 0)    # screened -> 0
})

test_that("screen=TRUE keeps all when every importance is zero", {
  skip_if_not_installed("grf")
  set.seed(7)
  n <- 200
  X <- matrix(1, n, 3)              # all constant -> all zero importance
  X <- X + matrix(rnorm(n * 3, sd = 1e-9), n, 3)
  colnames(X) <- paste0("X", 1:3)
  W <- rbinom(n, 1, 0.5)
  Y <- rnorm(n)
  cf <- grf::causal_forest(X, Y, W, num.trees = 100)
  expect_message(cf_loco(cf, screen = TRUE, verbose = FALSE), "keeping all")
})
```

- [ ] **Step 2: Run to verify it fails**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-cf_loco-screen-corr.R")'`
Expected: FAIL — old mean-rule may keep/drop differently; "keeping all" message absent.

- [ ] **Step 3: Rewrite the screen block**

In `R/cf_loco.R`, replace the screening computation + decision block (current lines 182-233, i.e. from `if (!isFALSE(screen) || interactive()) {` through the end of the interactive `else if` branch) with:

```r
  if (!isFALSE(screen)) {
    # compute split-frequency importance (essentially free)
    vi.split <- as.numeric(grf::variable_importance(c.forest))
    # aggregate to group level: max importance among member variables
    vi.group <- vapply(all.index.groups, function(idx) max(vi.split[idx]), numeric(1))
  }

  if (isTRUE(screen)) {
    # auto-screen: drop covariates the forest never split on (importance exactly 0)
    keep <- vi.group > 0
    if (sum(keep) == 0L) {
      keep <- rep(TRUE, n.groups)  # safety: all zero -> keep all
      message("Screening: all variables have zero split-frequency importance; ",
              "keeping all ", n.groups, ".")
    } else {
      message("Screening: LOCO on ", sum(keep), " of ", n.groups, " variables (",
              paste(all.variable.names[keep], collapse = ", "), ")")
    }
    screened <- !keep
    index.groups <- all.index.groups[keep]
    variable.names <- all.variable.names[keep]

  } else if (is.numeric(screen)) {
    # explicit top-k
    k <- as.integer(screen)
    ord <- order(vi.group, decreasing = TRUE)
    keep.idx <- ord[seq_len(k)]
    keep <- seq_len(n.groups) %in% keep.idx
    screened <- !keep
    message("Screening: LOCO on top ", k, " of ", n.groups, " variables (",
            paste(all.variable.names[keep], collapse = ", "), ")")
    index.groups <- all.index.groups[keep]
    variable.names <- all.variable.names[keep]
  }
```

This removes the entire `else if (isFALSE(screen) && interactive())` cost-prompt + `readline()` branch. The `screen` validation block above it (current lines 172-180) stays unchanged.

- [ ] **Step 4: Run to verify it passes**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-cf_loco-screen-corr.R")'`
Expected: both new screen tests PASS.

> If grf returns a tiny non-zero importance for the constant column, the first
> test reveals it — adjust the DGP (e.g. drop `num.trees` or confirm grf's
> behavior) rather than loosening the `> 0` rule.

- [ ] **Step 5: Commit**

```bash
git add R/cf_loco.R tests/testthat/test-cf_loco-screen-corr.R
git commit -m "feat(cf_loco): screen=TRUE drops zero split-freq covs; remove interactive prompt"
```

---

### Task 3: Correlation diagnostic

**Files:**
- Modify: `R/cf_loco.R` (insert block just before `result <- data.frame(...)`, current line 303)
- Modify: `tests/testthat/test-cf_loco-screen-corr.R`

- [ ] **Step 1: Write the failing tests**

Append to `tests/testthat/test-cf_loco-screen-corr.R`:

```r
.corr_cf <- function() {
  set.seed(1995)
  n <- 300
  x1 <- rnorm(n)
  x2 <- x1 + rnorm(n, sd = 0.1)        # r ~ 0.99 with x1
  x3 <- rnorm(n)                       # independent
  X <- cbind(X1 = x1, X2 = x2, X3 = x3)
  W <- rbinom(n, 1, 0.5)
  Y <- x1 * W + rnorm(n)
  grf::causal_forest(X, Y, W, num.trees = 200)
}

test_that("verbose=TRUE warns on correlated conditioning variables", {
  skip_if_not_installed("grf")
  cf <- .corr_cf()
  expect_warning(suppressMessages(cf_loco(cf, verbose = TRUE)), "correlated")
})

test_that("verbose=FALSE suppresses the correlation warning", {
  skip_if_not_installed("grf")
  cf <- .corr_cf()
  expect_no_warning(cf_loco(cf, verbose = FALSE))
})

test_that("verbose=TRUE prints the correlation matrix", {
  skip_if_not_installed("grf")
  cf <- .corr_cf()
  expect_output(suppressWarnings(suppressMessages(cf_loco(cf, verbose = TRUE))), "X1")
})

test_that("grouping suppresses the correlation warning but keeps the matrix", {
  skip_if_not_installed("grf")
  cf <- .corr_cf()
  expect_no_warning(suppressMessages(cf_loco(cf, group.by.corr = TRUE, verbose = TRUE)))
})
```

- [ ] **Step 2: Run to verify it fails**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-cf_loco-screen-corr.R")'`
Expected: the four correlation tests FAIL (no warning/print yet).

- [ ] **Step 3: Insert the diagnostic block**

In `R/cf_loco.R`, immediately before `result <- data.frame(Variable = variable.names, Importance = In_out)` (current line 303), insert:

```r
  # --- correlation diagnostic (conditioning variables) ---
  if (verbose && p > 1L) {
    cov.names <- colnames(X)
    if (is.null(cov.names)) cov.names <- paste0("V", seq_len(p))
    cm <- suppressWarnings(stats::cor(X))
    dimnames(cm) <- list(cov.names, cov.names)
    message("Conditioning-variable correlation matrix:")
    print(round(cm, 2))

    grouping.active <- isTRUE(group.by.corr) || !is.null(variable.groups)
    if (!grouping.active) {
      cm.off <- cm
      diag(cm.off) <- NA_real_
      hit <- upper.tri(cm.off) & !is.na(cm.off) & abs(cm.off) > 0.5
      if (any(hit)) {
        idx <- which(hit, arr.ind = TRUE)
        pair.str <- apply(idx, 1L, function(rc) {
          sprintf("%s & %s (r = %.2f)",
                  cov.names[rc[1L]], cov.names[rc[2L]], cm[rc[1L], rc[2L]])
        })
        warning("cf_loco: the following conditioning variables are correlated ",
                "above |r| = 0.5: ", paste(pair.str, collapse = "; "),
                ". LOCO importance is divided among correlated covariates and ",
                "may understate their individual relevance. Consider grouping ",
                "them via `group.by.corr = TRUE` or interpreting their ",
                "importances jointly.", call. = FALSE)
      }
    }
  }

```

Note: when `group.by.corr = TRUE`, `variable.groups` was already populated earlier in the function, so `grouping.active` is `TRUE` either way.

- [ ] **Step 4: Run to verify it passes**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-cf_loco-screen-corr.R")'`
Expected: all screen + correlation tests PASS.

- [ ] **Step 5: Commit**

```bash
git add R/cf_loco.R tests/testthat/test-cf_loco-screen-corr.R
git commit -m "feat(cf_loco): add verbose correlation diagnostic for conditioning variables"
```

---

### Task 4: Docs — roxygen, NEWS, regenerate man

**Files:**
- Modify: `R/cf_loco.R` (roxygen header)
- Modify: `NEWS.md`
- Regenerate: `man/cf_loco.Rd`

- [ ] **Step 1: Update roxygen**

In `R/cf_loco.R`, rewrite the `@param screen` `\item` for `TRUE` (currently "Keep variables with split-frequency importance above the mean.") to:

```r
#'     \item{`TRUE`}{Auto-screen. Keep only variables with non-zero
#'       split-frequency importance (drop covariates the forest never split on).}
```

Remove the interactive-prompt sentence from the `FALSE` `\item` so it reads:

```r
#'     \item{`FALSE` (default)}{No screening; LOCO is run on every variable.}
```

Add a new `@param verbose` block (place after `@param seed`):

```r
#' @param verbose Logical. If `TRUE` (default), after the refits print the
#'   conditioning-variable correlation matrix and warn when any pair of
#'   covariates is correlated above `|r| = 0.5` (suppressed when variables are
#'   grouped via `group.by.corr` or `variable.groups`). Default `TRUE`.
```

- [ ] **Step 2: Regenerate man + run full suite**

Run: `Rscript -e 'devtools::document(); devtools::test()'`
Expected: `man/cf_loco.Rd` updated; full suite green.

- [ ] **Step 3: Add NEWS entry**

In `NEWS.md`, under `# UtopiaPlanitia (development version)`, add:

```markdown
## `cf_loco()` screening + correlation diagnostic

* **`screen = TRUE` now drops only zero-importance covariates.** Auto-screening
  keeps every variable with non-zero split-frequency importance (previously: above
  the mean, which over-selected). The interactive runtime prompt was removed.
* **New `verbose` argument (default `TRUE`)** prints the conditioning-variable
  correlation matrix after the refits and warns when any covariate pair is
  correlated above `|r| = 0.5`. Set `verbose = FALSE` to silence. The warning is
  suppressed when variables are grouped via `group.by.corr` or `variable.groups`.
  Note: with the default `verbose = TRUE`, existing calls now emit this matrix and
  (when applicable) the new warning.
```

- [ ] **Step 4: Commit**

```bash
git add R/cf_loco.R man/cf_loco.Rd NEWS.md
git commit -m "docs(cf_loco): document verbose + new screen rule; NEWS entry"
```

---

### Task 5: Full check

- [ ] **Step 1: Run R CMD check**

Run: `Rscript -e 'devtools::check()'`
Expected: 0 errors, 0 warnings. (Notes about Suggests are acceptable.)

- [ ] **Step 2: Confirm API guard + new tests pass under check**

Verify `test-fixes-20260511.R` (formals now include `verbose`) and
`test-cf_loco-screen-corr.R` both pass within the check run.

---

## Self-Review

- **Spec coverage:** screen rule rewrite (T2), remove interactive prompt (T2),
  variable_importance only when `!isFALSE(screen)` (T2), `verbose` arg (T1),
  correlation matrix print + `>0.5` warning (T3), grouping suppression (T3),
  `p==1` guard (T3 code, `p > 1L`), API guard (T1), roxygen + NEWS (T4),
  full check (T5). All covered.
- **`p == 1` note:** no runtime test — `cf_loco` cannot refit with a single
  covariate (dropping it leaves 0 columns), so the `p > 1L` guard is belt-and-
  suspenders; not separately exercised.
- **Placeholder scan:** none.
- **Type/name consistency:** `keep`, `screened`, `vi.group`, `cov.names`, `cm`,
  `grouping.active` consistent across tasks; `verbose` formal name matches roxygen
  and guard.
