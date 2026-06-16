# tabcf() Repeated K-fold CV + Clip Control — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add repeated K-fold cross-fitting (`R`) and an explicit propensity-clip control (`clip`, deprecating `eps`) to `tabcf()`.

**Architecture:** Extract the single cross-fit loop into a pure, mockable helper `.tabcf_crossfit_once()`; `tabcf()` calls it once per repeat, averages per-unit nuisances via `.tabcf_average_repeats()`. Clip behavior is resolved once by `.tabcf_resolve_clip()` (handles `eps` deprecation + validation), applied per repeat, and an overlap warning (`.tabcf_check_overlap()`) fires when clipping is off.

**Tech Stack:** R, roxygen2 (markdown), testthat edition 3 (`local_mocked_bindings` for TabPFN-free unit tests), grf, tabpfn (Suggests).

**Reference spec:** `dev/specs/2026-06-16-tabcf-repeated-cv-design.md`

---

## File Structure

- Modify: `R/tabcf.R` — new helpers + rewired `tabcf()` body/signature/roxygen.
- Modify: `tests/testthat/test-tabcf.R` — update broken tests, add new unit tests.
- Modify: `NEWS.md` — behavior-change + feature entry.
- Regenerated: `man/tabcf.Rd`, `NAMESPACE` (via `devtools::document()`; helpers stay internal/unexported).

Helper boundaries (all `@keywords internal @noRd`, package-internal):
- `.tabcf_resolve_clip(clip, eps)` -> `list(active, lo, hi)`
- `.tabcf_clip_propensity(W.hat, lo, hi, active)` (generalized from eps)
- `.tabcf_check_overlap(W.hat, active)` (warn-only)
- `.tabcf_average_repeats(reps)` -> `list(Y.hat, W.hat, clipped)`
- `.tabcf_crossfit_once(...)` -> `list(Y.hat, W.hat, clipped)`

---

## Task 1: `.tabcf_resolve_clip()` — clip resolution + eps deprecation

**Files:**
- Modify: `R/tabcf.R` (add helper near other helpers, after `.tabcf_build_tune_args`)
- Test: `tests/testthat/test-tabcf.R`

- [ ] **Step 1: Write failing tests**

```r
# --- .tabcf_resolve_clip ----------------------------------------------------
test_that(".tabcf_resolve_clip: FALSE means no clipping", {
  rc <- UtopiaPlanitia:::.tabcf_resolve_clip
  out <- rc(clip = FALSE, eps = NULL)
  expect_false(out$active)
})

test_that(".tabcf_resolve_clip: TRUE uses default bound", {
  rc <- UtopiaPlanitia:::.tabcf_resolve_clip
  out <- rc(clip = TRUE, eps = NULL)
  expect_true(out$active)
  expect_equal(out$lo, 1e-3)
  expect_equal(out$hi, 1 - 1e-3)
})

test_that(".tabcf_resolve_clip: c(lo,hi) sets manual range", {
  rc <- UtopiaPlanitia:::.tabcf_resolve_clip
  out <- rc(clip = c(0.01, 0.99), eps = NULL)
  expect_true(out$active)
  expect_equal(out$lo, 0.01)
  expect_equal(out$hi, 0.99)
})

test_that(".tabcf_resolve_clip: invalid range errors", {
  rc <- UtopiaPlanitia:::.tabcf_resolve_clip
  expect_error(rc(clip = c(0.9, 0.1)), "0 < lo < hi < 1")
  expect_error(rc(clip = c(-0.1, 0.9)), "0 < lo < hi < 1")
  expect_error(rc(clip = c(0.1, 1.2)), "0 < lo < hi < 1")
  expect_error(rc(clip = c(0.1, 0.2, 0.3)), "FALSE, TRUE, or a numeric")
})

test_that(".tabcf_resolve_clip: eps is deprecated and maps to clip range", {
  rc <- UtopiaPlanitia:::.tabcf_resolve_clip
  expect_warning(out <- rc(clip = FALSE, eps = 0.05), "deprecated")
  expect_true(out$active)
  expect_equal(out$lo, 0.05)
  expect_equal(out$hi, 0.95)
})

test_that(".tabcf_resolve_clip: invalid eps errors", {
  rc <- UtopiaPlanitia:::.tabcf_resolve_clip
  expect_error(suppressWarnings(rc(eps = 0)), "eps")
  expect_error(suppressWarnings(rc(eps = 0.5)), "eps")
  expect_error(suppressWarnings(rc(eps = c(0.1, 0.2))), "eps")
})

test_that(".tabcf_resolve_clip: eps + non-default clip conflict errors", {
  rc <- UtopiaPlanitia:::.tabcf_resolve_clip
  expect_error(rc(clip = c(0.01, 0.99), eps = 0.05), "not both")
  expect_error(rc(clip = TRUE, eps = 0.05), "not both")
})
```

- [ ] **Step 2: Run tests, verify they fail**

Run: `Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-tabcf.R", filter = "resolve_clip")'`
Expected: FAIL — `could not find function ".tabcf_resolve_clip"`.

- [ ] **Step 3: Implement helper**

Add to `R/tabcf.R`:

```r
#' @keywords internal
#' @noRd
#' @description
#' Resolve the user-facing `clip`/`eps` arguments into a normalized
#' `list(active, lo, hi)`. Owns the `eps` deprecation, validation, and the
#' eps/clip conflict error so `tabcf()` stays linear.
.tabcf_resolve_clip <- function(clip = FALSE, eps = NULL) {
  eps_given       <- !is.null(eps)
  clip_is_default <- isFALSE(clip)

  if (eps_given && !clip_is_default) {
    stop("Pass either `clip` or the deprecated `eps`, not both.",
         call. = FALSE)
  }

  if (eps_given) {
    warning("`eps` is deprecated; use `clip = c(lo, hi)`. ",
            "Mapping `eps` to `clip = c(eps, 1 - eps)`.", call. = FALSE)
    if (!is.numeric(eps) || length(eps) != 1L || is.na(eps) ||
        eps <= 0 || eps >= 0.5) {
      stop("`eps` must be a single numeric in (0, 0.5).", call. = FALSE)
    }
    return(list(active = TRUE, lo = eps, hi = 1 - eps))
  }

  if (is.logical(clip) && length(clip) == 1L && !is.na(clip)) {
    if (!clip) return(list(active = FALSE, lo = NA_real_, hi = NA_real_))
    return(list(active = TRUE, lo = 1e-3, hi = 1 - 1e-3))
  }

  if (is.numeric(clip) && length(clip) == 2L && !anyNA(clip)) {
    lo <- clip[1L]; hi <- clip[2L]
    if (!(lo > 0 && hi < 1 && lo < hi)) {
      stop("`clip` range must satisfy 0 < lo < hi < 1.", call. = FALSE)
    }
    return(list(active = TRUE, lo = lo, hi = hi))
  }

  stop("`clip` must be FALSE, TRUE, or a numeric c(lo, hi).", call. = FALSE)
}
```

- [ ] **Step 4: Run tests, verify pass**

Run: `Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-tabcf.R", filter = "resolve_clip")'`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add R/tabcf.R tests/testthat/test-tabcf.R
git commit -m "feat(tabcf): add .tabcf_resolve_clip with eps deprecation"
```

---

## Task 2: Generalize `.tabcf_clip_propensity()` to `lo`/`hi`

**Files:**
- Modify: `R/tabcf.R` (replace existing `.tabcf_clip_propensity`)
- Test: `tests/testthat/test-tabcf.R` (replace existing clip-propensity tests, lines ~174-210)

- [ ] **Step 1: Replace the existing clip-propensity tests**

Delete the four `.tabcf_clip_propensity` tests currently using `eps =` and replace with:

```r
# --- .tabcf_clip_propensity (lo/hi) -----------------------------------------
test_that(".tabcf_clip_propensity clips both tails and counts correctly", {
  clip <- UtopiaPlanitia:::.tabcf_clip_propensity
  W.hat <- c(-0.1, 0, 0.0005, 0.5, 0.9999, 1, 1.5)
  out <- clip(W.hat, lo = 1e-3, hi = 1 - 1e-3, active = TRUE)
  expect_equal(out$clipped, 6L)
  expect_true(all(out$W.hat >= 1e-3))
  expect_true(all(out$W.hat <= 1 - 1e-3))
  expect_equal(out$W.hat[4], 0.5)
})

test_that(".tabcf_clip_propensity is a no-op when active=FALSE", {
  clip <- UtopiaPlanitia:::.tabcf_clip_propensity
  W.hat <- c(-2, 0.5, 3)
  out <- clip(W.hat, lo = 1e-3, hi = 1 - 1e-3, active = FALSE)
  expect_identical(out$W.hat, W.hat)
  expect_equal(out$clipped, 0L)
})

test_that(".tabcf_clip_propensity respects custom lo/hi", {
  clip <- UtopiaPlanitia:::.tabcf_clip_propensity
  W.hat <- c(0.05, 0.5, 0.95)
  out <- clip(W.hat, lo = 0.1, hi = 0.9, active = TRUE)
  expect_equal(out$clipped, 2L)
  expect_equal(out$W.hat, c(0.1, 0.5, 0.9))
})

test_that(".tabcf_clip_propensity returns clipped=0 when nothing to clip", {
  clip <- UtopiaPlanitia:::.tabcf_clip_propensity
  W.hat <- c(0.2, 0.5, 0.8)
  out <- clip(W.hat, lo = 1e-3, hi = 1 - 1e-3, active = TRUE)
  expect_equal(out$clipped, 0L)
  expect_identical(out$W.hat, W.hat)
})
```

- [ ] **Step 2: Run tests, verify they fail**

Run: `Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-tabcf.R", filter = "clip_propensity")'`
Expected: FAIL — `unused argument (lo = 1e-3)`.

- [ ] **Step 3: Replace the helper**

In `R/tabcf.R` replace the whole `.tabcf_clip_propensity` definition with:

```r
#' @keywords internal
#' @noRd
#' @description
#' Clip propensity predictions to `[lo, hi]`. Returns the clipped vector and
#' the count of clipped entries. When `active = FALSE`, returns the input
#' unchanged with `clipped = 0`.
.tabcf_clip_propensity <- function(W.hat, lo = 1e-3, hi = 1 - 1e-3,
                                   active = TRUE) {
  if (!active) {
    return(list(W.hat = W.hat, clipped = 0L))
  }
  n_clip_lo <- sum(W.hat < lo, na.rm = TRUE)
  n_clip_hi <- sum(W.hat > hi, na.rm = TRUE)
  clipped   <- as.integer(n_clip_lo + n_clip_hi)
  if (clipped > 0L) {
    W.hat <- pmin(pmax(W.hat, lo), hi)
  }
  list(W.hat = W.hat, clipped = clipped)
}
```

- [ ] **Step 4: Run tests, verify pass**

Run: `Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-tabcf.R", filter = "clip_propensity")'`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add R/tabcf.R tests/testthat/test-tabcf.R
git commit -m "refactor(tabcf): generalize clip_propensity to lo/hi bounds"
```

---

## Task 3: `.tabcf_check_overlap()` — warn-only overlap check

**Files:**
- Modify: `R/tabcf.R`
- Test: `tests/testthat/test-tabcf.R`

- [ ] **Step 1: Write failing tests**

```r
# --- .tabcf_check_overlap ---------------------------------------------------
test_that(".tabcf_check_overlap warns on out-of-range propensities", {
  chk <- UtopiaPlanitia:::.tabcf_check_overlap
  expect_warning(chk(c(0.005, 0.5, 0.995), active = TRUE),
                 "outside \\[0.01, 0.99\\]")
})

test_that(".tabcf_check_overlap is silent when all in range", {
  chk <- UtopiaPlanitia:::.tabcf_check_overlap
  expect_silent(chk(c(0.2, 0.5, 0.8), active = TRUE))
})

test_that(".tabcf_check_overlap is silent when inactive", {
  chk <- UtopiaPlanitia:::.tabcf_check_overlap
  expect_silent(chk(c(0.001, 0.5, 0.999), active = FALSE))
})

test_that(".tabcf_check_overlap reports count and min/max", {
  chk <- UtopiaPlanitia:::.tabcf_check_overlap
  expect_warning(chk(c(0.005, 0.5, 0.995), active = TRUE), "2 of 3")
})
```

- [ ] **Step 2: Run tests, verify they fail**

Run: `Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-tabcf.R", filter = "check_overlap")'`
Expected: FAIL — function not found.

- [ ] **Step 3: Implement helper**

Add to `R/tabcf.R`:

```r
#' @keywords internal
#' @noRd
#' @description
#' Warn (do not modify) when binary propensity predictions fall outside the
#' fixed overlap window `[0.01, 0.99]`. Reports the count outside and the
#' observed min/max. No-op when `active = FALSE` (continuous W or clipping on).
.tabcf_check_overlap <- function(W.hat, active = TRUE) {
  if (!active) return(invisible(NULL))
  out <- W.hat < 0.01 | W.hat > 0.99
  n_out <- sum(out, na.rm = TRUE)
  if (n_out > 0L) {
    warning("tabcf(): ", n_out, " of ", length(W.hat),
            " propensity prediction(s) fall outside [0.01, 0.99] ",
            "(min = ", signif(min(W.hat), 3L),
            ", max = ", signif(max(W.hat), 3L),
            "). Possible overlap violation; set `clip = TRUE` or ",
            "`clip = c(lo, hi)` to clip.", call. = FALSE)
  }
  invisible(NULL)
}
```

- [ ] **Step 4: Run tests, verify pass**

Run: `Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-tabcf.R", filter = "check_overlap")'`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add R/tabcf.R tests/testthat/test-tabcf.R
git commit -m "feat(tabcf): add .tabcf_check_overlap overlap warning"
```

---

## Task 4: `.tabcf_average_repeats()` — mean nuisances across repeats

**Files:**
- Modify: `R/tabcf.R`
- Test: `tests/testthat/test-tabcf.R`

- [ ] **Step 1: Write failing tests**

```r
# --- .tabcf_average_repeats -------------------------------------------------
test_that(".tabcf_average_repeats averages per-unit and sums clipped", {
  avg <- UtopiaPlanitia:::.tabcf_average_repeats
  reps <- list(
    list(Y.hat = c(0, 2), W.hat = c(0.2, 0.4), clipped = 1L),
    list(Y.hat = c(2, 4), W.hat = c(0.4, 0.6), clipped = 2L)
  )
  out <- avg(reps)
  expect_equal(out$Y.hat, c(1, 3))
  expect_equal(out$W.hat, c(0.3, 0.5))
  expect_equal(out$clipped, 3L)
})

test_that(".tabcf_average_repeats with R=1 returns the single repeat verbatim", {
  avg <- UtopiaPlanitia:::.tabcf_average_repeats
  reps <- list(list(Y.hat = c(1, 2, 3), W.hat = c(0.1, 0.2, 0.3),
                    clipped = 0L))
  out <- avg(reps)
  expect_equal(out$Y.hat, c(1, 2, 3))
  expect_equal(out$W.hat, c(0.1, 0.2, 0.3))
  expect_equal(out$clipped, 0L)
})
```

- [ ] **Step 2: Run tests, verify they fail**

Run: `Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-tabcf.R", filter = "average_repeats")'`
Expected: FAIL — function not found.

- [ ] **Step 3: Implement helper**

Add to `R/tabcf.R`:

```r
#' @keywords internal
#' @noRd
#' @description
#' Average a list of per-repeat cross-fit results elementwise. Each element is
#' `list(Y.hat, W.hat, clipped)`. Returns the mean `Y.hat`/`W.hat` and the
#' total `clipped` across repeats.
.tabcf_average_repeats <- function(reps) {
  R <- length(reps)
  Y.hat   <- Reduce(`+`, lapply(reps, `[[`, "Y.hat")) / R
  W.hat   <- Reduce(`+`, lapply(reps, `[[`, "W.hat")) / R
  clipped <- sum(vapply(reps, `[[`, integer(1L), "clipped"))
  list(Y.hat = Y.hat, W.hat = W.hat, clipped = as.integer(clipped))
}
```

- [ ] **Step 4: Run tests, verify pass**

Run: `Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-tabcf.R", filter = "average_repeats")'`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add R/tabcf.R tests/testthat/test-tabcf.R
git commit -m "feat(tabcf): add .tabcf_average_repeats"
```

---

## Task 5: Extract `.tabcf_crossfit_once()` from the cross-fit loop

**Files:**
- Modify: `R/tabcf.R` (add helper; leave `tabcf()` body untouched in THIS task)
- Test: `tests/testthat/test-tabcf.R` (mock `.tabcf_fit_predict`, no TabPFN needed)

- [ ] **Step 1: Write failing test (mocked fit-predict)**

```r
# --- .tabcf_crossfit_once (mocked) ------------------------------------------
test_that(".tabcf_crossfit_once cross-fits with mocked TabPFN and clips", {
  cf1 <- UtopiaPlanitia:::.tabcf_crossfit_once
  set.seed(1)
  X <- matrix(stats::rnorm(40 * 3), 40, 3); colnames(X) <- paste0("V", 1:3)
  Y <- stats::rnorm(40); W <- stats::rbinom(40, 1, 0.5)

  testthat::local_mocked_bindings(
    # regressor returns 1 for Y, classifier returns 0.5 for W; force a high
    # value into one test slot so clipping has something to clip.
    .tabcf_fit_predict = function(X_train, y_train, X_test, kind,
                                  tabpfn_args = list()) {
      rep(if (kind == "classifier") 0.9999 else 1.0, nrow(X_test))
    }
  )

  out <- cf1(X = X, Y = Y, W = W, w_type = "binary", K = 2,
             clusters = NULL, fold_seed = 1995L, tabpfn_random_base = 1995L,
             clip_active = TRUE, lo = 1e-3, hi = 1 - 1e-3,
             tabpfn_args = list(), user_supplied_control = FALSE,
             verbose = FALSE, repeat_id = 1L)

  expect_length(out$Y.hat, 40L)
  expect_length(out$W.hat, 40L)
  expect_true(all(out$Y.hat == 1.0))
  expect_true(all(out$W.hat <= 1 - 1e-3))   # 0.9999 > 0.999 -> clipped to 0.999
  expect_equal(out$clipped, 40L)            # all 0.9999 > 1-1e-3
})

test_that(".tabcf_crossfit_once aborts on non-finite output", {
  cf1 <- UtopiaPlanitia:::.tabcf_crossfit_once
  X <- matrix(stats::rnorm(20 * 2), 20, 2); colnames(X) <- c("V1", "V2")
  Y <- stats::rnorm(20); W <- stats::rbinom(20, 1, 0.5)
  testthat::local_mocked_bindings(
    .tabcf_fit_predict = function(X_train, y_train, X_test, kind,
                                  tabpfn_args = list()) {
      rep(NA_real_, nrow(X_test))
    }
  )
  expect_error(
    cf1(X = X, Y = Y, W = W, w_type = "binary", K = 2, clusters = NULL,
        fold_seed = 1L, tabpfn_random_base = 1L, clip_active = FALSE,
        lo = NA_real_, hi = NA_real_, tabpfn_args = list(),
        user_supplied_control = FALSE, verbose = FALSE, repeat_id = 2L),
    "repeat 2"
  )
})
```

NOTE on mocking: `testthat::local_mocked_bindings()` mocks the package-internal
`.tabcf_fit_predict` only when the test runs with the package loaded
(`devtools::load_all()` / `devtools::test()`); it requires the binding to exist,
which it does.

- [ ] **Step 2: Run tests, verify they fail**

Run: `Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-tabcf.R", filter = "crossfit_once")'`
Expected: FAIL — function not found.

- [ ] **Step 3: Implement helper**

Add to `R/tabcf.R`:

```r
#' @keywords internal
#' @noRd
#' @description
#' Run one full K-fold cross-fit of the TabPFN nuisances and return
#' `list(Y.hat, W.hat, clipped)`. Builds folds under `fold_seed`, derives each
#' fold's TabPFN `random_state` as `tabpfn_random_base + k` (unless the user
#' supplied their own `control`), checks finiteness/range postconditions
#' (aborting with the `repeat_id` named), and applies per-repeat propensity
#' clipping to `[lo, hi]` when `clip_active` and `w_type == "binary"`.
.tabcf_crossfit_once <- function(X, Y, W, w_type, K, clusters,
                                 fold_seed, tabpfn_random_base,
                                 clip_active, lo, hi,
                                 tabpfn_args, user_supplied_control,
                                 verbose, repeat_id = 1L) {
  n <- length(Y)
  set.seed(fold_seed)
  fold <- .tabcf_make_folds(n = n, K = K, clusters = clusters)

  Y.hat <- numeric(n)
  W.hat <- numeric(n)
  for (k in seq_len(K)) {
    test  <- which(fold == k)
    train <- which(fold != k)
    if (verbose) message("[tabcf] repeat ", repeat_id, " fold ", k, "/", K,
                         " (train n = ", length(train),
                         ", test n = ", length(test), ")")

    fold_args <- tabpfn_args
    if (!user_supplied_control) {
      fold_args$control <- tabpfn::control_tab_pfn(
        random_state = as.integer(tabpfn_random_base) + k
      )
    }

    Y.hat[test] <- .tabcf_fit_predict(
      X_train = X[train, , drop = FALSE], y_train = Y[train],
      X_test  = X[test,  , drop = FALSE],
      kind = "regressor", tabpfn_args = fold_args
    )
    W.hat[test] <- .tabcf_fit_predict(
      X_train = X[train, , drop = FALSE], y_train = W[train],
      X_test  = X[test,  , drop = FALSE],
      kind = if (w_type == "binary") "classifier" else "regressor",
      tabpfn_args = fold_args
    )
  }

  if (!all(is.finite(Y.hat))) {
    stop("tabcf(): TabPFN returned non-finite Y.hat in repeat ", repeat_id,
         ". Check tabpfn installation/output shape.", call. = FALSE)
  }
  if (w_type == "binary" && !all(W.hat >= 0 & W.hat <= 1, na.rm = TRUE)) {
    stop("tabcf(): TabPFN classifier returned out-of-range W.hat in repeat ",
         repeat_id, " (not in [0,1]).", call. = FALSE)
  }
  if (!all(is.finite(W.hat))) {
    stop("tabcf(): TabPFN returned non-finite W.hat in repeat ", repeat_id,
         ".", call. = FALSE)
  }

  clip_res <- .tabcf_clip_propensity(
    W.hat, lo = lo, hi = hi,
    active = isTRUE(clip_active) && (w_type == "binary")
  )
  list(Y.hat = Y.hat, W.hat = clip_res$W.hat, clipped = clip_res$clipped)
}
```

- [ ] **Step 4: Run tests, verify pass**

Run: `Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-tabcf.R", filter = "crossfit_once")'`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add R/tabcf.R tests/testthat/test-tabcf.R
git commit -m "refactor(tabcf): extract .tabcf_crossfit_once helper"
```

---

## Task 6: Rewire `tabcf()` — signature, repeat loop, clip, meta

**Files:**
- Modify: `R/tabcf.R` (`tabcf()` signature + body sections 1, 4, 5, 5b, 6, 7)
- Test: `tests/testthat/test-tabcf.R` (update signature/validation tests; add R-validation + mocked end-to-end averaging tests)

- [ ] **Step 1: Update the signature/validation tests and add new ones**

Replace the existing `"tabcf signature exposes expected formals and defaults"`
test with:

```r
test_that("tabcf signature exposes expected formals and defaults", {
  f <- formals(tabcf)
  expect_true(all(c("c.forest", "X", "Y", "W", "K", "R", "seed", "clip",
                    "eps", "tabpfn_args", "verbose") %in% names(f)))
  expect_equal(eval(f$K), 5L)
  expect_equal(eval(f$R), 1L)
  expect_equal(eval(f$seed), 1995L)
  expect_false(eval(f$clip))          # clip = FALSE default
  expect_null(eval(f$eps))            # eps = NULL default (deprecated)
})
```

Replace the existing `"tabcf rejects invalid eps"` test with:

```r
test_that("tabcf rejects invalid eps (deprecated path)", {
  cf <- make_small_cf(n = 40, w_kind = "binary")
  expect_error(suppressWarnings(tabcf(cf, eps = 0)),    "eps")
  expect_error(suppressWarnings(tabcf(cf, eps = 0.5)),  "eps")
  expect_error(suppressWarnings(tabcf(cf, eps = c(0.1, 0.2))), "eps")
})

test_that("tabcf rejects invalid R", {
  cf <- make_small_cf(n = 40, w_kind = "binary")
  expect_error(tabcf(cf, R = 0),   "R")
  expect_error(tabcf(cf, R = 1.5), "R")
  expect_error(tabcf(cf, R = NA),  "R")
})

test_that("tabcf rejects invalid clip range early", {
  cf <- make_small_cf(n = 40, w_kind = "binary")
  expect_error(tabcf(cf, clip = c(0.9, 0.1)), "0 < lo < hi < 1")
})

test_that("tabcf errors when both eps and clip supplied", {
  cf <- make_small_cf(n = 40, w_kind = "binary")
  expect_error(tabcf(cf, clip = c(0.01, 0.99), eps = 0.05), "not both")
})
```

Add a mocked end-to-end test that bypasses TabPFN/token (these gate `tabcf()`
before cross-fitting, so we mock the gate functions AND the fold helper output
path via `.tabcf_crossfit_once`):

```r
test_that("tabcf averages R repeats and records R in meta (mocked)", {
  cf <- make_small_cf(n = 60, w_kind = "binary")

  # Bypass the tabpfn install + token gates and the per-repeat cross-fit.
  testthat::local_mocked_bindings(
    .tabcf_crossfit_once = function(X, Y, W, w_type, K, clusters, fold_seed,
                                    tabpfn_random_base, clip_active, lo, hi,
                                    tabpfn_args, user_supplied_control,
                                    verbose, repeat_id = 1L) {
      n <- length(Y)
      # repeat r returns constant nuisances r/10 so the mean is checkable
      list(Y.hat = rep(repeat_id, n),
           W.hat = rep(repeat_id / 10, n),
           clipped = repeat_id)
    }
  )
  testthat::local_mocked_bindings(
    .pkg = "rlang",
    check_installed = function(...) invisible(TRUE)
  )
  withr::local_envvar(TABPFN_TOKEN = "dummy")

  out <- tabcf(cf, K = 2, R = 3, clip = TRUE)
  meta <- attr(out, "tabcf_meta")
  expect_equal(meta$R, 3L)
  # mean of {1,2,3} = 2 ; mean of {0.1,0.2,0.3} = 0.2
  expect_true(all(abs(out$Y.hat - 2) < 1e-8))
  expect_true(all(abs(out$W.hat - 0.2) < 1e-8))
  expect_equal(meta$clipped, 6L)        # 1 + 2 + 3
})
```

NOTE: if mocking `rlang::check_installed` via `local_mocked_bindings(.pkg=...)`
is unavailable in the installed testthat version, gate this test with
`skip_if_not_installed("tabpfn")` + token instead and drop the rlang mock; the
`.tabcf_crossfit_once` mock still removes the need for a live TabPFN backend
(the token/install gate is the only remaining barrier).

- [ ] **Step 2: Run tests, verify they fail**

Run: `Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-tabcf.R", filter = "tabcf")'`
Expected: FAIL — signature lacks `R`/`clip`; `meta$R` missing.

- [ ] **Step 3: Rewrite `tabcf()` signature and body**

In `R/tabcf.R`, change the signature to:

```r
tabcf <- function(c.forest,
                  X = NULL, Y = NULL, W = NULL,
                  K = 5L,
                  R = 1L,
                  seed = 1995L,
                  clip = FALSE,
                  eps = NULL,
                  tuning = c("orig", "cf.default", "cf.autotune"),
                  tabpfn_args = list(),
                  verbose = FALSE,
                  ...) {
```

In section `# ---- 1. validate inputs`, after the `K` validation block, add `R`
validation and replace the old `eps` validation with clip resolution:

```r
  if (!is.numeric(R) || length(R) != 1L || is.na(R) ||
      R < 1L || R != as.integer(R)) {
    stop("`R` must be an integer >= 1.", call. = FALSE)
  }
  R <- as.integer(R)

  # Resolve clip / deprecated eps into list(active, lo, hi). Surfaces eps
  # deprecation, range validation, and the eps/clip conflict error early.
  clip_spec <- .tabcf_resolve_clip(clip, eps)
```

Delete the old `eps` validation block:

```r
  # DELETE:
  # if (!is.numeric(eps) || length(eps) != 1L || eps <= 0 || eps >= 0.5) {
  #   stop("`eps` must be a single numeric in (0, 0.5).", call. = FALSE)
  # }
```

Replace sections `# ---- 4. build folds`, `# ---- 5. cross-fit nuisances`,
`# ---- 5b. validate`, and the clip block with the repeat loop + averaging:

```r
  # ---- 4-5. repeated cross-fit nuisances ----------------------------------
  clusters <- c.forest$clusters
  user_supplied_control <- "control" %in% names(tabpfn_args)

  reps <- vector("list", R)
  for (r in seq_len(R)) {
    reps[[r]] <- .tabcf_crossfit_once(
      X = X, Y = Y, W = W, w_type = w_type, K = K, clusters = clusters,
      fold_seed          = as.integer(seed) + (r - 1L),
      tabpfn_random_base = as.integer(seed) + (r - 1L) * K,
      clip_active = clip_spec$active, lo = clip_spec$lo, hi = clip_spec$hi,
      tabpfn_args = tabpfn_args, user_supplied_control = user_supplied_control,
      verbose = verbose, repeat_id = r
    )
  }

  agg     <- .tabcf_average_repeats(reps)
  Y.hat   <- agg$Y.hat
  W.hat   <- agg$W.hat
  clipped <- agg$clipped

  # Overlap warning fires only when clipping is OFF (binary W).
  .tabcf_check_overlap(W.hat, active = (!clip_spec$active && w_type == "binary"))

  if (clipped > 0L) {
    warning("tabcf(): clipped ", clipped,
            " propensity prediction(s) across ", R, " repeat(s) to [",
            clip_spec$lo, ", ", clip_spec$hi, "].", call. = FALSE)
  }
```

In section `# ---- 7. annotate and return`, replace the meta list with:

```r
  attr(out, "tabcf_meta") <- list(
    K       = K,
    R       = R,
    seed    = seed,
    clip    = if (clip_spec$active) c(clip_spec$lo, clip_spec$hi) else FALSE,
    w_type  = w_type,
    clipped = clipped,
    tuning  = tuning
  )
```

Leave section `# ---- 6. refit causal forest` unchanged (it already does
`set.seed(seed)` then builds `cf_args` from `Y.hat`/`W.hat`).

- [ ] **Step 4: Run tests, verify pass**

Run: `Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-tabcf.R", filter = "tabcf")'`
Expected: PASS (mocked averaging test included).

- [ ] **Step 5: Commit**

```bash
git add R/tabcf.R tests/testthat/test-tabcf.R
git commit -m "feat(tabcf): repeated K-fold CV (R) + polymorphic clip control"
```

---

## Task 7: Update TabPFN-gated end-to-end tests for new meta/clip

**Files:**
- Test: `tests/testthat/test-tabcf.R` (gated tests referencing `meta$eps`)

- [ ] **Step 1: Update the gated tests**

In `"tabcf cross-fits TabPFN nuisances ... (binary W)"`, replace
`expect_equal(meta$eps, 1e-3)` with:

```r
  expect_equal(meta$R, 1L)
  expect_identical(meta$clip, FALSE)   # default: no clipping
```

Replace the whole `"tabcf accepts custom eps and propagates to meta"` test with:

```r
test_that("tabcf clip=c(lo,hi) clips and records range in meta", {
  skip_if_not_installed("tabpfn")
  skip_if(!nzchar(Sys.getenv("TABPFN_TOKEN")),
          "TABPFN_TOKEN not set; skipping live TabPFN test.")
  cf  <- make_small_cf(n = 60, w_kind = "binary")
  cf2 <- tabcf(cf, K = 2, clip = c(0.05, 0.95))
  meta <- attr(cf2, "tabcf_meta")
  expect_equal(meta$clip, c(0.05, 0.95))
  expect_true(all(cf2$W.hat >= 0.05))
  expect_true(all(cf2$W.hat <= 0.95))
})

test_that("tabcf eps still works via deprecation (gated)", {
  skip_if_not_installed("tabpfn")
  skip_if(!nzchar(Sys.getenv("TABPFN_TOKEN")),
          "TABPFN_TOKEN not set; skipping live TabPFN test.")
  cf  <- make_small_cf(n = 60, w_kind = "binary")
  expect_warning(cf2 <- tabcf(cf, K = 2, eps = 0.05), "deprecated")
  expect_equal(attr(cf2, "tabcf_meta")$clip, c(0.05, 0.95))
})

test_that("tabcf R>1 averages nuisances end-to-end (gated)", {
  skip_if_not_installed("tabpfn")
  skip_if(!nzchar(Sys.getenv("TABPFN_TOKEN")),
          "TABPFN_TOKEN not set; skipping live TabPFN test.")
  cf  <- make_small_cf(n = 60, w_kind = "binary")
  cf2 <- tabcf(cf, K = 2, R = 2, clip = TRUE)
  expect_equal(attr(cf2, "tabcf_meta")$R, 2L)
  expect_length(cf2$W.hat, nrow(cf$X.orig))
})
```

In `"tabcf handles continuous W (regressor for propensity)"`, the
`expect_equal(meta$clipped, 0L)` line stays valid (continuous never clips).

- [ ] **Step 2: Run the full file (gated tests skip without token)**

Run: `Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-tabcf.R")'`
Expected: PASS or SKIP (no failures). Gated tests SKIP without `TABPFN_TOKEN`.

- [ ] **Step 3: Commit**

```bash
git add tests/testthat/test-tabcf.R
git commit -m "test(tabcf): update gated e2e tests for clip/R meta"
```

---

## Task 8: Roxygen docs + regenerate man/

**Files:**
- Modify: `R/tabcf.R` (roxygen block above `tabcf`)
- Regenerated: `man/tabcf.Rd`, `NAMESPACE`

- [ ] **Step 1: Update the roxygen block**

Make these edits to the `#'` block above `tabcf()`:

Add after the `@param K` entry:

```r
#' @param R Integer >= 1. Number of repeated K-fold cross-fits. Each repeat
#'   uses a distinct fold partition (seeded `seed + (r - 1)`) and the per-unit
#'   nuisance predictions are averaged across repeats to reduce the
#'   Monte-Carlo variance of a single partition. `R = 1` (default) reproduces
#'   the original single cross-fit exactly. Cost scales as `R * K * 2` TabPFN
#'   fits.
```

Replace the `@param eps` entry with `@param clip` + a deprecated `@param eps`:

```r
#' @param clip Propensity clipping control for binary `W`. One of:
#'   \itemize{
#'     \item `FALSE` (default): no clipping. If any averaged propensity falls
#'       outside `[0.01, 0.99]`, a warning reports the count and min/max.
#'     \item `TRUE`: clip to `[1e-3, 1 - 1e-3]`.
#'     \item `c(lo, hi)`: clip to `[lo, hi]` with `0 < lo < hi < 1`.
#'   }
#'   When clipping is active it is applied within each repeat before
#'   averaging. Ignored for continuous `W`.
#' @param eps `r lifecycle::badge("deprecated")` Deprecated. Use `clip`.
#'   If supplied, maps to `clip = c(eps, 1 - eps)` with a deprecation warning;
#'   passing both `eps` and `clip` is an error.
```

In `@return`, update the `tabcf_meta` `\describe{}` block: replace the `eps`
item with:

```r
#'     \item{R}{Number of repeated cross-fits.}
#'     \item{clip}{The resolved clip setting: `FALSE` (no clipping) or the
#'       numeric `c(lo, hi)` bounds used.}
```

In `@details`, update the **Cross-fitting** paragraph to mention repeats:

```r
#' **Cross-fitting and repeats.** For each of `R` repeats, TabPFN nuisance
#' predictions are produced via K-fold cross-fitting under a distinct fold
#' partition; the per-unit predictions are then averaged across repeats. With
#' `R = 1` this is a single cross-fit identical to earlier versions. If
#' `c.forest$clusters` is non-trivial, folds keep each cluster intact.
```

Add an example before the closing `}` of `@examples`:

```r
#' # repeated 10x 5-fold cross-fit, clip propensities to [0.01, 0.99]
#' cf5 <- tabcf(cf, K = 5, R = 10, clip = c(0.01, 0.99))
```

- [ ] **Step 2: Regenerate documentation**

Run: `Rscript -e 'devtools::document()'`
Expected: `man/tabcf.Rd` updated; no errors. `NAMESPACE` unchanged (helpers stay
unexported).

- [ ] **Step 3: Commit**

```bash
git add R/tabcf.R man/tabcf.Rd NAMESPACE
git commit -m "docs(tabcf): document R and clip; deprecate eps"
```

---

## Task 9: NEWS.md entry

**Files:**
- Modify: `NEWS.md`

- [ ] **Step 1: Add an entry at the top (under the dev/version heading)**

```markdown
## tabcf() repeated cross-fitting and clip control

* `tabcf()` gains an `R` argument for repeated K-fold cross-fitting: nuisance
  predictions are averaged over `R` fold partitions to cut Monte-Carlo
  variance. `R = 1` (default) is identical to previous behavior.
* New `clip` argument controls binary-propensity clipping: `FALSE` (default,
  no clipping with an overlap warning), `TRUE` (`[1e-3, 1-1e-3]`), or
  `c(lo, hi)`.
* **Behavior change:** binary propensities are no longer clipped by default.
  Previous versions always clipped at `1e-3`; the default is now warn-only.
  Pass `clip = TRUE` to restore the old clipping.
* `eps` is **deprecated** in favor of `clip`; it maps to `clip = c(eps, 1-eps)`
  with a warning.
```

- [ ] **Step 2: Commit**

```bash
git add NEWS.md
git commit -m "docs(news): tabcf R + clip control, eps deprecation"
```

---

## Task 10: Full validation

**Files:** none (verification only)

- [ ] **Step 1: Run the full test suite**

Run: `Rscript -e 'devtools::test()'`
Expected: all PASS or SKIP; 0 failures. (TabPFN-gated tests SKIP without token.)

- [ ] **Step 2: Run R CMD check**

Run: `Rscript -e 'devtools::check()'`
Expected: 0 errors, 0 warnings. Notes acceptable only if pre-existing.

- [ ] **Step 3: Lint**

Run: `Rscript -e 'lintr::lint_package()'`
Expected: no new lint vs. baseline.

- [ ] **Step 4: Final commit (if document() or lint produced changes)**

```bash
git add -A
git commit -m "chore(tabcf): finalize repeated CV + clip control"
```

---

## Self-Review Notes (filled by plan author)

- **Spec coverage:** R arg (T6), seeding R=1-identical (T5/T6), averaging (T4/T6),
  abort-on-failure (T5), clip FALSE/TRUE/range (T1/T6), eps deprecation +
  conflict (T1/T6/T7), overlap warn (T3/T6), per-repeat clip then average
  (T5/T6), meta R+clip (T6), docs (T8), NEWS behavior-change (T9), tests for
  every branch (T1-T7). All spec sections mapped.
- **Type consistency:** helper names/signatures identical across tasks
  (`.tabcf_resolve_clip`, `.tabcf_clip_propensity(W.hat, lo, hi, active)`,
  `.tabcf_check_overlap(W.hat, active)`, `.tabcf_average_repeats(reps)`,
  `.tabcf_crossfit_once(...)`); `clip_spec` fields `active/lo/hi` used uniformly.
- **Mocking caveat:** Task 6's rlang mock has a documented fallback to gating
  if `local_mocked_bindings(.pkg="rlang")` is unsupported.
