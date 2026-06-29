# plot_pdp() subgroup ATE mode — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add an opt-in `subgroup = FALSE` mode to `plot_pdp()` that, for a discrete covariate, plots the doubly-robust AIPW subgroup ATE per observed value (`grf::average_treatment_effect(cf, subset = ...)`) instead of a smooth partial-dependence curve.

**Architecture:** One new trailing formal (`subgroup`) on `plot_pdp()`; a dispatch branch routes to new internal helpers. Computation is separated from plotting so it is unit-testable: `.subgroup_ate_one()` (one subset) feeds `.subgroup_ate_1way_df()` / `.subgroup_ate_2way_df()` (assemble a data.frame), which feed `.plot_subgroup_1way()` / `.plot_subgroup_2way()` (build ggplot). A shared `.utopia_pdp_theme()` removes theme duplication in the new code, and `.as_utopia_plot()` tags a plain ggplot grob with the `utopia_plot` class (no ggMarginal). The existing PDP path is untouched.

**Tech Stack:** R, grf (>= 2.3.2), ggplot2 (Suggests), testthat edition 3, roxygen2/devtools.

> **Commit policy:** Commit steps follow TDD convention, but Jack commits manually — do **NOT** push or commit unless Jack confirms. Stage and show diffs instead if unsure.

> **Spec:** `dev/specs/2026-06-25-pdp-subgroup-ate-design.md`

> **API note:** `subgroup` is a new trailing formal defaulting to `FALSE`, reproducing current behavior byte-for-byte. No existing argument is renamed/reordered/re-defaulted. `grf::average_treatment_effect()` has no `num.threads`, so `num.threads` is ignored in subgroup mode (documented).

---

## File Structure

- Modify: `R/plot_pdp.R` — add `subgroup` formal + dispatch; add internal helpers (`.subgroup_ate_one`, `.subgroup_ate_1way_df`, `.subgroup_ate_2way_df`, `.plot_subgroup_1way`, `.plot_subgroup_2way`, `.utopia_pdp_theme`); roxygen for `subgroup`.
- Modify: `R/utopia_plot.R` — add `.as_utopia_plot()` (no-marginal wrapper) next to `.wrap_marginal()`.
- Create: `tests/testthat/test-pdp-subgroup.R` — unit tests for the df helpers + smoke tests for the plot path.
- Modify: `NEWS.md` — development-version entry.
- Regenerate: `man/plot_pdp.Rd` via `devtools::document()`.
- Optional: `tests/testthat/test-fixes-20260511.R` — add a `plot_pdp` formals guard (none exists today).

---

## Shared test fixture

Every test file builds a small causal forest with a binary and a 3-level integer covariate. Put this helper at the top of `tests/testthat/test-pdp-subgroup.R`:

```r
make_cf <- function(n = 400, seed = 1995) {
  set.seed(seed)
  p <- 4
  X <- matrix(stats::rnorm(n * p), n, p)
  colnames(X) <- paste0("X", seq_len(p))
  X[, "X1"] <- rbinom(n, 1, 0.5)            # binary covariate
  X[, "X2"] <- sample(0:2, n, replace = TRUE) # 3-level integer covariate
  W <- rbinom(n, 1, 0.5)
  Y <- X[, "X1"] * W + stats::rnorm(n)
  grf::causal_forest(X, Y, W, num.trees = 200)
}
```

---

### Task 1: `.subgroup_ate_one()` + `.subgroup_ate_1way_df()`

**Files:**
- Create: `tests/testthat/test-pdp-subgroup.R`
- Modify: `R/plot_pdp.R` (append internal helpers after the existing helpers, near line 152)

- [ ] **Step 1: Write the failing test**

Create `tests/testthat/test-pdp-subgroup.R` with the `make_cf()` helper above, then add:

```r
test_that(".subgroup_ate_1way_df returns one row per level with CI + signif", {
  skip_on_cran()
  cf <- make_cf()
  df <- UtopiaPlanitia:::.subgroup_ate_1way_df(cf, "X1")

  expect_s3_class(df, "data.frame")
  expect_setequal(df$level, c(0, 1))
  expect_true(all(c("estimate", "std.err", "lower", "upper", "signif", "level")
                  %in% names(df)))
  expect_type(df$signif, "logical")
  # CI consistency
  expect_equal(df$lower, df$estimate - 1.96 * df$std.err)
  expect_equal(df$upper, df$estimate + 1.96 * df$std.err)
})
```

- [ ] **Step 2: Run test to verify it fails**

Run: `Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-pdp-subgroup.R")'`
Expected: FAIL — `.subgroup_ate_1way_df` not found.

- [ ] **Step 3: Implement the two helpers**

Append to `R/plot_pdp.R` (after `.compute_pd`, before `.point_in_polygon`, or at end of the helper block — any internal-helper location):

```r
#' Doubly-robust AIPW ATE on one subset, with 95% CI and significance flag.
#' Returns NULL (and warns) if grf cannot estimate the subset.
#' @keywords internal
#' @noRd
.subgroup_ate_one <- function(c.forest, subset, label) {
  res <- tryCatch(
    grf::average_treatment_effect(c.forest, subset = subset),
    error = function(e) {
      warning("plot_pdp(subgroup): ", label, " skipped (",
              conditionMessage(e), ")", call. = FALSE)
      NULL
    }
  )
  if (is.null(res)) return(NULL)
  est <- res[["estimate"]]
  se  <- res[["std.err"]]
  data.frame(
    estimate = est,
    std.err  = se,
    lower    = est - 1.96 * se,
    upper    = est + 1.96 * se,
    signif   = (est - 1.96 * se) > 0 | (est + 1.96 * se) < 0
  )
}

#' Assemble per-level subgroup ATEs for a single covariate.
#' Levels are sort(unique(x)); failed levels are warned + dropped.
#' @keywords internal
#' @noRd
.subgroup_ate_1way_df <- function(c.forest, x_var) {
  x.col <- .col_vec(c.forest$X.orig, x_var)
  lv <- sort(unique(x.col[!is.na(x.col)]))
  rows <- lapply(lv, function(v) {
    s <- !is.na(x.col) & x.col == v
    one <- .subgroup_ate_one(c.forest, s, paste0(x_var, " = ", v))
    if (is.null(one)) return(NULL)
    one$level <- v
    one
  })
  df <- do.call(rbind, rows)
  if (is.null(df) || nrow(df) == 0L) {
    stop("plot_pdp(subgroup): no level of '", x_var,
         "' had enough units to estimate an ATE.", call. = FALSE)
  }
  df
}
```

- [ ] **Step 4: Run test to verify it passes**

Run: `Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-pdp-subgroup.R")'`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add R/plot_pdp.R tests/testthat/test-pdp-subgroup.R
git commit -m "feat(plot_pdp): add subgroup ATE computation helpers"
```

---

### Task 2: 1-way plot helper + `subgroup` formal + dispatch

**Files:**
- Modify: `R/utopia_plot.R` (add `.as_utopia_plot` after `.wrap_marginal`, ~line 16)
- Modify: `R/plot_pdp.R` (theme helper, plot helper, signature line 72-79, dispatch after line 101)
- Modify: `tests/testthat/test-pdp-subgroup.R`

- [ ] **Step 1: Write the failing test**

Append to `tests/testthat/test-pdp-subgroup.R`:

```r
test_that("plot_pdp(subgroup = TRUE) 1-way returns a utopia_plot", {
  skip_on_cran()
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("ggExtra")
  cf <- make_cf()
  p <- plot_pdp(cf, x_var = "X1", subgroup = TRUE)
  expect_s3_class(p, "utopia_plot")
})

test_that("plot_pdp(subgroup = FALSE) is unchanged (still utopia_plot)", {
  skip_on_cran()
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("ggExtra")
  cf <- make_cf()
  p <- plot_pdp(cf, x_var = "X1")
  expect_s3_class(p, "utopia_plot")
})
```

- [ ] **Step 2: Run test to verify it fails**

Run: `Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-pdp-subgroup.R")'`
Expected: FAIL — `unused argument (subgroup = TRUE)`.

- [ ] **Step 3: Add `.as_utopia_plot()` to `R/utopia_plot.R`**

Insert after the closing brace of `.wrap_marginal` (after line 16):

```r
#' Wrap a plain ggplot (no marginal histograms) as a `utopia_plot`.
#'
#' Converts the ggplot to a grob and tags it `"utopia_plot"` so
#' [print.utopia_plot()] renders it via `grid::grid.draw()`. Used by the
#' discrete subgroup plots, where ggMarginal histograms are meaningless.
#' @param p A ggplot object.
#' @return An object of class `c("utopia_plot", "gtable", "grob", ...)`.
#' @keywords internal
#' @noRd
.as_utopia_plot <- function(p) {
  g <- ggplot2::ggplotGrob(p)
  class(g) <- c("utopia_plot", class(g))
  g
}
```

- [ ] **Step 4: Add the shared theme helper and the 1-way plot helper to `R/plot_pdp.R`**

Append to the internal-helper block of `R/plot_pdp.R`:

```r
#' Shared ggplot theme for the subgroup plots (matches the PDP panels).
#' @keywords internal
#' @noRd
.utopia_pdp_theme <- function() {
  ggplot2::theme(
    text = ggplot2::element_text(size = 12, family = "serif"),
    panel.background = ggplot2::element_rect(fill = "#e6e6e6"),
    plot.title = ggplot2::element_text(hjust = 0.5),
    legend.position = "bottom",
    panel.border = ggplot2::element_blank(),
    axis.text = ggplot2::element_text(),
    axis.title = ggplot2::element_text(size = ggplot2::rel(1.2)),
    strip.text = ggplot2::element_text(hjust = 0),
    strip.background = ggplot2::element_rect(fill = NA, color = NA),
    legend.key = ggplot2::element_blank(),
    complete = TRUE
  )
}

#' 1-way discrete subgroup ATE plot: point + 95% CI per level.
#' @keywords internal
#' @noRd
.plot_subgroup_1way <- function(c.forest, x_var, show_ate_region,
                                y.limits, xlab) {
  df <- .subgroup_ate_1way_df(c.forest, x_var)
  df$level <- factor(as.character(df$level),
                     levels = as.character(sort(unique(df$level))))
  x.label <- if (!is.null(xlab)) xlab else x_var

  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data[["level"]],
                                        y = .data[["estimate"]]))

  if (show_ate_region) {
    ate.result <- grf::average_treatment_effect(c.forest)
    ate <- ate.result[["estimate"]]
    hi  <- ate + 1.96 * ate.result[["std.err"]]
    lo  <- ate - 1.96 * ate.result[["std.err"]]
    p <- p +
      ggplot2::geom_hline(yintercept = ate, color = "black", linewidth = 0.5) +
      ggplot2::geom_hline(yintercept = hi, linetype = "dotted") +
      ggplot2::geom_hline(yintercept = lo, linetype = "dotted")
  }

  p <- p +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = .data[["lower"]], ymax = .data[["upper"]]),
      width = 0.15, color = "#5ab0c0", linewidth = 0.8) +
    ggplot2::geom_point(color = "#ea794e", size = 2.5) +
    ggplot2::labs(x = x.label, y = "CATE") +
    ggplot2::scale_y_continuous(limits = y.limits) +
    .utopia_pdp_theme()

  .as_utopia_plot(p)
}
```

- [ ] **Step 5: Add the `subgroup` formal and dispatch branch in `plot_pdp()`**

In `R/plot_pdp.R`, change the signature (lines 72-79) by adding `subgroup = FALSE` as the final argument:

```r
plot_pdp <- function(c.forest, x_var, y_var = NULL,
                     grid_size = 50, n_max = 2000,
                     show_ate_region = TRUE, show_scatter = TRUE,
                     trim = TRUE,
                     x.limits = NULL, y.limits = NULL,
                     color.var = NULL, color.cat = NULL,
                     color.lab = NULL, xlab = NULL,
                     num.threads = NULL,
                     subgroup = FALSE) {
```

Then insert the dispatch immediately after the `color.var` / `y_var` exclusion check (current lines 99-101), before the `n_max` subsample logic (current line 103):

```r
  if (subgroup) {
    if (is.null(y_var)) {
      return(.plot_subgroup_1way(c.forest, x_var, show_ate_region,
                                 y.limits, xlab))
    }
    return(.plot_subgroup_2way(c.forest, x_var, y_var, xlab))
  }
```

(The `.plot_subgroup_2way` reference is implemented in Task 4; until then the 2-way branch is unreachable in tests, which only exercise 1-way here.)

- [ ] **Step 6: Run tests to verify they pass**

Run: `Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-pdp-subgroup.R")'`
Expected: PASS (both new tests + Task 1 tests).

- [ ] **Step 7: Commit**

```bash
git add R/plot_pdp.R R/utopia_plot.R tests/testthat/test-pdp-subgroup.R
git commit -m "feat(plot_pdp): add subgroup arg + 1-way subgroup ATE plot"
```

---

### Task 3: Robustness — warn + skip tiny subsets, stop when all fail

**Files:**
- Modify: `tests/testthat/test-pdp-subgroup.R`

- [ ] **Step 1: Write the failing tests**

Append to `tests/testthat/test-pdp-subgroup.R`:

```r
test_that("a level with too few units is warned and dropped", {
  skip_on_cran()
  cf <- make_cf(n = 200)
  # Inject a rare extra level (single unit) into X2 of the stored matrix
  cf$X.orig[1, "X2"] <- 99
  expect_warning(
    df <- UtopiaPlanitia:::.subgroup_ate_1way_df(cf, "X2"),
    "skipped"
  )
  expect_false(99 %in% df$level)   # the singleton level was dropped
})

test_that("all-fail subsets raise an informative error", {
  skip_on_cran()
  cf <- make_cf(n = 120)
  # X3 is continuous => every value unique => every subset has 1 unit => all fail
  expect_error(
    suppressWarnings(UtopiaPlanitia:::.subgroup_ate_1way_df(cf, "X3")),
    "no level of 'X3'"
  )
})
```

- [ ] **Step 2: Run tests to verify behavior**

Run: `Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-pdp-subgroup.R")'`
Expected: PASS — the helpers from Task 1 already implement warn-skip and the all-fail `stop()`. If a test FAILS, fix the helper, not the test.

- [ ] **Step 3: Commit**

```bash
git add tests/testthat/test-pdp-subgroup.R
git commit -m "test(plot_pdp): cover subgroup small-subset warn/skip + all-fail stop"
```

---

### Task 4: 2-way subgroup heatmap with significance marks

**Files:**
- Modify: `R/plot_pdp.R` (add `.subgroup_ate_2way_df` + `.plot_subgroup_2way`)
- Modify: `tests/testthat/test-pdp-subgroup.R`

- [ ] **Step 1: Write the failing tests**

Append to `tests/testthat/test-pdp-subgroup.R`:

```r
test_that(".subgroup_ate_2way_df returns one row per non-empty cell with a label", {
  skip_on_cran()
  cf <- make_cf()
  df <- UtopiaPlanitia:::.subgroup_ate_2way_df(cf, "X1", "X2")
  expect_s3_class(df, "data.frame")
  expect_true(all(c("xv", "yv", "estimate", "signif", "lab") %in% names(df)))
  # X1 has 2 levels, X2 has 3 => up to 6 cells
  expect_lte(nrow(df), 6)
  expect_gt(nrow(df), 0)
  # significant cells carry a trailing asterisk
  expect_true(all(grepl("\\*$", df$lab[df$signif])))
  expect_false(any(grepl("\\*$", df$lab[!df$signif])))
})

test_that("plot_pdp 2-way subgroup returns a utopia_plot", {
  skip_on_cran()
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("ggExtra")
  cf <- make_cf()
  p <- plot_pdp(cf, x_var = "X1", y_var = "X2", subgroup = TRUE)
  expect_s3_class(p, "utopia_plot")
})
```

- [ ] **Step 2: Run tests to verify they fail**

Run: `Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-pdp-subgroup.R")'`
Expected: FAIL — `.subgroup_ate_2way_df` not found.

- [ ] **Step 3: Implement the 2-way helpers in `R/plot_pdp.R`**

Append to the internal-helper block:

```r
#' Assemble subgroup ATEs over the cross of two covariates' levels.
#' Empty and failed cells are skipped; significance => trailing "*".
#' @keywords internal
#' @noRd
.subgroup_ate_2way_df <- function(c.forest, x_var, y_var) {
  x.col <- .col_vec(c.forest$X.orig, x_var)
  y.col <- .col_vec(c.forest$X.orig, y_var)
  x.lv <- sort(unique(x.col[!is.na(x.col)]))
  y.lv <- sort(unique(y.col[!is.na(y.col)]))

  cells <- list()
  for (vx in x.lv) {
    for (vy in y.lv) {
      s <- !is.na(x.col) & !is.na(y.col) & x.col == vx & y.col == vy
      if (!any(s)) next
      one <- .subgroup_ate_one(
        c.forest, s, paste0(x_var, "=", vx, ", ", y_var, "=", vy))
      if (is.null(one)) next
      one$xv <- vx
      one$yv <- vy
      cells[[length(cells) + 1L]] <- one
    }
  }
  df <- do.call(rbind, cells)
  if (is.null(df) || nrow(df) == 0L) {
    stop("plot_pdp(subgroup): no (", x_var, ", ", y_var,
         ") cell had enough units to estimate an ATE.", call. = FALSE)
  }
  df$lab <- paste0(formatC(df$estimate, format = "f", digits = 2),
                   ifelse(df$signif, "*", ""))
  df
}

#' 2-way discrete subgroup ATE heatmap with significance asterisks.
#' @keywords internal
#' @noRd
.plot_subgroup_2way <- function(c.forest, x_var, y_var, xlab) {
  df <- .subgroup_ate_2way_df(c.forest, x_var, y_var)
  x.lv <- sort(unique(df$xv))
  y.lv <- sort(unique(df$yv))
  df$xv <- factor(as.character(df$xv), levels = as.character(x.lv))
  df$yv <- factor(as.character(df$yv), levels = as.character(y.lv))
  x.label <- if (!is.null(xlab)) xlab else x_var

  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data[["xv"]], y = .data[["yv"]])) +
    ggplot2::geom_tile(ggplot2::aes(fill = .data[["estimate"]]),
                       color = "white") +
    ggplot2::geom_text(ggplot2::aes(label = .data[["lab"]]),
                       family = "serif", size = 3.5) +
    ggplot2::scale_fill_gradient2(low = "#fd647c", mid = "#e6e6e6",
                                  high = "#3d900e", midpoint = 0) +
    ggplot2::labs(x = x.label, y = y_var, fill = "Subgroup ATE") +
    ggplot2::scale_x_discrete(expand = c(0, 0)) +
    ggplot2::scale_y_discrete(expand = c(0, 0)) +
    .utopia_pdp_theme()

  .as_utopia_plot(p)
}
```

- [ ] **Step 4: Run tests to verify they pass**

Run: `Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-pdp-subgroup.R")'`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add R/plot_pdp.R tests/testthat/test-pdp-subgroup.R
git commit -m "feat(plot_pdp): add 2-way subgroup ATE heatmap with significance marks"
```

---

### Task 5: Documentation, NEWS, and API guard

**Files:**
- Modify: `R/plot_pdp.R` (roxygen block above `plot_pdp`, lines 1-71)
- Modify: `NEWS.md`
- Modify: `tests/testthat/test-fixes-20260511.R` (optional formals guard)
- Regenerate: `man/plot_pdp.Rd`

- [ ] **Step 1: Add the `@param subgroup` roxygen entry**

In `R/plot_pdp.R`, add after the `@param num.threads` block (current lines 38-40):

```r
#' @param subgroup Logical. If `TRUE`, ignore the partial-dependence machinery
#'   and instead plot the doubly-robust (AIPW) subgroup average treatment effect
#'   at each observed value of `x_var` (and, if `y_var` is given, each cell of the
#'   `x_var` x `y_var` cross), via [grf::average_treatment_effect()] with
#'   `subset =`. Intended for binary or low-cardinality integer covariates.
#'   1-way: points with 95% CIs per level. 2-way: a heatmap of subgroup ATEs with
#'   a `*` on cells whose 95% CI excludes 0. Levels are `sort(unique(.))` with no
#'   binning; a level/cell with too few units to estimate is warned and dropped.
#'   When `TRUE`, `grid_size`, `n_max`, `trim`, `show_scatter`, `color.var`,
#'   `color.cat`, `color.lab`, `x.limits`, and `num.threads` are ignored;
#'   `show_ate_region` (1-way reference band), `y.limits`, and `xlab` still apply.
#'   Default `FALSE`.
```

- [ ] **Step 2: Add an example to the roxygen `@examplesIf` block**

In `R/plot_pdp.R`, append to the existing `@examplesIf` block (after current line 71):

```r
#' # Discrete subgroup ATE instead of a PDP curve
#' Xb <- X; Xb[, 1] <- rbinom(n, 1, 0.5)
#' cfb <- causal_forest(Xb, Y, W, num.trees = 100)
#' plot_pdp(cfb, x_var = "X1", subgroup = TRUE)
```

- [ ] **Step 3: Regenerate documentation**

Run: `Rscript -e 'devtools::document()'`
Expected: `man/plot_pdp.Rd` updated with the `subgroup` argument; NAMESPACE unchanged.

- [ ] **Step 4: Add a NEWS entry**

In `NEWS.md`, under `# UtopiaPlanitia (development version)`, add:

```markdown
## `plot_pdp()` gains a discrete subgroup ATE mode

* **New `subgroup` argument (default `FALSE`).** For a binary or low-cardinality
  integer covariate, `subgroup = TRUE` plots the doubly-robust (AIPW) subgroup
  average treatment effect at each observed value of `x_var` — points with 95%
  CIs (1-way), or a heatmap of subgroup ATEs with significance asterisks over the
  `x_var` x `y_var` cross (2-way) — instead of a smooth partial-dependence curve.
  Subgroup ATEs come from `grf::average_treatment_effect(cf, subset = ...)`.
  `subgroup = FALSE` is the default and reproduces existing behavior exactly.
```

- [ ] **Step 5 (optional): Pin the new signature with an API guard**

In `tests/testthat/test-fixes-20260511.R`, add (no `plot_pdp` guard exists today):

```r
test_that("T-API: plot_pdp formals unchanged", {
  fn <- names(formals(plot_pdp))
  expect_setequal(fn, c("c.forest", "x_var", "y_var", "grid_size", "n_max",
                        "show_ate_region", "show_scatter", "trim",
                        "x.limits", "y.limits", "color.var", "color.cat",
                        "color.lab", "xlab", "num.threads", "subgroup"))
})
```

- [ ] **Step 6: Run the full test suite + check docs**

Run: `Rscript -e 'devtools::test(filter = "pdp")'`
Expected: PASS for `test-pdp-subgroup.R` (and `test-fixes-*` if the guard was added).
Run: `Rscript -e 'devtools::document()'` once more — expect no further diff.

- [ ] **Step 7: Commit**

```bash
git add R/plot_pdp.R man/plot_pdp.Rd NEWS.md tests/testthat/test-fixes-20260511.R
git commit -m "docs(plot_pdp): document subgroup mode; NEWS + API guard"
```

---

## Self-Review notes

- **Spec coverage:** trigger/scope (Task 2 dispatch + signature), no-detection level enumeration (Tasks 1 & 4 use `sort(unique(.))`), 1-way points+CI+ATE band (Task 2), 2-way heatmap+`*` (Task 4), robustness warn/skip + all-fail stop (helpers in Tasks 1/4, tested in Task 3), ignored-args + `num.threads` correction (Task 5 roxygen), NEWS/docs/guard (Task 5), no-ggMarginal wrapper (Task 2 `.as_utopia_plot`). All spec sections map to a task.
- **`num.threads`:** confirmed absent from `grf::average_treatment_effect()` formals; ignored in subgroup mode and documented — do not thread it through the helpers.
- **Estimator:** `target.sample = "all"` AIPW default; no estimator args exposed (YAGNI per spec).
- **Theme DRY:** `.utopia_pdp_theme()` is used only by the new helpers; the three existing PDP theme blocks are intentionally left untouched to avoid behavior risk.
