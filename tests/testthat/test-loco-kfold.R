# Tests for loco()'s opt-in K-fold cross-fit path (`cross.fit = TRUE`,
# `num.folds`). Covers: the back-compat bit-identical gate for
# `cross.fit = FALSE` (the single most important scenario here), the new
# error paths (wilcox/OOB incompatibility, num.folds validation), well-formed
# cross-fit output across backends/loss families/group mode, the shared
# metadata/print contract, property-based invariants (midpoint, Bonferroni
# monotonicity, reproducibility), and a value-based pin of the cross-fit
# inference reference (Normal/z, not Student-t at `num.folds - 1` df).
#
# This file is self-sufficient (runs standalone via
# `testthat::test_file("tests/testthat/test-loco-kfold.R")`) -- it defines
# its own skip helpers and DGP/model builders rather than relying on ones
# defined in sibling test files.
#
# Slow, Monte-Carlo-style correctness claims (nominal Type-I / power of the
# cross-fit inference) are validated by a separate simulation study, not by
# this file; see the `UTOPIA_RUN_SLOW_TESTS`-gated convention used elsewhere
# in this suite (e.g. `test-h2-crossfit-typeI.R`) for that class of check.
# All tests here are fast, deterministic, structural/property/value-pin
# checks intended to run in the default `devtools::test()` budget.

skip_if_no_ranger <- function() {
  testthat::skip_if_not_installed("ranger")
}
skip_if_no_grf <- function() {
  testthat::skip_if_not_installed("grf")
}

## Every model fit in this file pins BOTH `num.threads = 1` AND an explicit
## `seed =`. Neither is optional for a reliable test:
##  - `num.threads`: grf/ranger's multi-threaded tree building is only
##    reproducible *within* a fixed thread count, and the default (all-cores)
##    count depends on the host machine, which would make exact-value
##    assertions (the back-compat golden values, the z-reference algebraic
##    pin) flaky across machines/CI.
##  - model `seed`: `loco()` itself calls `set.seed()` before any refit it
##    performs, so refit-derived output (e.g. the split/cross-fit paths) is
##    already deterministic regardless of ambient RNG state. OOB mode,
##    however, scores the *original* (un-refit) fitted model as its
##    baseline -- and that model's own random structure is NOT reset by
##    anything inside `loco()`, since it was fit *before* `loco()` was ever
##    called. Leaving its `seed` unset (relying on ambient `set.seed()`
##    state alone) makes results depend on however much randomness happened
##    to be consumed earlier in the R session -- i.e. on other tests' /
##    files' execution order, a classic self-sufficiency violation.
##    Passing `seed =` explicitly to the model constructor removes that
##    dependency entirely (verified during development: identical output
##    regardless of unrelated RNG draws injected beforehand).
CVK_THREADS <- 1L
CVK_MODEL_SEED <- 1L

## ---- DGP builders -------------------------------------------------------

## Regression DGP: x1/x2 carry signal, x3/x4 are pure noise.
cvk_reg_dgp <- function(n = 300, seed = 101) {
  set.seed(seed)
  x1 <- rnorm(n); x2 <- rnorm(n); x3 <- rnorm(n); x4 <- rnorm(n)
  y  <- x1 + 0.5 * x2 + rnorm(n, sd = 0.5)
  X  <- cbind(x1, x2, x3, x4)
  dat <- data.frame(y = y, x1 = x1, x2 = x2, x3 = x3, x4 = x4)
  list(X = X, y = y, dat = dat)
}

## Binary-probability DGP with the same signal/noise structure.
cvk_prob_dgp <- function(n = 300, seed = 102) {
  set.seed(seed)
  x1 <- rnorm(n); x2 <- rnorm(n); x3 <- rnorm(n); x4 <- rnorm(n)
  y  <- factor(rbinom(n, 1, plogis(1.2 * x1 + 0.6 * x2)))
  X  <- cbind(x1, x2, x3, x4)
  dat <- data.frame(y = y, x1 = x1, x2 = x2, x3 = x3, x4 = x4)
  list(X = X, y = y, dat = dat)
}

## 6-covariate DGP + 3 groups of 2 for group-LOCO; g1 (x1,x2) carries most
## of the signal, g3 (x5,x6) carries a little, g2 (x3,x4) is pure noise.
cvk_group_dgp <- function(n = 300, seed = 103) {
  set.seed(seed)
  X <- matrix(rnorm(n * 6), n, 6)
  colnames(X) <- paste0("x", 1:6)
  y <- X[, 1] + X[, 2] + 0.3 * X[, 5] + rnorm(n, sd = 0.5)
  dat <- as.data.frame(X)
  dat$y <- y
  groups <- list(g1 = c("x1", "x2"), g2 = c("x3", "x4"), g3 = c("x5", "x6"))
  list(X = X, y = y, dat = dat, groups = groups)
}

## Small-n, class-imbalanced DGP for the stratification boundary checks
## (minority class ~10% of n).
cvk_strat_small_dgp <- function(n = 60, seed = 50, minority = 6) {
  set.seed(seed)
  x1 <- rnorm(n); x2 <- rnorm(n); x3 <- rnorm(n); x4 <- rnorm(n)
  y  <- factor(c(rep(1, minority), rep(0, n - minority)))
  X  <- cbind(x1, x2, x3, x4)
  dat <- data.frame(y = y, x1 = x1, x2 = x2, x3 = x3, x4 = x4)
  list(X = X, y = y, dat = dat)
}

## ---- model builders (always seeded + single-threaded; see rationale above) ----

cvk_grf_reg <- function(d, num.trees = 100, seed = CVK_MODEL_SEED) {
  grf::regression_forest(d$X, d$y, num.trees = num.trees,
                         num.threads = CVK_THREADS, seed = seed)
}
cvk_grf_prob <- function(d, num.trees = 100, seed = CVK_MODEL_SEED) {
  grf::probability_forest(d$X, d$y, num.trees = num.trees,
                          num.threads = CVK_THREADS, seed = seed)
}
cvk_ranger_reg <- function(d, num.trees = 100, seed = CVK_MODEL_SEED) {
  ranger::ranger(y ~ ., data = d$dat, num.trees = num.trees,
                 num.threads = CVK_THREADS, seed = seed)
}
cvk_ranger_prob <- function(d, num.trees = 100, seed = CVK_MODEL_SEED) {
  ranger::ranger(y ~ ., data = d$dat, num.trees = num.trees,
                 probability = TRUE, num.threads = CVK_THREADS, seed = seed)
}

## ==========================================================================
## Scenario: regression, per-variable, cross-fit basic well-formedness
## ==========================================================================

test_that("cross-fit regression (grf backend) returns a well-formed loco_vimp with cross-fit metadata", {
  skip_if_no_grf()
  d <- cvk_reg_dgp()
  mod <- cvk_grf_reg(d, num.trees = 200)
  out <- loco(mod, cross.fit = TRUE, num.folds = 5, seed = 1)

  expect_s3_class(out, "loco_vimp")
  expect_named(out$vimp, c("Variable", "Importance", "CI.lower", "CI.upper", "p.value"))
  expect_equal(nrow(out$vimp), 4L)
  expect_setequal(out$vimp$Variable, c("x1", "x2", "x3", "x4"))
  expect_false(anyNA(out$vimp$Importance))
  expect_true(all(out$vimp$CI.lower <= out$vimp$CI.upper))
  expect_identical(out$method, "z")
  expect_true(out$split)
  expect_identical(out$cross.fit, TRUE)
  expect_identical(out$num.folds, 5L)
})

test_that("cross-fit regression (ranger backend) returns a well-formed loco_vimp with cross-fit metadata", {
  skip_if_no_ranger()
  d <- cvk_reg_dgp()
  mod <- cvk_ranger_reg(d, num.trees = 200)
  out <- loco(mod, data = d$dat, cross.fit = TRUE, num.folds = 5, seed = 1)

  expect_s3_class(out, "loco_vimp")
  expect_named(out$vimp, c("Variable", "Importance", "CI.lower", "CI.upper", "p.value"))
  expect_equal(nrow(out$vimp), 4L)
  expect_setequal(out$vimp$Variable, c("x1", "x2", "x3", "x4"))
  expect_false(anyNA(out$vimp$Importance))
  expect_true(all(out$vimp$CI.lower <= out$vimp$CI.upper))
  expect_identical(out$method, "z")
  expect_true(out$split)
  expect_identical(out$cross.fit, TRUE)
  expect_identical(out$num.folds, 5L)
})

## ==========================================================================
## Scenario: probability forest, cross-fit, class-stratified folds
## ==========================================================================

test_that("cross-fit probability forest (grf backend) is well-formed across brier/zero_one/log and reproducible", {
  skip_if_no_grf()
  d <- cvk_prob_dgp()
  mod <- cvk_grf_prob(d, num.trees = 200)

  for (ls in c("brier", "zero_one", "log")) {
    out <- loco(mod, cross.fit = TRUE, num.folds = 5, loss = ls, seed = 1)
    expect_s3_class(out, "loco_vimp")
    expect_named(out$vimp, c("Variable", "Importance", "CI.lower", "CI.upper", "p.value"))
    expect_equal(nrow(out$vimp), 4L)
    expect_true(all(out$vimp$CI.lower <= out$vimp$CI.upper))
    expect_identical(out$cross.fit, TRUE)
    expect_identical(out$num.folds, 5L)
  }

  o1 <- loco(mod, cross.fit = TRUE, num.folds = 5, loss = "brier", seed = 1)
  o2 <- loco(mod, cross.fit = TRUE, num.folds = 5, loss = "brier", seed = 1)
  expect_identical(o1$vimp, o2$vimp)
})

test_that("cross-fit probability forest (ranger, probability = TRUE) is well-formed", {
  skip_if_no_ranger()
  d <- cvk_prob_dgp()
  mod <- cvk_ranger_prob(d, num.trees = 200)
  out <- loco(mod, data = d$dat, cross.fit = TRUE, num.folds = 5,
              loss = "zero_one", seed = 1)

  expect_s3_class(out, "loco_vimp")
  expect_named(out$vimp, c("Variable", "Importance", "CI.lower", "CI.upper", "p.value"))
  expect_true(all(out$vimp$CI.lower <= out$vimp$CI.upper))
  expect_identical(out$cross.fit, TRUE)
})

## ==========================================================================
## Scenario: group-LOCO, cross-fit
## ==========================================================================

test_that("group-LOCO cross-fit returns 3 rows with a Members column matching the supplied groups", {
  skip_if_no_grf()
  d <- cvk_group_dgp()
  mod <- cvk_grf_reg(d, num.trees = 100)
  out <- loco(mod, groups = d$groups, cross.fit = TRUE, num.folds = 5, seed = 1)

  expect_s3_class(out, "loco_vimp")
  expect_named(out$vimp, c("Variable", "Importance", "CI.lower", "CI.upper",
                            "p.value", "Members"))
  expect_equal(nrow(out$vimp), 3L)
  expect_setequal(out$vimp$Variable, c("g1", "g2", "g3"))
  expect_true(all(out$vimp$CI.lower <= out$vimp$CI.upper))
  expect_identical(out$cross.fit, TRUE)

  members <- setNames(out$vimp$Members, out$vimp$Variable)
  expect_setequal(members[["g1"]], d$groups$g1)
  expect_setequal(members[["g2"]], d$groups$g2)
  expect_setequal(members[["g3"]], d$groups$g3)
})

## ==========================================================================
## Scenario: num.folds validation -- error paths
## ==========================================================================

test_that("num.folds = 1 errors (below the minimum of 2)", {
  skip_if_no_grf()
  d <- cvk_reg_dgp()
  mod <- cvk_grf_reg(d, num.trees = 50)
  expect_error(loco(mod, cross.fit = TRUE, num.folds = 1), "num.folds")
})

test_that("num.folds >= n errors", {
  skip_if_no_grf()
  d <- cvk_reg_dgp(n = 300)
  mod <- cvk_grf_reg(d, num.trees = 50)
  expect_error(loco(mod, cross.fit = TRUE, num.folds = 300), "num.folds")
  expect_error(loco(mod, cross.fit = TRUE, num.folds = 400), "num.folds")
})

test_that("num.folds exceeding the minority class size errors under stratification; at-or-below succeeds", {
  skip_if_no_grf()
  ## n = 60, minority class has exactly 6 observations (~10%).
  d <- cvk_strat_small_dgp(n = 60, minority = 6)
  mod <- cvk_grf_prob(d, num.trees = 50)

  expect_error(loco(mod, cross.fit = TRUE, num.folds = 8), "num.folds")
  expect_no_error(loco(mod, cross.fit = TRUE, num.folds = 5, seed = 1))
  ## Boundary: exactly the minority class count must still succeed (one
  ## minority observation per fold, not "exceeding" it).
  expect_no_error(loco(mod, cross.fit = TRUE, num.folds = 6, seed = 1))
})

test_that("valid num.folds (5 and 10) do not error on a regression forest with no class-size constraint", {
  skip_if_no_grf()
  d <- cvk_reg_dgp(n = 300)
  mod <- cvk_grf_reg(d, num.trees = 100)
  expect_no_error(loco(mod, cross.fit = TRUE, num.folds = 5, seed = 1))
  expect_no_error(loco(mod, cross.fit = TRUE, num.folds = 10, seed = 1))
})

## ==========================================================================
## Scenario: method = "wilcox" + cross.fit = TRUE -- error path
## ==========================================================================

test_that("method = 'wilcox' with cross.fit = TRUE errors and points the user to method = 'z'", {
  skip_if_no_grf()
  d <- cvk_reg_dgp()
  mod <- cvk_grf_reg(d, num.trees = 50)
  err <- tryCatch(
    loco(mod, cross.fit = TRUE, method = "wilcox", num.folds = 5),
    error = function(e) conditionMessage(e)
  )
  expect_true(grepl("wilcox", err, ignore.case = TRUE))
  expect_true(grepl("z", err, ignore.case = TRUE))
})

## ==========================================================================
## Scenario: split = FALSE (OOB) + cross.fit = TRUE -- error path
## ==========================================================================

test_that("split = FALSE with cross.fit = TRUE errors instead of silently running OOB or ignoring cross.fit", {
  skip_if_no_grf()
  d <- cvk_reg_dgp()
  mod <- cvk_grf_reg(d, num.trees = 50)
  err <- tryCatch(
    loco(mod, split = FALSE, cross.fit = TRUE),
    error = function(e) conditionMessage(e)
  )
  expect_true(grepl("split", err, ignore.case = TRUE))
})

test_that("when both wilcox and OOB conditions hold, the wilcox error fires first (deterministic ordering)", {
  skip_if_no_grf()
  d <- cvk_reg_dgp()
  mod <- cvk_grf_reg(d, num.trees = 50)
  err <- tryCatch(
    loco(mod, split = FALSE, cross.fit = TRUE, method = "wilcox"),
    error = function(e) conditionMessage(e)
  )
  expect_true(grepl("wilcox", err, ignore.case = TRUE))
})

## ==========================================================================
## Scenario: back-compat regression -- cross.fit = FALSE is bit-identical
## to the pre-feature output. THE SINGLE MOST IMPORTANT SCENARIO IN THIS
## FILE.
##
## Golden values below were captured by checking out the commit immediately
## PRIOR to this feature (the parent of the merge that introduced
## `cross.fit`/`num.folds`), running the exact DGP/seed/backend/method
## combinations through the pre-change `loco()`, and recording `$vimp` at
## full round-trip precision (`sprintf("%.17g", .)`, which -- unlike a plain
## `dput()` at default precision -- is guaranteed to parse back to the exact
## original double). Models are fit with `num.threads = 1` and an explicit
## `seed =` (see `CVK_THREADS`/`CVK_MODEL_SEED` above) so the comparison is
## reproducible regardless of the host machine's core count or what ran
## earlier in the R session. Comparing at `tolerance = 1e-12` rather than
## raw `identical()`/`tolerance = 0` guards against the (extremely unlikely,
## but not provably impossible) case of a last-ULP difference from a
## different grf/ranger/BLAS build on another machine, while remaining many
## orders of magnitude tighter than this suite's existing golden-comparison
## convention (`tolerance = 1e-6` in the GOLD-vs-NEW conformalInference
## block) -- a real regression here would show up at the 1e-2..1e0 scale
## (as demonstrated during development by comparing against a
## Student-t-referenced cross-fit run), not at 1e-12.
## ==========================================================================

test_that("cross.fit = FALSE reproduces the pre-feature output bit-for-bit (grf backend, z/wilcox/OOB)", {
  skip_if_no_grf()
  d <- cvk_reg_dgp()
  mod <- cvk_grf_reg(d, num.trees = 200)

  z_golden <- list(
    Variable   = c("x1", "x2", "x4", "x3"),
    Importance = c(0.29299418707876501, 0.067377482594667637, -0.006823580109320191, -0.019070564834643627),
    CI.lower   = c(0.18641260520584657, 0.025368616756727691, -0.016518097912408155, -0.029837426380393599),
    CI.upper   = c(0.39957576895168345, 0.10938634843260758, 0.0028709376937677738, -0.0083037032888936563),
    p.value    = c(1.4398536526898099e-09, 0.00064888990642777348, 1, 1)
  )
  wilcox_golden <- list(
    Variable   = c("x1", "x2", "x4", "x3"),
    Importance = c(0.29444518423037236, 0.066976440118359376, -0.0073317454460032939, -0.01933735896978573),
    CI.lower   = c(0.17708191338749929, 0.020594860387926558, -0.017501153439336503, -0.030227589405952122),
    CI.upper   = c(0.41180845507324537, 0.11335801984879221, 0.0028376625473299148, -0.0084471285336193414),
    p.value    = c(2.0008807528953833e-07, 0.0028493266411475189, 1, 1)
  )
  oob_golden <- list(
    Variable   = c("x1", "x2", "x4", "x3"),
    Importance = c(0.38815906931602007, 0.090784894578804232, -0.0082827484273579888, -0.020515698867375198)
  )

  z_out <- loco(mod, method = "z", seed = 1)$vimp
  expect_identical(z_out$Variable, z_golden$Variable)
  expect_equal(z_out$Importance, z_golden$Importance, tolerance = 1e-12)
  expect_equal(z_out$CI.lower,   z_golden$CI.lower,   tolerance = 1e-12)
  expect_equal(z_out$CI.upper,   z_golden$CI.upper,   tolerance = 1e-12)
  expect_equal(z_out$p.value,    z_golden$p.value,    tolerance = 1e-12)

  wilcox_out <- loco(mod, method = "wilcox", seed = 1)$vimp
  expect_identical(wilcox_out$Variable, wilcox_golden$Variable)
  expect_equal(wilcox_out$Importance, wilcox_golden$Importance, tolerance = 1e-12)
  expect_equal(wilcox_out$CI.lower,   wilcox_golden$CI.lower,   tolerance = 1e-12)
  expect_equal(wilcox_out$CI.upper,   wilcox_golden$CI.upper,   tolerance = 1e-12)
  expect_equal(wilcox_out$p.value,    wilcox_golden$p.value,    tolerance = 1e-12)

  oob_out <- loco(mod, split = FALSE, seed = 1)$vimp
  expect_identical(oob_out$Variable, oob_golden$Variable)
  expect_equal(oob_out$Importance, oob_golden$Importance, tolerance = 1e-12)
  expect_true(all(is.na(oob_out$CI.lower)))
  expect_true(all(is.na(oob_out$CI.upper)))
  expect_true(all(is.na(oob_out$p.value)))
})

test_that("cross.fit = FALSE reproduces the pre-feature output bit-for-bit (ranger backend, z/wilcox/OOB)", {
  skip_if_no_ranger()
  d <- cvk_reg_dgp()
  mod <- cvk_ranger_reg(d, num.trees = 200)

  z_golden <- list(
    Variable   = c("x1", "x2", "x4", "x3"),
    Importance = c(0.35255122601403166, 0.13757118174192873, -0.011716679209354208, -0.025644199626365154),
    CI.lower   = c(0.23795198888720601, 0.074765297567523856, -0.037669291648866635, -0.054325048061658651),
    CI.upper   = c(0.46715046314085729, 0.20037706591633359, 0.014235933230158218, 0.0030366488089283473),
    p.value    = c(1.0741568515670583e-11, 1.8251502836560684e-06, 1, 1)
  )
  wilcox_golden <- list(
    Variable   = c("x1", "x2", "x4", "x3"),
    Importance = c(0.34360384514514847, 0.1222231808632567, -0.0095698344872051505, -0.018557451659205901),
    CI.lower   = c(0.22040298118392623, 0.058726714716524943, -0.033486976719470388, -0.046119249534507613),
    CI.upper   = c(0.46680470910637073, 0.18571964700998847, 0.014347307745060088, 0.0090043462160958095),
    p.value    = c(4.8496711200406918e-09, 2.5745530541799493e-05, 1, 1)
  )
  oob_golden <- list(
    Variable   = c("x1", "x2", "x4", "x3"),
    Importance = c(0.46573702112417875, 0.13856889489690344, 0.011344591866064013, -0.012097363518332294)
  )

  z_out <- loco(mod, data = d$dat, method = "z", seed = 1)$vimp
  expect_identical(z_out$Variable, z_golden$Variable)
  expect_equal(z_out$Importance, z_golden$Importance, tolerance = 1e-12)
  expect_equal(z_out$CI.lower,   z_golden$CI.lower,   tolerance = 1e-12)
  expect_equal(z_out$CI.upper,   z_golden$CI.upper,   tolerance = 1e-12)
  expect_equal(z_out$p.value,    z_golden$p.value,    tolerance = 1e-12)

  wilcox_out <- loco(mod, data = d$dat, method = "wilcox", seed = 1)$vimp
  expect_identical(wilcox_out$Variable, wilcox_golden$Variable)
  expect_equal(wilcox_out$Importance, wilcox_golden$Importance, tolerance = 1e-12)
  expect_equal(wilcox_out$CI.lower,   wilcox_golden$CI.lower,   tolerance = 1e-12)
  expect_equal(wilcox_out$CI.upper,   wilcox_golden$CI.upper,   tolerance = 1e-12)
  expect_equal(wilcox_out$p.value,    wilcox_golden$p.value,    tolerance = 1e-12)

  oob_out <- loco(mod, data = d$dat, split = FALSE, seed = 1)$vimp
  expect_identical(oob_out$Variable, oob_golden$Variable)
  expect_equal(oob_out$Importance, oob_golden$Importance, tolerance = 1e-12)
  expect_true(all(is.na(oob_out$CI.lower)))
  expect_true(all(is.na(oob_out$CI.upper)))
  expect_true(all(is.na(oob_out$p.value)))
})

test_that("cross.fit = FALSE (default) == cross.fit = FALSE (explicit); new metadata added, nothing removed/renamed", {
  skip_if_no_grf()
  d <- cvk_reg_dgp()
  mod <- cvk_grf_reg(d, num.trees = 100)

  o_default  <- loco(mod, method = "z", seed = 1)
  o_explicit <- loco(mod, method = "z", seed = 1, cross.fit = FALSE)
  expect_identical(o_default$vimp, o_explicit$vimp)

  expect_identical(o_default$cross.fit, FALSE)
  expect_identical(o_default$num.folds, NA_integer_)
  ## Every pre-existing field must still be present.
  expect_true(all(c("vimp", "n", "p", "method", "loss", "split", "alpha",
                    "bonf.correct", "backend", "group") %in% names(o_default)))
})

## ==========================================================================
## Scenario: point-estimate structural sanity (coarse, not a power claim)
## ==========================================================================

test_that("cross-fit importance trends higher/more significant for signal covariates than null covariates", {
  skip_if_no_grf()
  n_sig_gt_null <- 0L
  n_signal_reject <- 0L
  n_null_reject   <- 0L
  reps <- 5L

  for (r in seq_len(reps)) {
    d <- cvk_reg_dgp(n = 1000, seed = 5000 + r)
    mod <- cvk_grf_reg(d, num.trees = 100, seed = r)
    out <- loco(mod, cross.fit = TRUE, num.folds = 5, seed = 1)
    v <- out$vimp
    imp <- setNames(v$Importance, v$Variable)
    pv  <- setNames(v$p.value,    v$Variable)

    if (imp[["x1"]] > imp[["x3"]] && imp[["x1"]] > imp[["x4"]] &&
        imp[["x2"]] > imp[["x3"]] && imp[["x2"]] > imp[["x4"]]) {
      n_sig_gt_null <- n_sig_gt_null + 1L
    }
    n_signal_reject <- n_signal_reject + sum(pv[c("x1", "x2")] < 0.1)
    n_null_reject   <- n_null_reject   + sum(pv[c("x3", "x4")] < 0.1)
  }

  expect_gte(n_sig_gt_null, reps - 1L)
  expect_gt(n_signal_reject, n_null_reject)
})

## ==========================================================================
## Edge cases
## ==========================================================================

test_that("num.folds = 2 (the minimum) succeeds on a moderate n", {
  skip_if_no_grf()
  d <- cvk_reg_dgp(n = 100, seed = 5)
  mod <- cvk_grf_reg(d, num.trees = 100)
  out <- loco(mod, cross.fit = TRUE, num.folds = 2, seed = 1)
  expect_s3_class(out, "loco_vimp")
  expect_identical(out$num.folds, 2L)
})

test_that("num.folds = n - 1 (LOO-like) succeeds without erroring", {
  skip_if_no_grf()
  ## Kept deliberately tiny (n = 20) so num.folds = 19 runs quickly.
  d <- cvk_reg_dgp(n = 20, seed = 20)
  mod <- cvk_grf_reg(d, num.trees = 50)
  expect_no_error(loco(mod, cross.fit = TRUE, num.folds = 19, seed = 1))
})

test_that("a minority class with exactly num.folds observations succeeds (stratification boundary)", {
  skip_if_no_grf()
  d <- cvk_strat_small_dgp(n = 60, minority = 6)
  mod <- cvk_grf_prob(d, num.trees = 50)
  expect_no_error(loco(mod, cross.fit = TRUE, num.folds = 6, seed = 1))
})

test_that("group validation errors fire identically under cross.fit = TRUE (not skipped or deferred)", {
  skip_if_no_grf()
  d <- cvk_group_dgp()
  mod <- cvk_grf_reg(d, num.trees = 50)

  expect_error(
    loco(mod, groups = list(g1 = c("x1", "x2"), g2 = c("x2", "x3")),
         cross.fit = TRUE, num.folds = 5),
    "overlap"
  )
  expect_error(
    loco(mod, groups = list(g1 = character(0)), cross.fit = TRUE, num.folds = 5),
    "non-empty"
  )
  expect_error(
    loco(mod, groups = list(g1 = paste0("x", 1:6)), cross.fit = TRUE, num.folds = 5),
    "zero predictors"
  )
})

## ==========================================================================
## Property-based invariants
## ==========================================================================

test_that("midpoint invariant holds exactly for cross-fit output", {
  skip_if_no_grf()
  d <- cvk_reg_dgp()
  mod <- cvk_grf_reg(d, num.trees = 100)
  out <- loco(mod, cross.fit = TRUE, num.folds = 5, seed = 1)
  expect_equal(out$vimp$Importance, (out$vimp$CI.lower + out$vimp$CI.upper) / 2)
})

test_that("Bonferroni correction is monotone (never smaller p-value / narrower CI when off) under cross-fit", {
  skip_if_no_grf()
  d <- cvk_reg_dgp()
  mod <- cvk_grf_reg(d, num.trees = 100)

  o_corr   <- loco(mod, cross.fit = TRUE, num.folds = 5, seed = 1, bonf.correct = TRUE)
  o_uncorr <- loco(mod, cross.fit = TRUE, num.folds = 5, seed = 1, bonf.correct = FALSE)
  v_corr   <- o_corr$vimp[order(o_corr$vimp$Variable), ]
  v_uncorr <- o_uncorr$vimp[order(o_uncorr$vimp$Variable), ]

  expect_true(all(v_corr$p.value >= v_uncorr$p.value - 1e-12))
  width_corr   <- v_corr$CI.upper   - v_corr$CI.lower
  width_uncorr <- v_uncorr$CI.upper - v_uncorr$CI.lower
  expect_true(all(width_corr >= width_uncorr - 1e-12))
})

test_that("cross-fit output is reproducible under a fixed seed and sensitive to a changed seed", {
  skip_if_no_grf()
  d <- cvk_reg_dgp()
  mod <- cvk_grf_reg(d, num.trees = 100)

  o1 <- loco(mod, cross.fit = TRUE, num.folds = 5, seed = 7)
  o2 <- loco(mod, cross.fit = TRUE, num.folds = 5, seed = 7)
  o3 <- loco(mod, cross.fit = TRUE, num.folds = 5, seed = 8)

  expect_identical(o1$vimp, o2$vimp)
  expect_false(isTRUE(identical(o1$vimp, o3$vimp)))
})

test_that("grf and ranger cross-fit outputs share identical $vimp column names and metadata field names", {
  skip_if_no_grf()
  skip_if_no_ranger()
  d <- cvk_reg_dgp()
  grf_mod    <- cvk_grf_reg(d, num.trees = 100)
  ranger_mod <- cvk_ranger_reg(d, num.trees = 100)

  out_grf    <- loco(grf_mod, cross.fit = TRUE, num.folds = 5, seed = 1)
  out_ranger <- loco(ranger_mod, data = d$dat, cross.fit = TRUE, num.folds = 5, seed = 1)

  expect_identical(names(out_grf$vimp), names(out_ranger$vimp))
  expect_identical(names(out_grf), names(out_ranger))
  expect_identical(vapply(out_grf$vimp, class, character(1)),
                    vapply(out_ranger$vimp, class, character(1)))
})

## ==========================================================================
## z-reference pin: the cross-fit inference reference is Normal (z), not
## Student-t at num.folds - 1 df.
## ==========================================================================

test_that(".nb_ttest(reference = 'z') differs from reference = 't' in the p-value only, holding NB SE fixed", {
  ## Synthetic per-fold statistics for 2 targets across 5 folds -- fully
  ## independent of any model fit, so this isolates the shared NB-aggregation
  ## helper's contract directly.
  Psi <- matrix(
    c(0.10, 0.12, 0.08, 0.09, 0.11,
      0.05, -0.06, 0.055, -0.045, 0.05),
    nrow = 5, ncol = 2, dimnames = list(NULL, c("A", "B"))
  )
  n1 <- rep(240L, 5); n2 <- rep(60L, 5)

  agg_z <- UtopiaPlanitia:::.nb_ttest(Psi, n1, n2, reference = "z")
  agg_t <- UtopiaPlanitia:::.nb_ttest(Psi, n1, n2, reference = "t")

  ## Same SE/test-statistic/df regardless of reference -- only the
  ## reference distribution used for the p-value differs.
  expect_identical(agg_z$se, agg_t$se)
  expect_identical(agg_z$t,  agg_t$t)
  expect_identical(agg_z$df, agg_t$df)

  ## Each reference must use its own closed-form formula exactly.
  expect_equal(agg_z$pval, stats::pnorm(agg_z$t, lower.tail = FALSE))
  expect_equal(agg_t$pval, stats::pt(agg_t$t, df = agg_t$df, lower.tail = FALSE))

  ## The two references give meaningfully different p-values here (this is
  ## not a tautology of the shared SE formula) -- if a future edit silently
  ## reverted the reference distinction, this would catch it.
  expect_true(all(abs(agg_z$pval - agg_t$pval) > 3e-4))
})

test_that("loco(cross.fit = TRUE)'s p.value/CI imply a Normal reference, not Student-t at num.folds - 1 df", {
  skip_if_no_grf()
  ## bonf.correct = FALSE keeps alpha_eff == alpha, so no need to know G here.
  d <- cvk_reg_dgp()
  mod <- cvk_grf_reg(d, num.trees = 100)
  alpha <- 0.1
  out <- loco(mod, cross.fit = TRUE, num.folds = 5, bonf.correct = FALSE,
              alpha = alpha, seed = 1)
  v <- out$vimp

  ## For a one-sided test against a *symmetric* reference distribution F,
  ## the p-value and the (symmetric, two-sided) CI half-width are both
  ## deterministic functions of the same (Importance, SE) pair, related by
  ## qF(1 - p) == Importance * qF(1 - alpha/2) / half. Checking this with
  ## F = Normal is discriminating: it holds only when BOTH the p-value and
  ## the CI were actually built against a Normal reference. Under the
  ## pre-fix Student-t(num.folds - 1 = 4) reference this identity fails by
  ## a wide margin (verified by hand against the pre-fix implementation
  ## during development), since qnorm() != qt(df = 4) in general.
  for (i in seq_len(nrow(v))) {
    half <- v$CI.upper[i] - v$Importance[i]
    lhs <- stats::qnorm(1 - v$p.value[i])
    rhs <- v$Importance[i] * stats::qnorm(1 - alpha / 2) / half
    expect_equal(lhs, rhs, tolerance = 1e-6, info = v$Variable[i])
  }
})

## ==========================================================================
## print() header contract
## ==========================================================================

test_that("print.loco_vimp header notes cross-fit and num.folds only when cross.fit = TRUE", {
  skip_if_no_grf()
  d <- cvk_reg_dgp()
  mod <- cvk_grf_reg(d, num.trees = 100)

  out_cv  <- loco(mod, cross.fit = TRUE, num.folds = 5, seed = 1)
  out_def <- loco(mod, method = "z", seed = 1)

  txt_cv  <- capture.output(print(out_cv))
  txt_def <- capture.output(print(out_def))

  expect_true(any(grepl("cross-fit", txt_cv, ignore.case = TRUE)))
  expect_true(any(grepl("5", txt_cv, fixed = TRUE)))
  expect_false(any(grepl("cross-fit", txt_def, ignore.case = TRUE)))

  testthat::skip_if_not_installed("ggplot2")
  g <- plot(out_cv)
  expect_s3_class(g, "ggplot")
})
