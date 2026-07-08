# Tests for loco() — covers correctness audit fixes, the method = "z" /
# "wilcox" interface, the loss / classification feature, and group-LOCO.

skip_if_no_ranger <- function() {
  testthat::skip_if_not_installed("ranger")
}
skip_if_no_conformal <- function() {
  testthat::skip_if_not_installed("conformalInference")
}

make_reg_model <- function(n = 80, seed = 1, trees = 50) {
  set.seed(seed)
  dat <- data.frame(
    y  = rnorm(n),
    x1 = rnorm(n),
    x2 = rnorm(n),
    x3 = rnorm(n)
  )
  dat$y <- dat$x1 + 0.5 * dat$x2 + rnorm(n, sd = 0.5)
  list(dat = dat,
       mod = ranger::ranger(y ~ ., data = dat, num.trees = trees))
}

make_prob_model <- function(n = 120, seed = 2, trees = 60) {
  set.seed(seed)
  dat <- data.frame(x1 = rnorm(n), x2 = rnorm(n), x3 = rnorm(n))
  p <- plogis(1.2 * dat$x1 + 0.6 * dat$x2)
  dat$y <- factor(rbinom(n, 1, p))
  list(dat = dat,
       mod = ranger::ranger(y ~ ., data = dat, num.trees = trees,
                            probability = TRUE))
}

make_clf_model <- function(n = 120, seed = 2, trees = 60) {
  set.seed(seed)
  dat <- data.frame(x1 = rnorm(n), x2 = rnorm(n), x3 = rnorm(n))
  p <- plogis(1.2 * dat$x1 + 0.6 * dat$x2)
  dat$y <- factor(rbinom(n, 1, p))
  list(dat = dat,
       mod = ranger::ranger(y ~ ., data = dat, num.trees = trees))
}

make_multi_prob_model <- function(n = 150, seed = 3, trees = 60) {
  set.seed(seed)
  dat <- data.frame(x1 = rnorm(n), x2 = rnorm(n),
                    x3 = rnorm(n), x4 = rnorm(n))
  score1 <- dat$x1 + 0.5 * dat$x2
  score2 <- -0.5 * dat$x1 + dat$x2
  score3 <- 0.1 * dat$x3
  expS <- exp(cbind(score1, score2, score3))
  P <- expS / rowSums(expS)
  dat$y <- factor(apply(P, 1, function(p) sample(c("A","B","C"), 1, prob = p)))
  list(dat = dat,
       mod = ranger::ranger(y ~ ., data = dat, num.trees = trees,
                            probability = TRUE))
}

# ---- Existing tests (updated for `loss` column) ------------------------

test_that("OOB mode returns the expected data-frame shape", {
  skip_if_no_ranger()
  bundle <- make_reg_model()
  out <- loco(bundle$mod, data = bundle$dat, split = FALSE, seed = 1)
  expect_s3_class(out, "loco_vimp")
  expect_named(out$vimp, c("Variable", "Importance", "CI.lower", "CI.upper", "p.value"))
  expect_setequal(out$vimp$Variable, c("x1", "x2", "x3"))
  expect_identical(out$method, "oob")
  expect_identical(out$loss, "abs")
  expect_equal(out$vimp$Importance, sort(out$vimp$Importance, decreasing = TRUE))
})

test_that("OOB mode is reproducible with the same seed", {
  skip_if_no_ranger()
  bundle <- make_reg_model()
  o1 <- loco(bundle$mod, data = bundle$dat, split = FALSE, seed = 42)
  o2 <- loco(bundle$mod, data = bundle$dat, split = FALSE, seed = 42)
  o1s <- o1$vimp[order(o1$vimp$Variable), ]
  o2s <- o2$vimp[order(o2$vimp$Variable), ]
  expect_equal(o1s$Importance, o2s$Importance)
  o3 <- loco(bundle$mod, data = bundle$dat, split = FALSE, seed = 999)
  o3s <- o3$vimp[order(o3$vimp$Variable), ]
  expect_false(isTRUE(all.equal(o1s$Importance, o3s$Importance)))
})

test_that("OOB mode handles extra non-predictor columns in `data`", {
  skip_if_no_ranger()
  set.seed(2)
  dat <- data.frame(
    y  = rnorm(80), x1 = rnorm(80), x2 = rnorm(80), x3 = rnorm(80),
    id = seq_len(80), wt = runif(80)
  )
  dat$y <- dat$x1 + rnorm(80, sd = 0.5)
  mod <- ranger::ranger(y ~ x1 + x2 + x3, data = dat, num.trees = 50)
  out <- loco(mod, split = FALSE, seed = 1)
  expect_s3_class(out, "loco_vimp")
  expect_setequal(out$vimp$Variable, c("x1", "x2", "x3"))
})

test_that("loco() rejects survival forests", {
  skip_if_no_ranger()
  ## A minimal survival fit; skip if Surv() not available
  if (!requireNamespace("survival", quietly = TRUE)) {
    testthat::skip("survival not available")
  }
  set.seed(3)
  dat <- data.frame(
    time = rexp(80), status = sample(0:1, 80, replace = TRUE),
    x1 = rnorm(80), x2 = rnorm(80)
  )
  mod <- ranger::ranger(survival::Surv(time, status) ~ x1 + x2,
                        data = dat, num.trees = 30)
  expect_error(loco(mod, split = FALSE, seed = 1),
               "does not support survival forests",
               fixed = TRUE)
})

test_that("loco() requires at least two predictors", {
  skip_if_no_ranger()
  set.seed(4)
  dat <- data.frame(y = rnorm(60), x1 = rnorm(60))
  mod <- ranger::ranger(y ~ x1, data = dat, num.trees = 50)
  expect_error(loco(mod, split = FALSE, seed = 1),
               "at least two predictors",
               fixed = TRUE)
})

test_that("OOB mode accepts factor predictors (ranger handles them natively)", {
  skip_if_no_ranger()
  set.seed(5)
  dat <- data.frame(
    y  = rnorm(80), x1 = rnorm(80),
    x2 = factor(sample(letters[1:3], 80, replace = TRUE))
  )
  mod <- ranger::ranger(y ~ ., data = dat, num.trees = 50)
  out <- loco(mod, split = FALSE, seed = 1)
  expect_setequal(out$vimp$Variable, c("x1", "x2"))
})

test_that("split mode rejects factor predictors with a clear message", {
  skip_if_no_ranger()
  set.seed(6)
  dat <- data.frame(
    y  = rnorm(80), x1 = rnorm(80),
    x2 = factor(sample(letters[1:3], 80, replace = TRUE))
  )
  mod <- ranger::ranger(y ~ ., data = dat, num.trees = 50)
  expect_error(loco(mod, split = TRUE, seed = 1),
               "does not support factor predictors",
               fixed = TRUE)
})

test_that("hyperparameter symbols evaluate in caller environment", {
  skip_if_no_ranger()
  set.seed(7)
  dat <- data.frame(y = rnorm(80), x1 = rnorm(80), x2 = rnorm(80), x3 = rnorm(80))
  ntrees_local <- 40L
  mod <- ranger::ranger(y ~ ., data = dat, num.trees = ntrees_local)
  out <- loco(mod, split = FALSE, seed = 1)
  expect_s3_class(out, "loco_vimp")
  expect_setequal(out$vimp$Variable, c("x1", "x2", "x3"))
})

test_that("core hyperparameters survive being defined inside a function", {
  skip_if_no_ranger()
  set.seed(8)
  dat_outer <- data.frame(y = rnorm(80), x1 = rnorm(80),
                          x2 = rnorm(80), x3 = rnorm(80))
  fit_in_fn <- function(d) {
    ntrees_gone <- 40L
    ranger::ranger(y ~ ., data = d, num.trees = ntrees_gone)
  }
  mod <- fit_in_fn(dat_outer)
  out <- loco(mod, data = dat_outer, split = FALSE, seed = 1)
  expect_setequal(out$vimp$Variable, c("x1", "x2", "x3"))
})

test_that("uncommon hyperparameter symbols that can't be resolved warn but continue", {
  skip_if_no_ranger()
  set.seed(9)
  dat_outer <- data.frame(y = rnorm(80), x1 = rnorm(80),
                          x2 = rnorm(80), x3 = rnorm(80))
  fit_in_fn <- function(d) {
    sf_gone <- 0.7
    ranger::ranger(y ~ ., data = d, num.trees = 40,
                   sample.fraction = sf_gone, replace = FALSE)
  }
  mod <- fit_in_fn(dat_outer)
  expect_warning(
    out <- loco(mod, data = dat_outer, split = FALSE, seed = 1),
    "ignoring ranger argument"
  )
  expect_setequal(out$vimp$Variable, c("x1", "x2", "x3"))
})

test_that("x/y interface without dependent.variable.name errors clearly", {
  skip_if_no_ranger()
  set.seed(9)
  X <- matrix(rnorm(80 * 3), 80, 3, dimnames = list(NULL, c("x1","x2","x3")))
  y <- X[, 1] + rnorm(80, sd = 0.5)
  mod <- ranger::ranger(x = X, y = y, num.trees = 50)
  dat <- data.frame(y = y, X)
  expect_error(loco(mod, data = dat, split = FALSE, seed = 1),
               "Cannot determine the response variable name",
               fixed = TRUE)
})

test_that("x/y interface works when dependent.variable.name is supplied", {
  skip_if_no_ranger()
  set.seed(10)
  X <- matrix(rnorm(80 * 3), 80, 3, dimnames = list(NULL, c("x1","x2","x3")))
  y <- X[, 1] + rnorm(80, sd = 0.5)
  mod <- ranger::ranger(x = X, y = y, num.trees = 50,
                        dependent.variable.name = "y")
  dat <- data.frame(y = y, X)
  out <- loco(mod, data = dat, split = FALSE, seed = 1)
  expect_setequal(out$vimp$Variable, c("x1", "x2", "x3"))
})

test_that("split mode returns the expected columns and method tag", {
  skip_if_no_ranger()
  bundle <- make_reg_model(n = 120, trees = 100)
  out <- suppressWarnings(loco(bundle$mod, data = bundle$dat,
                               split = TRUE, seed = 1, method = "z"))
  expect_s3_class(out, "loco_vimp")
  expect_named(out$vimp, c("Variable", "Importance",
                      "CI.lower", "CI.upper", "p.value"))
  expect_identical(out$method, "z")
  expect_identical(out$loss, "abs")
  expect_true(all(out$vimp$CI.lower <= out$vimp$CI.upper))
  expect_equal(out$vimp$Importance, (out$vimp$CI.lower + out$vimp$CI.upper) / 2)
  expect_true(all(out$vimp$p.value >= 0 & out$vimp$p.value <= 1))
})

test_that("split mode supports method = 'wilcox'", {
  skip_if_no_ranger()
  bundle <- make_reg_model(n = 120, trees = 100)
  out <- suppressWarnings(loco(bundle$mod, data = bundle$dat,
                               split = TRUE, seed = 1, method = "wilcox"))
  expect_identical(out$method, "wilcox")
  expect_named(out$vimp, c("Variable", "Importance",
                      "CI.lower", "CI.upper", "p.value"))
})

test_that("bonf.correct = TRUE produces p-values at least as large as raw", {
  skip_if_no_ranger()
  bundle <- make_reg_model(n = 120, trees = 100)
  oA <- suppressWarnings(loco(bundle$mod, data = bundle$dat, split = TRUE,
                              seed = 1, method = "z", bonf.correct = TRUE))
  oB <- suppressWarnings(loco(bundle$mod, data = bundle$dat, split = TRUE,
                              seed = 1, method = "z", bonf.correct = FALSE))
  vA <- oA$vimp[order(oA$vimp$Variable), ]
  vB <- oB$vimp[order(oB$vimp$Variable), ]
  expect_true(all(vA$p.value >= vB$p.value - 1e-12))
})

test_that("alpha controls CI width monotonically", {
  skip_if_no_ranger()
  bundle <- make_reg_model(n = 120, trees = 100)
  w_wide   <- suppressWarnings(loco(bundle$mod, data = bundle$dat, split = TRUE,
                                    seed = 1, method = "z", alpha = 0.20))
  w_narrow <- suppressWarnings(loco(bundle$mod, data = bundle$dat, split = TRUE,
                                    seed = 1, method = "z", alpha = 0.01))
  v_wide   <- w_wide$vimp[order(w_wide$vimp$Variable), ]
  v_narrow <- w_narrow$vimp[order(w_narrow$vimp$Variable), ]
  width_wide   <- v_wide$CI.upper - v_wide$CI.lower
  width_narrow <- v_narrow$CI.upper - v_narrow$CI.lower
  expect_true(all(width_narrow >= width_wide - 1e-12))
})

test_that("invalid `method` is rejected by match.arg", {
  skip_if_no_ranger()
  bundle <- make_reg_model()
  expect_error(loco(bundle$mod, data = bundle$dat, split = TRUE, method = "bogus"))
})

test_that("invalid `alpha` is rejected", {
  skip_if_no_ranger()
  bundle <- make_reg_model()
  expect_error(loco(bundle$mod, data = bundle$dat, alpha = -0.1))
  expect_error(loco(bundle$mod, data = bundle$dat, alpha = 1.0))
})

# ---- New: Classification DV support ------------------------------------

test_that("probability forest, OOB, auto-detects brier loss", {
  skip_if_no_ranger()
  b <- make_prob_model()
  out <- loco(b$mod, data = b$dat, split = FALSE, seed = 1)
  expect_named(out$vimp, c("Variable", "Importance", "CI.lower", "CI.upper", "p.value"))
  expect_identical(out$loss, "brier")
  expect_setequal(out$vimp$Variable, c("x1","x2","x3"))
  ## x1 should beat x3 in importance
  i <- setNames(out$vimp$Importance, out$vimp$Variable)
  expect_gt(i["x1"], i["x3"])
})

test_that("probability forest, OOB, supports zero_one and log losses", {
  skip_if_no_ranger()
  b <- make_prob_model()
  out_zo <- loco(b$mod, data = b$dat, split = FALSE, seed = 1, loss = "zero_one")
  out_lg <- loco(b$mod, data = b$dat, split = FALSE, seed = 1, loss = "log")
  expect_identical(out_zo$loss, "zero_one")
  expect_identical(out_lg$loss, "log")
  expect_setequal(out_zo$vimp$Variable, c("x1","x2","x3"))
})

test_that("classification forest, OOB, auto-detects zero_one loss", {
  skip_if_no_ranger()
  b <- make_clf_model()
  out <- loco(b$mod, data = b$dat, split = FALSE, seed = 1)
  expect_identical(out$loss, "zero_one")
  expect_setequal(out$vimp$Variable, c("x1","x2","x3"))
})

test_that("classification forest rejects brier and log losses", {
  skip_if_no_ranger()
  b <- make_clf_model()
  expect_error(loco(b$mod, data = b$dat, split = FALSE, loss = "brier"),
               "not valid for a .hard. classification")
  expect_error(loco(b$mod, data = b$dat, split = FALSE, loss = "log"),
               "not valid for a .hard. classification")
})

test_that("regression forest rejects brier / zero_one / log", {
  skip_if_no_ranger()
  bundle <- make_reg_model()
  expect_error(loco(bundle$mod, data = bundle$dat, split = FALSE, loss = "brier"),
               "not valid for a regression forest")
  expect_error(loco(bundle$mod, data = bundle$dat, split = FALSE, loss = "zero_one"),
               "not valid for a regression forest")
  expect_error(loco(bundle$mod, data = bundle$dat, split = FALSE, loss = "log"),
               "not valid for a regression forest")
})

test_that("probability forest rejects 'abs' and 'mse' losses", {
  skip_if_no_ranger()
  b <- make_prob_model()
  expect_error(loco(b$mod, data = b$dat, split = FALSE, loss = "abs"),
               "not valid for a probability forest")
  expect_error(loco(b$mod, data = b$dat, split = FALSE, loss = "mse"),
               "not valid for a probability forest")
})

test_that("probability forest, split mode, brier", {
  skip_if_no_ranger()
  b <- make_prob_model(n = 160, trees = 80)
  out <- loco(b$mod, data = b$dat, split = TRUE, seed = 1)
  expect_named(out$vimp, c("Variable","Importance","CI.lower","CI.upper",
                      "p.value"))
  expect_identical(out$loss, "brier")
  expect_true(all(out$vimp$CI.lower <= out$vimp$CI.upper))
  expect_true(all(out$vimp$p.value >= 0 & out$vimp$p.value <= 1))
})

test_that("classification forest, split mode, zero_one", {
  skip_if_no_ranger()
  b <- make_clf_model(n = 160, trees = 80)
  out <- loco(b$mod, data = b$dat, split = TRUE, seed = 1)
  expect_identical(out$loss, "zero_one")
  expect_identical(out$method, "z")
  expect_true(all(out$vimp$CI.lower <= out$vimp$CI.upper))
})

test_that("multi-class probability forest works with all loss choices", {
  skip_if_no_ranger()
  b <- make_multi_prob_model(n = 150, trees = 60)
  for (lo in c("brier", "zero_one", "log")) {
    out <- loco(b$mod, data = b$dat, split = FALSE, seed = 1, loss = lo)
    expect_identical(out$loss, lo)
    expect_setequal(out$vimp$Variable, c("x1","x2","x3","x4"))
  }
})

test_that("MSE loss on regression forest produces a result and differs from abs", {
  skip_if_no_ranger()
  bundle <- make_reg_model(n = 120, trees = 60)
  o_abs <- loco(bundle$mod, data = bundle$dat, split = FALSE,
                seed = 1, loss = "abs")
  o_mse <- loco(bundle$mod, data = bundle$dat, split = FALSE,
                seed = 1, loss = "mse")
  expect_identical(o_mse$loss, "mse")
  expect_identical(o_abs$loss, "abs")
  expect_setequal(o_abs$vimp$Variable, o_mse$vimp$Variable)
  oa <- o_abs$vimp[order(o_abs$vimp$Variable), ]
  om <- o_mse$vimp[order(o_mse$vimp$Variable), ]
  ## They should differ (MSE inflates large residuals)
  expect_false(isTRUE(all.equal(oa$Importance, om$Importance)))
})

# ---- New: Group-LOCO ----------------------------------------------------

test_that("group-LOCO OOB returns one row per group, with members column", {
  skip_if_no_ranger()
  bundle <- make_reg_model(n = 100, trees = 60)
  out <- loco(bundle$mod, data = bundle$dat, split = FALSE, seed = 1,
              groups = list(g_x12 = c("x1","x2"), g_x3 = "x3"))
  expect_named(out$vimp, c("Variable", "Importance", "CI.lower", "CI.upper",
                      "p.value", "Members"))
  expect_identical(out$method, "oob")
  expect_identical(out$loss, "abs")
  expect_setequal(out$vimp$Variable, c("g_x12", "g_x3"))
  expect_equal(length(out$vimp$Members), 2L)
  ## The combined importance of (x1,x2) should exceed x3 alone.
  i <- setNames(out$vimp$Importance, out$vimp$Variable)
  expect_gt(i["g_x12"], i["g_x3"])
})

test_that("group-LOCO split returns CI / p.value with members", {
  skip_if_no_ranger()
  bundle <- make_reg_model(n = 160, trees = 80)
  out <- loco(bundle$mod, data = bundle$dat, split = TRUE, seed = 1,
              groups = list(g_x12 = c("x1","x2"), g_x3 = "x3"))
  expect_named(out$vimp, c("Variable","Importance","CI.lower","CI.upper",
                      "p.value","Members"))
  expect_setequal(out$vimp$Variable, c("g_x12", "g_x3"))
  expect_true(all(out$vimp$CI.lower <= out$vimp$CI.upper))
})

test_that("singleton groups in pred-name order equal per-variable LOCO (OOB)", {
  skip_if_no_ranger()
  bundle <- make_reg_model(n = 120, trees = 80)
  per_var <- loco(bundle$mod, data = bundle$dat, split = FALSE, seed = 1)
  singletons <- loco(bundle$mod, data = bundle$dat, split = FALSE, seed = 1,
                     groups = list(x1 = "x1", x2 = "x2", x3 = "x3"))
  per_var_v <- per_var$vimp[order(per_var$vimp$Variable), ]
  singletons_v <- singletons$vimp[order(singletons$vimp$Variable), ]
  expect_equal(singletons_v$Importance, per_var_v$Importance, tolerance = 1e-8)
})

test_that("character-vector groups arg becomes single unnamed group", {
  skip_if_no_ranger()
  bundle <- make_reg_model(n = 80, trees = 50)
  out <- loco(bundle$mod, data = bundle$dat, split = FALSE, seed = 1,
              groups = c("x1","x2"))
  expect_equal(nrow(out$vimp), 1L)
  expect_equal(out$vimp$Variable, "group1")
  expect_equal(out$vimp$Members[[1]], c("x1","x2"))
})

test_that("group-LOCO validation: unknown member name errors", {
  skip_if_no_ranger()
  bundle <- make_reg_model(n = 60, trees = 30)
  expect_error(
    loco(bundle$mod, data = bundle$dat, split = FALSE,
         groups = list(g1 = c("x1","not_a_predictor"))),
    "reference predictor name"
  )
})

test_that("group-LOCO validation: overlapping members errors", {
  skip_if_no_ranger()
  bundle <- make_reg_model(n = 60, trees = 30)
  expect_error(
    loco(bundle$mod, data = bundle$dat, split = FALSE,
         groups = list(g1 = c("x1","x2"), g2 = c("x2","x3"))),
    "must not overlap"
  )
})

test_that("group-LOCO validation: empty group errors", {
  skip_if_no_ranger()
  bundle <- make_reg_model(n = 60, trees = 30)
  expect_error(
    loco(bundle$mod, data = bundle$dat, split = FALSE,
         groups = list(g1 = character(0))),
    "non-empty"
  )
})

test_that("group-LOCO validation: dropping all predictors errors", {
  skip_if_no_ranger()
  bundle <- make_reg_model(n = 60, trees = 30)
  expect_error(
    loco(bundle$mod, data = bundle$dat, split = FALSE,
         groups = list(g1 = c("x1","x2","x3"))),
    "would leave zero predictors"
  )
})

test_that("group-LOCO validation: duplicated group names errors", {
  skip_if_no_ranger()
  bundle <- make_reg_model(n = 60, trees = 30)
  expect_error(
    loco(bundle$mod, data = bundle$dat, split = FALSE,
         groups = list(g1 = "x1", g1 = "x2")),
    "duplicated names"
  )
})

test_that("group-LOCO on probability forest with brier loss works", {
  skip_if_no_ranger()
  b <- make_prob_model(n = 140, trees = 60)
  out <- loco(b$mod, data = b$dat, split = FALSE, seed = 1,
              groups = list(g_signal = c("x1","x2"), g_noise = "x3"))
  expect_named(out$vimp, c("Variable","Importance","CI.lower","CI.upper",
                      "p.value","Members"))
  expect_identical(out$loss, "brier")
  i <- setNames(out$vimp$Importance, out$vimp$Variable)
  expect_gt(i["g_signal"], i["g_noise"])
})

test_that("group-LOCO + classification + split mode is wired end-to-end", {
  skip_if_no_ranger()
  b <- make_prob_model(n = 160, trees = 60)
  out <- loco(b$mod, data = b$dat, split = TRUE, seed = 1,
              groups = list(g_signal = c("x1","x2"), g_noise = "x3"))
  expect_named(out$vimp, c("Variable","Importance","CI.lower","CI.upper",
                      "p.value","Members"))
  expect_identical(out$loss, "brier")
  expect_true(all(out$vimp$CI.lower <= out$vimp$CI.upper))
})

# ---- GOLD-vs-NEW verification: conformalInference migration ------------
#
# Permanent regression coverage for the removal of the conformalInference
# hard-path from loco()'s split-mode code (per-variable, ranger, regression,
# loss = "abs"). GOLD = conformalInference::loco() run as an independent
# oracle (recipe below mirrors, using only conformalInference's own public
# entry point plus the fitted ranger model's own hyperparameters, the exact
# fit/predict wiring loco() used pre-migration). NEW = the migrated
# loco_custom_split() fallthrough path exercised via the public loco().
#
# Tolerances/assertions are taken verbatim from this run's test-spec.md
# (2026-07-06-loco-drop-conformal) -- do not widen without re-deriving them.

make_fake_gold_data <- function() {
  set.seed(20260706)
  n <- 150
  dat <- data.frame(
    x1 = rnorm(n), x2 = rnorm(n), x3 = rnorm(n), x4 = rnorm(n), x5 = rnorm(n)
  )
  dat$y <- 2 * dat$x1 - 1.5 * dat$x2 + rnorm(n, sd = 1)
  mod <- ranger::ranger(y ~ ., data = dat, num.trees = 100)
  list(dat = dat, mod = mod,
       pred.names = c("x1", "x2", "x3", "x4", "x5"), resp.name = "y")
}

make_real_gold_data <- function() {
  predictors_real <- c("cyl", "disp", "hp", "drat", "wt", "qsec")
  dat <- datasets::mtcars[, c("mpg", predictors_real)]
  mod <- ranger::ranger(
    mpg ~ cyl + disp + hp + drat + wt + qsec,
    data = dat, num.trees = 100
  )
  list(dat = dat, mod = mod, pred.names = predictors_real, resp.name = "mpg")
}

## GOLD recipe (test-spec.md Sec 2.3): calls conformalInference::loco()
## directly, wiring train.fun/predict.fun/active.fun from the fitted
## ranger model's own hyperparameters -- no access to package internals.
make_gold_loco_raw <- function(bundle, seed, alpha, bonf.correct) {
  mod <- bundle$mod
  hp.args <- list(
    num.trees     = mod$num.trees,
    mtry          = mod$mtry,
    min.node.size = mod$min.node.size,
    splitrule     = mod$splitrule,
    replace       = mod$replace,
    max.depth     = mod$max.depth,
    importance    = "none"
  )

  train.fun <- function(x, y, out = NULL) {
    args <- hp.args
    args$x <- x
    args$y <- y
    if (!is.null(args$mtry)) args$mtry <- min(as.integer(args$mtry), ncol(x))
    do.call(ranger::ranger, args)
  }
  predict.fun <- function(out, newx) {
    stats::predict(out, data = as.data.frame(newx))$predictions
  }
  active.fun <- function(out) {
    list(seq_len(length(out$forest$independent.variable.names)))
  }

  x <- as.matrix(bundle$dat[, bundle$pred.names, drop = FALSE])
  y <- as.numeric(bundle$dat[[bundle$resp.name]])

  conformalInference::loco(
    x, y,
    train.fun = train.fun, predict.fun = predict.fun, active.fun = active.fun,
    alpha = alpha, bonf.correct = bonf.correct, seed = seed, verbose = FALSE
  )
}

gold_frame <- function(gold_raw, pred.names, method = c("z", "wilcox")) {
  method <- match.arg(method)
  inf <- switch(method, z = gold_raw$inf.z[[1L]], wilcox = gold_raw$inf.wilcox[[1L]])
  vars <- pred.names[gold_raw$active[[1L]]]
  out <- data.frame(
    variable   = vars,
    importance = as.numeric((inf[, "LowConfPt"] + inf[, "UpConfPt"]) / 2),
    ci.lower   = as.numeric(inf[, "LowConfPt"]),
    ci.upper   = as.numeric(inf[, "UpConfPt"]),
    p.value    = as.numeric(inf[, "P-value"]),
    stringsAsFactors = FALSE
  )
  out[order(out$variable), ]
}

## The 8 required GOLD-vs-NEW scenarios (test-spec.md Sec 2.5): 2 datasets x
## 3 seeds at the alpha/bonf.correct defaults, plus 2 extra FAKE-only runs
## that vary bonf.correct / alpha to confirm the Bonferroni/alpha bookkeeping
## travels identically through both implementations.
gold_vs_new_scenarios <- function() {
  fake <- make_fake_gold_data()
  real <- make_real_gold_data()
  list(
    list(label = "FAKE seed=1",             dataset = "fake", bundle = fake,
         seed = 1,    alpha = 0.10, bonf.correct = TRUE),
    list(label = "FAKE seed=42",            dataset = "fake", bundle = fake,
         seed = 42,   alpha = 0.10, bonf.correct = TRUE),
    list(label = "FAKE seed=2026",          dataset = "fake", bundle = fake,
         seed = 2026, alpha = 0.10, bonf.correct = TRUE),
    list(label = "REAL seed=1",             dataset = "real", bundle = real,
         seed = 1,    alpha = 0.10, bonf.correct = TRUE),
    list(label = "REAL seed=42",            dataset = "real", bundle = real,
         seed = 42,   alpha = 0.10, bonf.correct = TRUE),
    list(label = "REAL seed=2026",          dataset = "real", bundle = real,
         seed = 2026, alpha = 0.10, bonf.correct = TRUE),
    list(label = "FAKE seed=42 bonf=FALSE", dataset = "fake", bundle = fake,
         seed = 42,   alpha = 0.10, bonf.correct = FALSE),
    list(label = "FAKE seed=42 alpha=0.05", dataset = "fake", bundle = fake,
         seed = 42,   alpha = 0.05, bonf.correct = TRUE)
  )
}

test_that("GOLD-vs-NEW: method = 'z' exact match vs conformalInference (tolerance 1e-6)", {
  skip_if_no_ranger()
  skip_if_no_conformal()
  skip_on_cran()

  for (sc in gold_vs_new_scenarios()) {
    gold_raw <- make_gold_loco_raw(sc$bundle, seed = sc$seed, alpha = sc$alpha,
                                   bonf.correct = sc$bonf.correct)
    gold <- gold_frame(gold_raw, sc$bundle$pred.names, method = "z")

    new_res <- suppressWarnings(loco(
      sc$bundle$mod, data = sc$bundle$dat, split = TRUE, method = "z",
      alpha = sc$alpha, bonf.correct = sc$bonf.correct, seed = sc$seed
    ))
    new_out <- new_res$vimp[order(new_res$vimp$Variable), ]

    expect_equal(new_out$Importance, gold$importance, tolerance = 1e-6,
                 info = sc$label)
    expect_equal(new_out$CI.lower, gold$ci.lower, tolerance = 1e-6,
                 info = sc$label)
    expect_equal(new_out$CI.upper, gold$ci.upper, tolerance = 1e-6,
                 info = sc$label)
    expect_equal(new_out$p.value, gold$p.value, tolerance = 1e-6,
                 info = sc$label)
  }
})

test_that("GOLD-vs-NEW: method = 'wilcox' equivalence bar vs conformalInference", {
  skip_if_no_ranger()
  skip_if_no_conformal()
  skip_on_cran()

  ## Sec 2.6 uses only the 6 dataset x seed combinations at the defaults
  ## (alpha = 0.10, bonf.correct = TRUE) -- the 2 extra Bonferroni/alpha
  ## combos are z-tier-only (Sec 2.5) and are not part of the wilcox grid.
  wilcox_scenarios <- gold_vs_new_scenarios()[1:6]

  for (sc in wilcox_scenarios) {
    gold_raw <- make_gold_loco_raw(sc$bundle, seed = sc$seed, alpha = sc$alpha,
                                   bonf.correct = sc$bonf.correct)
    gold_w <- gold_frame(gold_raw, sc$bundle$pred.names, method = "wilcox")

    new_res <- suppressWarnings(loco(
      sc$bundle$mod, data = sc$bundle$dat, split = TRUE, method = "wilcox",
      alpha = sc$alpha, bonf.correct = sc$bonf.correct, seed = sc$seed
    ))
    new_out <- new_res$vimp[order(new_res$vimp$Variable), ]

    ## 1. Significance-decision agreement (exact).
    sig_new  <- new_out$Variable[new_out$p.value < 0.10]
    sig_gold <- gold_w$variable[gold_w$p.value < 0.10]
    expect_setequal(sig_new, sig_gold)

    ## 2. Ranking agreement (Spearman >= 0.9).
    rho <- cor(new_out$Importance, gold_w$importance, method = "spearman")
    expect_gte(rho, 0.9)

    ## 3. Point-estimate / CI tolerance, calibrated by dataset size:
    ## FAKE (n=150) <= 0.02, REAL/mtcars (n=32) <= 0.08.
    tol <- if (identical(sc$dataset, "fake")) 0.02 else 0.08
    max_diff <- max(
      abs(new_out$Importance - gold_w$importance),
      abs(new_out$CI.lower - gold_w$ci.lower),
      abs(new_out$CI.upper - gold_w$ci.upper)
    )
    expect_lte(max_diff, tol)
  }
})

# ---- Edge cases (test-spec.md Sec 3) ------------------------------------

test_that("verbose is a true no-op on the migrated split path", {
  skip_if_no_ranger()
  fake <- make_fake_gold_data()

  warn_msgs <- character(0)
  capture_warnings <- function(expr) {
    withCallingHandlers(
      expr,
      warning = function(w) {
        warn_msgs <<- c(warn_msgs, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    )
  }

  out_v  <- capture_warnings(loco(fake$mod, data = fake$dat, split = TRUE,
                                  method = "z", seed = 1, verbose = TRUE))
  out_nv <- capture_warnings(loco(fake$mod, data = fake$dat, split = TRUE,
                                  method = "z", seed = 1, verbose = FALSE))

  expect_equal(out_v, out_nv)
  expect_false(any(grepl("conformalInference", warn_msgs, fixed = TRUE)))
})

test_that("loss = 'mse' split mode is unaffected (never routed through conformalInference)", {
  skip_if_no_ranger()
  fake <- make_fake_gold_data()
  out <- loco(fake$mod, data = fake$dat, split = TRUE, loss = "mse", seed = 1)
  expect_s3_class(out, "loco_vimp")
  expect_named(out$vimp, c("Variable", "Importance", "CI.lower", "CI.upper",
                      "p.value"))
  expect_identical(out$loss, "mse")
  expect_equal(nrow(out$vimp), 5L)
})

# ---- API-stability (test-spec.md Sec 7) ---------------------------------

test_that("T-API: loco formals unchanged", {
  expect_identical(
    names(formals(loco)),
    c("model", "data", "alpha", "split", "method", "loss",
      "groups", "bonf.correct", "seed", "verbose",
      "cross.fit", "num.folds")
  )
  expect_identical(eval(formals(loco)$alpha), 0.1)
  expect_identical(eval(formals(loco)$split), TRUE)
  expect_identical(eval(formals(loco)$method), c("z", "wilcox"))
  expect_identical(eval(formals(loco)$loss),
                    c("auto", "abs", "mse", "brier", "zero_one", "log"))
  expect_identical(eval(formals(loco)$groups), NULL)
  expect_identical(eval(formals(loco)$bonf.correct), TRUE)
  expect_identical(eval(formals(loco)$seed), 1995)
  expect_identical(eval(formals(loco)$verbose), FALSE)
  expect_identical(eval(formals(loco)$cross.fit), FALSE)
  expect_identical(eval(formals(loco)$num.folds), 5L)
})
