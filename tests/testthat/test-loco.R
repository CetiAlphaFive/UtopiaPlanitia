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
  expect_s3_class(out, "data.frame")
  expect_named(out, c("variable", "importance", "method", "loss"))
  expect_setequal(out$variable, c("x1", "x2", "x3"))
  expect_true(all(out$method == "oob"))
  expect_true(all(out$loss == "abs"))
  expect_equal(out$importance, sort(out$importance, decreasing = TRUE))
})

test_that("OOB mode is reproducible with the same seed", {
  skip_if_no_ranger()
  bundle <- make_reg_model()
  o1 <- loco(bundle$mod, data = bundle$dat, split = FALSE, seed = 42)
  o2 <- loco(bundle$mod, data = bundle$dat, split = FALSE, seed = 42)
  o1s <- o1[order(o1$variable), ]
  o2s <- o2[order(o2$variable), ]
  expect_equal(o1s$importance, o2s$importance)
  o3 <- loco(bundle$mod, data = bundle$dat, split = FALSE, seed = 999)
  o3s <- o3[order(o3$variable), ]
  expect_false(isTRUE(all.equal(o1s$importance, o3s$importance)))
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
  expect_s3_class(out, "data.frame")
  expect_setequal(out$variable, c("x1", "x2", "x3"))
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
  expect_setequal(out$variable, c("x1", "x2"))
})

test_that("split mode rejects factor predictors with a clear message", {
  skip_if_no_ranger()
  skip_if_no_conformal()
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
  expect_s3_class(out, "data.frame")
  expect_setequal(out$variable, c("x1", "x2", "x3"))
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
  expect_setequal(out$variable, c("x1", "x2", "x3"))
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
  expect_setequal(out$variable, c("x1", "x2", "x3"))
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
  expect_setequal(out$variable, c("x1", "x2", "x3"))
})

test_that("split mode returns the expected columns and method tag", {
  skip_if_no_ranger()
  skip_if_no_conformal()
  bundle <- make_reg_model(n = 120, trees = 100)
  out <- suppressWarnings(loco(bundle$mod, data = bundle$dat,
                               split = TRUE, seed = 1, method = "z"))
  expect_s3_class(out, "data.frame")
  expect_named(out, c("variable", "importance",
                      "ci.lower", "ci.upper", "p.value", "method", "loss"))
  expect_true(all(out$method == "z"))
  expect_true(all(out$loss == "abs"))
  expect_true(all(out$ci.lower <= out$ci.upper))
  expect_equal(out$importance, (out$ci.lower + out$ci.upper) / 2)
  expect_true(all(out$p.value >= 0 & out$p.value <= 1))
})

test_that("split mode supports method = 'wilcox'", {
  skip_if_no_ranger()
  skip_if_no_conformal()
  bundle <- make_reg_model(n = 120, trees = 100)
  out <- suppressWarnings(loco(bundle$mod, data = bundle$dat,
                               split = TRUE, seed = 1, method = "wilcox"))
  expect_true(all(out$method == "wilcox"))
  expect_named(out, c("variable", "importance",
                      "ci.lower", "ci.upper", "p.value", "method", "loss"))
})

test_that("bonf.correct = TRUE produces p-values at least as large as raw", {
  skip_if_no_ranger()
  skip_if_no_conformal()
  bundle <- make_reg_model(n = 120, trees = 100)
  oA <- suppressWarnings(loco(bundle$mod, data = bundle$dat, split = TRUE,
                              seed = 1, method = "z", bonf.correct = TRUE))
  oB <- suppressWarnings(loco(bundle$mod, data = bundle$dat, split = TRUE,
                              seed = 1, method = "z", bonf.correct = FALSE))
  oA <- oA[order(oA$variable), ]
  oB <- oB[order(oB$variable), ]
  expect_true(all(oA$p.value >= oB$p.value - 1e-12))
})

test_that("alpha controls CI width monotonically", {
  skip_if_no_ranger()
  skip_if_no_conformal()
  bundle <- make_reg_model(n = 120, trees = 100)
  w_wide   <- suppressWarnings(loco(bundle$mod, data = bundle$dat, split = TRUE,
                                    seed = 1, method = "z", alpha = 0.20))
  w_narrow <- suppressWarnings(loco(bundle$mod, data = bundle$dat, split = TRUE,
                                    seed = 1, method = "z", alpha = 0.01))
  w_wide   <- w_wide[order(w_wide$variable), ]
  w_narrow <- w_narrow[order(w_narrow$variable), ]
  width_wide   <- w_wide$ci.upper - w_wide$ci.lower
  width_narrow <- w_narrow$ci.upper - w_narrow$ci.lower
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
  expect_named(out, c("variable", "importance", "method", "loss"))
  expect_true(all(out$loss == "brier"))
  expect_setequal(out$variable, c("x1","x2","x3"))
  ## x1 should beat x3 in importance
  i <- setNames(out$importance, out$variable)
  expect_gt(i["x1"], i["x3"])
})

test_that("probability forest, OOB, supports zero_one and log losses", {
  skip_if_no_ranger()
  b <- make_prob_model()
  out_zo <- loco(b$mod, data = b$dat, split = FALSE, seed = 1, loss = "zero_one")
  out_lg <- loco(b$mod, data = b$dat, split = FALSE, seed = 1, loss = "log")
  expect_true(all(out_zo$loss == "zero_one"))
  expect_true(all(out_lg$loss == "log"))
  expect_setequal(out_zo$variable, c("x1","x2","x3"))
})

test_that("classification forest, OOB, auto-detects zero_one loss", {
  skip_if_no_ranger()
  b <- make_clf_model()
  out <- loco(b$mod, data = b$dat, split = FALSE, seed = 1)
  expect_true(all(out$loss == "zero_one"))
  expect_setequal(out$variable, c("x1","x2","x3"))
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
  expect_named(out, c("variable","importance","ci.lower","ci.upper",
                      "p.value","method","loss"))
  expect_true(all(out$loss == "brier"))
  expect_true(all(out$ci.lower <= out$ci.upper))
  expect_true(all(out$p.value >= 0 & out$p.value <= 1))
})

test_that("classification forest, split mode, zero_one", {
  skip_if_no_ranger()
  b <- make_clf_model(n = 160, trees = 80)
  out <- loco(b$mod, data = b$dat, split = TRUE, seed = 1)
  expect_true(all(out$loss == "zero_one"))
  expect_true(all(out$method == "z"))
  expect_true(all(out$ci.lower <= out$ci.upper))
})

test_that("multi-class probability forest works with all loss choices", {
  skip_if_no_ranger()
  b <- make_multi_prob_model(n = 150, trees = 60)
  for (lo in c("brier", "zero_one", "log")) {
    out <- loco(b$mod, data = b$dat, split = FALSE, seed = 1, loss = lo)
    expect_true(all(out$loss == lo))
    expect_setequal(out$variable, c("x1","x2","x3","x4"))
  }
})

test_that("MSE loss on regression forest produces a result and differs from abs", {
  skip_if_no_ranger()
  bundle <- make_reg_model(n = 120, trees = 60)
  o_abs <- loco(bundle$mod, data = bundle$dat, split = FALSE,
                seed = 1, loss = "abs")
  o_mse <- loco(bundle$mod, data = bundle$dat, split = FALSE,
                seed = 1, loss = "mse")
  expect_true(all(o_mse$loss == "mse"))
  expect_true(all(o_abs$loss == "abs"))
  expect_setequal(o_abs$variable, o_mse$variable)
  oa <- o_abs[order(o_abs$variable), ]
  om <- o_mse[order(o_mse$variable), ]
  ## They should differ (MSE inflates large residuals)
  expect_false(isTRUE(all.equal(oa$importance, om$importance)))
})

# ---- New: Group-LOCO ----------------------------------------------------

test_that("group-LOCO OOB returns one row per group, with members column", {
  skip_if_no_ranger()
  bundle <- make_reg_model(n = 100, trees = 60)
  out <- loco(bundle$mod, data = bundle$dat, split = FALSE, seed = 1,
              groups = list(g_x12 = c("x1","x2"), g_x3 = "x3"))
  expect_named(out, c("variable", "importance", "method", "loss", "members"))
  expect_setequal(out$variable, c("g_x12", "g_x3"))
  expect_equal(length(out$members), 2L)
  ## The combined importance of (x1,x2) should exceed x3 alone.
  i <- setNames(out$importance, out$variable)
  expect_gt(i["g_x12"], i["g_x3"])
})

test_that("group-LOCO split returns CI / p.value with members", {
  skip_if_no_ranger()
  bundle <- make_reg_model(n = 160, trees = 80)
  out <- loco(bundle$mod, data = bundle$dat, split = TRUE, seed = 1,
              groups = list(g_x12 = c("x1","x2"), g_x3 = "x3"))
  expect_named(out, c("variable","importance","ci.lower","ci.upper",
                      "p.value","method","loss","members"))
  expect_setequal(out$variable, c("g_x12", "g_x3"))
  expect_true(all(out$ci.lower <= out$ci.upper))
})

test_that("singleton groups in pred-name order equal per-variable LOCO (OOB)", {
  skip_if_no_ranger()
  bundle <- make_reg_model(n = 120, trees = 80)
  per_var <- loco(bundle$mod, data = bundle$dat, split = FALSE, seed = 1)
  singletons <- loco(bundle$mod, data = bundle$dat, split = FALSE, seed = 1,
                     groups = list(x1 = "x1", x2 = "x2", x3 = "x3"))
  per_var <- per_var[order(per_var$variable), ]
  singletons <- singletons[order(singletons$variable), ]
  expect_equal(singletons$importance, per_var$importance, tolerance = 1e-8)
})

test_that("character-vector groups arg becomes single unnamed group", {
  skip_if_no_ranger()
  bundle <- make_reg_model(n = 80, trees = 50)
  out <- loco(bundle$mod, data = bundle$dat, split = FALSE, seed = 1,
              groups = c("x1","x2"))
  expect_equal(nrow(out), 1L)
  expect_equal(out$variable, "group1")
  expect_equal(out$members[[1]], c("x1","x2"))
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
  expect_named(out, c("variable","importance","method","loss","members"))
  expect_true(all(out$loss == "brier"))
  i <- setNames(out$importance, out$variable)
  expect_gt(i["g_signal"], i["g_noise"])
})

test_that("group-LOCO + classification + split mode is wired end-to-end", {
  skip_if_no_ranger()
  b <- make_prob_model(n = 160, trees = 60)
  out <- loco(b$mod, data = b$dat, split = TRUE, seed = 1,
              groups = list(g_signal = c("x1","x2"), g_noise = "x3"))
  expect_named(out, c("variable","importance","ci.lower","ci.upper",
                      "p.value","method","loss","members"))
  expect_true(all(out$loss == "brier"))
  expect_true(all(out$ci.lower <= out$ci.upper))
})
