# Tests for loco() grf backend support — covers grf::regression_forest,
# grf::boosted_regression_forest, grf::probability_forest in both OOB and
# split modes, plus rejection of out-of-scope grf forest classes.

skip_if_no_grf <- function() {
  testthat::skip_if_not_installed("grf")
}

make_grf_reg <- function(n = 100, seed = 1, trees = 100) {
  set.seed(seed)
  X <- matrix(rnorm(n * 4), n, 4)
  colnames(X) <- paste0("x", 1:4)
  Y <- X[, 1] + 0.5 * X[, 2] + rnorm(n, sd = 0.5)
  list(X = X, Y = Y, mod = grf::regression_forest(X, Y, num.trees = trees))
}

make_grf_brf <- function(n = 100, seed = 1, trees = 50) {
  set.seed(seed)
  X <- matrix(rnorm(n * 4), n, 4)
  colnames(X) <- paste0("x", 1:4)
  Y <- X[, 1] + 0.5 * X[, 2] + rnorm(n, sd = 0.5)
  list(X = X, Y = Y,
       mod = grf::boosted_regression_forest(X, Y, num.trees = trees))
}

make_grf_prob_binary <- function(n = 150, seed = 2, trees = 100) {
  set.seed(seed)
  X <- matrix(rnorm(n * 4), n, 4)
  colnames(X) <- paste0("x", 1:4)
  Y <- factor(rbinom(n, 1, plogis(1.2 * X[, 1] + 0.6 * X[, 2])))
  list(X = X, Y = Y, mod = grf::probability_forest(X, Y, num.trees = trees))
}

make_grf_prob_multi <- function(n = 180, seed = 3, trees = 100) {
  set.seed(seed)
  X <- matrix(rnorm(n * 4), n, 4)
  colnames(X) <- paste0("x", 1:4)
  s1 <- X[, 1] + 0.5 * X[, 2]
  s2 <- -0.5 * X[, 1] + X[, 2]
  s3 <- 0.1 * X[, 3]
  P <- exp(cbind(s1, s2, s3)); P <- P / rowSums(P)
  Y <- factor(apply(P, 1, function(p) sample(c("A","B","C"), 1, prob = p)))
  list(X = X, Y = Y, mod = grf::probability_forest(X, Y, num.trees = trees))
}

# ---- grf::regression_forest ---------------------------------------------

test_that("grf regression_forest + OOB + abs returns expected shape", {
  skip_if_no_grf()
  b <- make_grf_reg()
  out <- loco(b$mod, split = FALSE, seed = 1)
  expect_s3_class(out, "loco")
  expect_named(out$vimp, c("Variable", "Importance", "CI.lower", "CI.upper", "p.value"))
  expect_setequal(out$vimp$Variable, paste0("x", 1:4))
  expect_identical(out$method, "oob")
  expect_identical(out$loss, "abs")
  expect_equal(out$vimp$Importance, sort(out$vimp$Importance, decreasing = TRUE))
  # x1 should be the top signal
  expect_equal(out$vimp$Variable[1L], "x1")
})

test_that("grf regression_forest + OOB + mse works", {
  skip_if_no_grf()
  b <- make_grf_reg()
  out <- loco(b$mod, split = FALSE, loss = "mse", seed = 1)
  expect_identical(out$loss, "mse")
  expect_equal(out$vimp$Variable[1L], "x1")
})

test_that("grf regression_forest + split + abs uses custom split loop and gives one-sided p-values", {
  skip_if_no_grf()
  b <- make_grf_reg()
  out <- loco(b$mod, split = TRUE, method = "z", seed = 1, alpha = 0.2)
  expect_named(out$vimp, c("Variable", "Importance", "CI.lower", "CI.upper",
                      "p.value"))
  expect_identical(out$method, "z")
  expect_identical(out$loss, "abs")
  # signal variables: x1 should reject at alpha=0.2 with high confidence
  pv_x1 <- out$vimp$p.value[out$vimp$Variable == "x1"]
  expect_lt(pv_x1, 0.2)
  # noise variables: p-values bounded in [0,1]
  expect_true(all(out$vimp$p.value >= 0 & out$vimp$p.value <= 1))
})

test_that("grf regression_forest + split + wilcox", {
  skip_if_no_grf()
  b <- make_grf_reg()
  out <- loco(b$mod, split = TRUE, method = "wilcox", seed = 1, alpha = 0.2)
  expect_identical(out$method, "wilcox")
  expect_true(all(out$vimp$p.value >= 0 & out$vimp$p.value <= 1, na.rm = TRUE))
})

test_that("grf regression_forest + group-LOCO (OOB and split)", {
  skip_if_no_grf()
  b <- make_grf_reg()
  grp <- list(g_signal = c("x1", "x2"), g_noise = c("x3", "x4"))
  out_oob <- loco(b$mod, split = FALSE, groups = grp, seed = 1)
  expect_setequal(out_oob$vimp$Variable, c("g_signal", "g_noise"))
  expect_true("Members" %in% names(out_oob$vimp))
  expect_equal(out_oob$vimp$Variable[1L], "g_signal")

  out_split <- loco(b$mod, split = TRUE, groups = grp, method = "z",
                    seed = 1, alpha = 0.2)
  expect_setequal(out_split$vimp$Variable, c("g_signal", "g_noise"))
  expect_true("Members" %in% names(out_split$vimp))
  expect_lt(out_split$vimp$p.value[out_split$vimp$Variable == "g_signal"], 0.2)
})

test_that("grf regression: same seed gives reproducible importances", {
  skip_if_no_grf()
  b <- make_grf_reg()
  o1 <- loco(b$mod, split = FALSE, seed = 42)
  o2 <- loco(b$mod, split = FALSE, seed = 42)
  ord <- function(o) o$vimp[order(o$vimp$Variable), ]
  expect_equal(ord(o1)$Importance, ord(o2)$Importance)
})

test_that("grf regression: data= argument is ignored with a warning", {
  skip_if_no_grf()
  b <- make_grf_reg()
  expect_warning(
    loco(b$mod, data = data.frame(x1=1,x2=2,x3=3,x4=4),
         split = FALSE, seed = 1),
    "ignored for grf models"
  )
})

# ---- grf::boosted_regression_forest --------------------------------------

test_that("grf boosted_regression_forest + OOB + abs", {
  skip_if_no_grf()
  b <- make_grf_brf()
  out <- loco(b$mod, split = FALSE, seed = 1)
  expect_named(out$vimp, c("Variable", "Importance", "CI.lower", "CI.upper", "p.value"))
  expect_setequal(out$vimp$Variable, paste0("x", 1:4))
  expect_equal(out$vimp$Variable[1L], "x1")
})

test_that("grf boosted_regression_forest + OOB + mse", {
  skip_if_no_grf()
  b <- make_grf_brf()
  out <- loco(b$mod, split = FALSE, loss = "mse", seed = 1)
  expect_identical(out$loss, "mse")
  expect_equal(out$vimp$Variable[1L], "x1")
})

test_that("grf boosted_regression_forest + split + z", {
  skip_if_no_grf()
  b <- make_grf_brf()
  # Smaller sample-fraction inside boosted forests can be slow; this is small.
  out <- loco(b$mod, split = TRUE, method = "z", seed = 1, alpha = 0.2)
  expect_named(out$vimp, c("Variable", "Importance", "CI.lower", "CI.upper",
                      "p.value"))
  expect_lt(out$vimp$p.value[out$vimp$Variable == "x1"], 0.2)
})

# ---- grf::probability_forest --------------------------------------------

test_that("grf probability_forest + OOB + brier", {
  skip_if_no_grf()
  b <- make_grf_prob_binary()
  out <- loco(b$mod, split = FALSE, seed = 1)
  expect_named(out$vimp, c("Variable", "Importance", "CI.lower", "CI.upper", "p.value"))
  expect_identical(out$loss, "brier")
  expect_equal(out$vimp$Variable[1L], "x1")
})

test_that("grf probability_forest + OOB + zero_one", {
  skip_if_no_grf()
  b <- make_grf_prob_binary()
  out <- loco(b$mod, split = FALSE, loss = "zero_one", seed = 1)
  expect_identical(out$loss, "zero_one")
})

test_that("grf probability_forest + OOB + log loss", {
  skip_if_no_grf()
  b <- make_grf_prob_binary()
  out <- loco(b$mod, split = FALSE, loss = "log", seed = 1)
  expect_identical(out$loss, "log")
  expect_true(all(is.finite(out$vimp$Importance)))
})

test_that("grf probability_forest + group-LOCO brier", {
  skip_if_no_grf()
  b <- make_grf_prob_binary()
  out <- loco(b$mod, split = FALSE,
              groups = list(g_sig = c("x1","x2"), g_noise = c("x3","x4")),
              seed = 1)
  expect_setequal(out$vimp$Variable, c("g_sig", "g_noise"))
  expect_equal(out$vimp$Variable[1L], "g_sig")
})

test_that("grf probability_forest multi-class brier", {
  skip_if_no_grf()
  b <- make_grf_prob_multi()
  out <- loco(b$mod, split = FALSE, seed = 1)
  expect_setequal(out$vimp$Variable, paste0("x", 1:4))
  # x4 is pure noise — should land near the bottom
  expect_equal(out$vimp$Variable[1L], "x1")
})

test_that("grf probability_forest + split + z + brier", {
  skip_if_no_grf()
  b <- make_grf_prob_binary()
  out <- loco(b$mod, split = TRUE, method = "z", seed = 1, alpha = 0.2)
  expect_named(out$vimp, c("Variable", "Importance", "CI.lower", "CI.upper",
                      "p.value"))
  expect_identical(out$loss, "brier")
})

# ---- rejection of out-of-scope grf classes -------------------------------

test_that("loco() rejects causal_forest with a pointer to cf_loco()", {
  skip_if_no_grf()
  set.seed(1); n <- 60; p <- 3
  X <- matrix(rnorm(n * p), n, p)
  Y <- rnorm(n); W <- rbinom(n, 1, 0.5)
  cf <- grf::causal_forest(X, Y, W, num.trees = 50)
  expect_error(loco(cf), "cf_loco\\(\\)")
})

test_that("loco() rejects survival/quantile/instrumental/multi_arm forests", {
  skip_if_no_grf()
  set.seed(1); n <- 60; p <- 3
  X <- matrix(rnorm(n * p), n, p)
  Y <- rnorm(n); W <- rbinom(n, 1, 0.5)

  qf <- grf::quantile_forest(X, Y, num.trees = 50)
  expect_error(loco(qf), "quantile_forest")

  Z <- rnorm(n)
  iv <- grf::instrumental_forest(X, Y, W, Z, num.trees = 50)
  expect_error(loco(iv), "instrumental_forest")

  # Survival forest if available
  if (requireNamespace("grf", quietly = TRUE) &&
      exists("survival_forest", where = asNamespace("grf"))) {
    Yt <- abs(rnorm(n)) + 0.01
    D <- rbinom(n, 1, 0.7)
    sf <- tryCatch(grf::survival_forest(X, Yt, D, num.trees = 50),
                   error = function(e) NULL)
    if (!is.null(sf)) {
      expect_error(loco(sf), "survival_forest")
    }
  }
})

test_that("loco() rejects unsupported model classes (e.g. lm)", {
  fit <- lm(mpg ~ wt + hp, data = mtcars)
  expect_error(loco(fit), "unsupported")
})

test_that("loco() works on grf forests fit with a data frame", {
  skip_if_no_grf()
  set.seed(1)
  n <- 120
  Xdf <- data.frame(x1 = rnorm(n), x2 = rnorm(n), x3 = rnorm(n))
  Y   <- Xdf$x1 + 0.5 * Xdf$x2 + rnorm(n, sd = 0.5)
  rf  <- grf::regression_forest(Xdf, Y, num.trees = 100)

  expect_s3_class(rf$X.orig, "data.frame")

  out_oob <- loco(rf, split = FALSE)
  expect_s3_class(out_oob, "loco")
  expect_setequal(out_oob$vimp$Variable, names(Xdf))

  out_split <- loco(rf, split = TRUE)
  expect_s3_class(out_split, "loco")
  expect_setequal(out_split$vimp$Variable, names(Xdf))
})

test_that("loco() errors clearly when grf X.orig has non-numeric columns", {
  skip_if_no_grf()
  obj <- make_grf_reg(n = 80, trees = 50)
  # Inject a non-numeric column to simulate a malformed X.orig.
  rf <- obj$mod
  rf$X.orig <- as.data.frame(rf$X.orig)
  rf$X.orig$bad <- letters[seq_len(nrow(rf$X.orig))]
  expect_error(loco(rf, split = FALSE), "non-numeric column")
})
