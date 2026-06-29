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

test_that("cf_perm rejects covariates containing NA", {
  cf <- make_test_cf()
  cf$X.orig[1, 1] <- NA
  expect_error(cf_perm(cf, verbose = FALSE),
               "does not support missing values")
})

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

test_that("cf_perm validates num.folds under cross.fit", {
  cf <- make_test_cf()
  expect_error(cf_perm(cf, cross.fit = TRUE, num.folds = 1, verbose = FALSE),
               "num.folds")
})

test_that("cf_perm cross.fit + screen: screened vars get 0 importance, p=1, NA inference", {
  cf  <- make_test_cf(n = 400)
  res <- cf_perm(cf, n.perm = 10, cross.fit = TRUE, num.folds = 3,
                 screen = 2L, seed = 1, verbose = FALSE)
  dropped <- res$vimp[res$vimp$Importance == 0 & res$vimp$p.value == 1, ]
  expect_true(nrow(dropped) >= 2L)
  expect_true(all(is.na(dropped$SE)))
})

test_that("cf_perm normalize warns and returns uniform 1/p when all importances <= 0", {
  set.seed(11)
  n <- 250; p <- 3
  X <- matrix(stats::rnorm(n * p), n, p); colnames(X) <- paste0("X", 1:p)
  W <- stats::rbinom(n, 1, 0.5)
  Y <- stats::rnorm(n)  # no treatment-effect heterogeneity -> all importances <= 0
  cf <- grf::causal_forest(X, Y, W, num.trees = 250, seed = 11)
  expect_warning(
    res <- cf_perm(cf, n.perm = 10, normalize = TRUE, seed = 1, verbose = FALSE),
    "uniform"
  )
  expect_equal(sum(res$vimp$Importance), 1, tolerance = 1e-8)
  expect_true(all(abs(res$vimp$Importance - 1 / p) < 1e-8))
})

test_that("cf_perm S3 methods behave", {
  cf  <- make_test_cf()
  res <- cf_perm(cf, n.perm = 10, seed = 1, verbose = FALSE)

  expect_output(print(res), "PermuCATE Variable Importance")
  expect_invisible(print(res))
  expect_output(summary(res), "PermuCATE Variable Importance")
  expect_invisible(summary(res))

  skip_if_not_installed("ggplot2")
  g <- plot(res)
  expect_s3_class(g, "ggplot")
})

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
  expect_lt(mean(reject), 0.30)   # generous bound; OOB light path is conservative
})

test_that("cf_perm guards single-covariate forests", {
  set.seed(3); n <- 150
  X <- matrix(stats::rnorm(n), n, 1); colnames(X) <- "X1"
  W <- stats::rbinom(n, 1, 0.5); Y <- X[, 1] * W + stats::rnorm(n)
  cf <- grf::causal_forest(X, Y, W, num.trees = 200)
  expect_error(cf_perm(cf, verbose = FALSE), "at least 2 covariates")
})

test_that("cf_perm errors on clustered forests (single and multi cluster)", {
  set.seed(4); n <- 200; p <- 3
  X <- matrix(stats::rnorm(n * p), n, p); colnames(X) <- paste0("X", 1:p)
  W <- stats::rbinom(n, 1, 0.5); Y <- X[, 1] * W + stats::rnorm(n)
  cf1 <- grf::causal_forest(X, Y, W, num.trees = 200, clusters = rep(1, n))
  expect_error(cf_perm(cf1, verbose = FALSE), "clustered")
  cf2 <- grf::causal_forest(X, Y, W, num.trees = 200,
                            clusters = sample(1:4, n, replace = TRUE))
  expect_error(cf_perm(cf2, verbose = FALSE), "clustered")
})

test_that("cf_perm cross.fit rejects num.folds >= n", {
  cf <- make_test_cf(n = 120)
  expect_error(cf_perm(cf, cross.fit = TRUE, num.folds = 120, verbose = FALSE),
               "num.folds")
})

test_that("cf_perm screen = p is a no-op (keeps all), not an error", {
  cf <- make_test_cf()  # p = 4
  expect_no_error(res <- cf_perm(cf, n.perm = 5, screen = 4L, seed = 1, verbose = FALSE))
  expect_equal(nrow(res$vimp), 4L)
})

test_that("cf_perm normalize + screen gives no importance to screened-out vars", {
  set.seed(11); n <- 250; p <- 4
  X <- matrix(stats::rnorm(n * p), n, p); colnames(X) <- paste0("X", 1:p)
  W <- stats::rbinom(n, 1, 0.5); Y <- stats::rnorm(n)  # null DGP
  cf <- grf::causal_forest(X, Y, W, num.trees = 250, seed = 11)
  suppressWarnings(
    res <- cf_perm(cf, n.perm = 5, screen = 2L, normalize = TRUE, seed = 1, verbose = FALSE)
  )
  expect_true(sum(res$vimp$Importance == 0) >= 2L)  # screened-out stay 0, not 1/p
  expect_equal(sum(res$vimp$Importance), 1, tolerance = 1e-8)
})

test_that("cf_perm cross.fit handles continuous treatment without empty folds", {
  set.seed(5); n <- 300; p <- 3
  X <- matrix(stats::rnorm(n * p), n, p); colnames(X) <- paste0("X", 1:p)
  W <- stats::runif(n)  # continuous treatment
  Y <- X[, 1] * W + stats::rnorm(n)
  cf <- grf::causal_forest(X, Y, W, num.trees = 200, seed = 5)
  expect_no_error(
    res <- cf_perm(cf, n.perm = 5, cross.fit = TRUE, num.folds = 3, seed = 1, verbose = FALSE)
  )
  expect_false(any(is.na(res$vimp$Importance)))
})
