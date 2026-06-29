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
