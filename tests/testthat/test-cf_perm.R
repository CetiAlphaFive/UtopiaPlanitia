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
