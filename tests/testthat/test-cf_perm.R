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
