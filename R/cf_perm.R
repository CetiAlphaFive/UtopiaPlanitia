# Per-observation CATE risk. Only `tau` varies between baseline and permuted;
# the nuisances (m, pi, psi, Y, W) stay fixed at the original X.
#
# loss = "R":    ((Y - m) - (W - pi) * tau)^2   (Robinson residual loss)
# loss = "AIPW": (psi - tau)^2                  (psi from grf::get_scores())
.cf_perm_risk <- function(tau, Y, m, W, pi, psi, loss) {
  if (loss == "R") {
    ((Y - m) - (W - pi) * tau)^2
  } else {
    (psi - tau)^2
  }
}

# Conditionally permute covariate `j`. Fit nu_hat_j = E[X_j | X_-j] on `Xfit`,
# apply on `Xapply`. Continuous: shuffle residuals of the apply set around its
# fitted conditional mean. Discrete (integer-coded, <= disc.max levels): draw a
# new level per row from the predicted class probabilities.
# Returns an nrow(Xapply) x n.perm matrix of permuted X_j columns.
.cf_perm_cp_sample <- function(j, Xfit, Xapply, n.perm, disc.max = 10L, seed = 1995) {
  xj.fit <- Xfit[, j]
  Xm.fit <- Xfit[, -j, drop = FALSE]
  xj.app <- Xapply[, j]
  Xm.app <- Xapply[, -j, drop = FALSE]
  na <- nrow(Xapply)

  is.disc <- all(xj.fit == round(xj.fit)) &&
    length(unique(xj.fit)) <= disc.max

  out <- matrix(NA_real_, na, n.perm)

  if (!is.disc) {
    rf <- grf::regression_forest(Xm.fit, xj.fit, seed = seed)
    nu <- stats::predict(rf, Xm.app)$predictions
    e  <- xj.app - nu
    for (k in seq_len(n.perm)) {
      out[, k] <- nu + e[sample.int(na)]
    }
  } else {
    fy    <- factor(xj.fit)
    pf    <- grf::probability_forest(Xm.fit, fy, seed = seed)
    probs <- stats::predict(pf, Xm.app)$predictions
    lev   <- as.numeric(colnames(probs))
    for (k in seq_len(n.perm)) {
      out[, k] <- lev[apply(probs, 1L, function(pr) sample.int(length(lev), 1L, prob = pr))]
    }
  }
  out
}
