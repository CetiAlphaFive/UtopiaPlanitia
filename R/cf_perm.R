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
