# Shared toy DGP: X1 drives the CATE (signal), X2 a main effect, X3/X4 noise.
make_test_cf <- function(n = 300, p = 4, seed = 1) {
  set.seed(seed)
  X <- matrix(stats::rnorm(n * p), n, p)
  colnames(X) <- paste0("X", seq_len(p))
  W <- stats::rbinom(n, 1, 0.5)
  Y <- X[, 1] * W + X[, 2] + stats::rnorm(n)
  grf::causal_forest(X, Y, W, num.trees = 300, seed = seed)
}
