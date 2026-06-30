# Tests for cf_perm() observed-support missingness handling (allow.missing).
# Complete-data behavior is pinned in test-cf_perm.R; these cover only the opt-in
# missingness branch. All tests are fast (single small forests).

# Toy DGP with NAs injected into one covariate; the forest is fit WITH the NAs so
# grf's MIA routing is trained. X1 drives the CATE, X2 is a main effect.
make_missing_cf <- function(n = 300, p = 4, miss.col = 2L, miss.rate = 0.2,
                            seed = 1) {
  set.seed(seed)
  X <- matrix(stats::rnorm(n * p), n, p)
  colnames(X) <- paste0("X", seq_len(p))
  W <- stats::rbinom(n, 1, 0.5)
  Y <- X[, 1] * W + X[, 2] + stats::rnorm(n)
  miss <- sample.int(n, floor(miss.rate * n))
  X[miss, miss.col] <- NA
  cf <- grf::causal_forest(X, Y, W, num.trees = 300, seed = seed)
  list(cf = cf, miss.col = miss.col, miss = miss)
}

# 1. Gate message: NA + allow.missing = FALSE -> error naming both scopes.
test_that("cf_perm: NA + allow.missing = FALSE errors naming observed and marginal", {
  d <- make_missing_cf()
  expect_error(cf_perm(d$cf, verbose = FALSE), "allow.missing")
  expect_error(cf_perm(d$cf, verbose = FALSE), "observed")
  expect_error(cf_perm(d$cf, verbose = FALSE), "marginal")
})

# 2. Complete-data equivalence: allow.missing values do not change complete data.
test_that("cf_perm: complete data is identical across allow.missing values", {
  cf <- make_test_cf()
  r0 <- cf_perm(cf, n.perm = 15, seed = 1, verbose = FALSE)
  r1 <- cf_perm(cf, n.perm = 15, seed = 1, verbose = FALSE,
                allow.missing = "observed")
  r2 <- cf_perm(cf, n.perm = 15, seed = 1, verbose = FALSE,
                allow.missing = "marginal")
  expect_equal(r1$vimp, r0$vimp)
  expect_equal(r2$vimp, r0$vimp)
  expect_true(all(r0$miss.rate == 0))
})

# 3. Runs with NA in a continuous covariate -> finite importances, no error.
test_that("cf_perm: runs with NA in a continuous covariate (observed scope)", {
  d <- make_missing_cf(miss.col = 1L, miss.rate = 0.2, seed = 12)
  res <- cf_perm(d$cf, n.perm = 15, seed = 1, verbose = FALSE,
                 allow.missing = "observed")
  expect_s3_class(res, "cf_perm")
  expect_true(all(is.finite(res$vimp$Importance)))
})

# 4. Exact-zero NA rows: white-box + manual predict comparison.
test_that(".cf_perm_cp_sample(obs.support=TRUE) leaves NA rows NA; their delta is 0", {
  set.seed(1)
  X <- matrix(stats::rnorm(200 * 3), 200, 3)
  colnames(X) <- paste0("X", 1:3)
  na.idx <- c(3, 10, 50)
  X[na.idx, 1] <- NA
  out <- .cf_perm_cp_sample(1, X, X, n.perm = 8, seed = 1, obs.support = TRUE)
  expect_equal(dim(out), c(200L, 8L))
  expect_true(all(is.na(out[na.idx, ])))        # NA apply rows stay NA
  expect_true(all(is.finite(out[-na.idx, ])))   # observed rows perturbed

  # NA-X_j rows are unchanged pre/post permutation -> identical CATE -> zero delta.
  d <- make_missing_cf(miss.col = 1L, miss.rate = 0.2, seed = 13)
  cf <- d$cf
  Xo <- cf$X.orig
  Xperm <- .cf_perm_cp_sample(1, Xo, Xo, n.perm = 1, seed = 1, obs.support = TRUE)
  Xk <- Xo
  Xk[, 1] <- Xperm[, 1]
  tau0 <- stats::predict(cf, Xo)$predictions
  tauk <- stats::predict(cf, Xk)$predictions
  na.rows <- is.na(Xo[, 1])
  expect_equal(tauk[na.rows], tau0[na.rows])
})

# 5. Binary with NA -> Bernoulli branch; non-NA emissions stay on {lo, hi}.
test_that(".cf_perm_cp_sample(obs.support=TRUE) binary stays on observed levels", {
  set.seed(2)
  xj <- stats::rbinom(200, 1, 0.4)
  X <- cbind(xj, stats::rnorm(200), stats::rnorm(200))
  colnames(X) <- paste0("X", 1:3)
  na.idx <- c(5, 25, 60)
  X[na.idx, 1] <- NA
  out <- .cf_perm_cp_sample(1, X, X, n.perm = 6, seed = 2, obs.support = TRUE)
  obs <- out[!is.na(out)]
  expect_true(all(obs %in% c(0, 1)))
  expect_true(all(is.na(out[na.idx, ])))
})

# 6. Multi-level discrete with NA -> residual-shuffle branch (runs).
test_that(".cf_perm_cp_sample(obs.support=TRUE) multi-level discrete residual-shuffles", {
  set.seed(3)
  xj <- sample(0:4, 200, replace = TRUE)
  X <- cbind(xj, stats::rnorm(200), stats::rnorm(200))
  colnames(X) <- paste0("X", 1:3)
  na.idx <- c(7, 30)
  X[na.idx, 1] <- NA
  out <- .cf_perm_cp_sample(1, X, X, n.perm = 5, seed = 3, obs.support = TRUE)
  expect_true(all(is.na(out[na.idx, ])))
  expect_true(all(is.finite(out[-na.idx, ])))
})

# 7. All-NA / < min.obs covariate -> all-NA matrix (white-box) and 0 / p 1 / warn.
test_that(".cf_perm_cp_sample returns all-NA when observed labels < min.obs", {
  set.seed(4)
  X <- matrix(stats::rnorm(200 * 3), 200, 3)
  colnames(X) <- paste0("X", 1:3)
  X[5:200, 1] <- NA   # only 4 observed values < min.obs (5)
  out <- .cf_perm_cp_sample(1, X, X, n.perm = 5, seed = 4, obs.support = TRUE)
  expect_true(all(is.na(out)))
})

test_that("cf_perm degrades a near-all-NA covariate to 0 / p 1 with a warning", {
  set.seed(5); n <- 300; p <- 3
  X <- matrix(stats::rnorm(n * p), n, p); colnames(X) <- paste0("X", 1:p)
  X[5:n, 2] <- NA   # only 4 observed values in X2 < min.obs
  W <- stats::rbinom(n, 1, 0.5)
  Y <- X[, 1] * W + stats::rnorm(n)
  cf <- grf::causal_forest(X, Y, W, num.trees = 300, seed = 5)
  expect_warning(
    res <- cf_perm(cf, n.perm = 10, seed = 1, verbose = FALSE,
                   allow.missing = "observed"),
    "observed values"
  )
  x2 <- res$vimp[res$vimp$Variable == "X2", ]
  expect_equal(x2$Importance, 0)
  expect_equal(x2$p.value, 1)
})

# 8. Cross-fit with NA: runs; fold handling robust.
test_that("cf_perm cross-fit runs with NA (R loss, observed scope)", {
  d <- make_missing_cf(n = 400, miss.col = 2L, miss.rate = 0.2, seed = 6)
  expect_no_error(
    res <- cf_perm(d$cf, n.perm = 8, cross.fit = TRUE, num.folds = 3,
                   seed = 1, verbose = FALSE, allow.missing = "observed")
  )
  expect_true(res$cross.fit)
  expect_true(all(is.finite(res$vimp$Importance)))
})

# 9. Scope direction: NA-X_j rows contribute EXACTLY 0 (their prediction is
# invariant to permuting X_j; the light-path OOB-baseline artifact is zeroed), so
# light-path "marginal" is the observed-support importance discounted exactly by
# the missingness rate: imp_marginal == imp_observed * (n_obs / n). The two scopes
# coincide with no NA.
test_that("cf_perm marginal scope is an exact missingness discount of observed", {
  d <- make_missing_cf(n = 400, miss.col = 1L, miss.rate = 0.4, seed = 7)
  ro <- cf_perm(d$cf, n.perm = 20, seed = 1, verbose = FALSE,
                allow.missing = "observed")
  rm <- cf_perm(d$cf, n.perm = 20, seed = 1, verbose = FALSE,
                allow.missing = "marginal")
  n_all <- nrow(d$cf$X.orig)
  io <- stats::setNames(ro$vimp$Importance, ro$vimp$Variable)
  im <- stats::setNames(rm$vimp$Importance, rm$vimp$Variable)
  expect_gt(io[["X1"]], 0)   # CATE driver, large positive importance
  for (v in ro$vimp$Variable) {
    n_obs <- sum(!is.na(d$cf$X.orig[, v]))
    expect_equal(im[[v]], io[[v]] * (n_obs / n_all), tolerance = 1e-8)
  }

  # With complete data the two scopes coincide exactly.
  cf <- make_test_cf()
  io2 <- cf_perm(cf, n.perm = 15, seed = 1, verbose = FALSE,
                 allow.missing = "observed")$vimp$Importance
  im2 <- cf_perm(cf, n.perm = 15, seed = 1, verbose = FALSE,
                 allow.missing = "marginal")$vimp$Importance
  expect_equal(im2, io2)
})

# 10. miss.rate element + printed table appears iff any > 0.
test_that("cf_perm miss.rate matches colMeans(is.na(X)) and drives the print table", {
  d <- make_missing_cf(miss.col = 2L, miss.rate = 0.2, seed = 8)
  res <- cf_perm(d$cf, n.perm = 10, seed = 1, verbose = FALSE,
                 allow.missing = "observed")
  expect_equal(unname(res$miss.rate), unname(colMeans(is.na(d$cf$X.orig))))
  expect_true(res$miss.rate[["X2"]] > 0)

  out <- capture.output(print(res))
  expect_true(any(grepl("Missing covariate values", out)))
  expect_true(any(grepl("scope = observed", out)))

  # No missingness -> table suppressed, complete-data printing unchanged.
  cf <- make_test_cf()
  res2 <- cf_perm(cf, n.perm = 10, seed = 1, verbose = FALSE)
  out2 <- capture.output(print(res2))
  expect_false(any(grepl("Missing covariate values", out2)))
  expect_true(all(res2$miss.rate == 0))
})

# 11. AIPW + missing and screen + missing both run.
test_that("cf_perm runs with AIPW + missing and screen + missing", {
  d <- make_missing_cf(miss.col = 2L, miss.rate = 0.2, seed = 9)
  expect_no_error(
    rA <- cf_perm(d$cf, loss = "AIPW", n.perm = 10, seed = 1, verbose = FALSE,
                  allow.missing = "observed")
  )
  expect_identical(rA$loss, "AIPW")
  expect_no_error(
    rs <- cf_perm(d$cf, n.perm = 10, screen = 2L, seed = 1, verbose = FALSE,
                  allow.missing = "marginal")
  )
  expect_s3_class(rs, "cf_perm")
})
