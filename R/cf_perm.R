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

#' PermuCATE Variable Importance for Causal Forests
#'
#' Computes conditional-permutation variable importance (PermuCATE; Paillard et
#' al., 2025) for a fitted \code{grf} causal forest. Unlike \code{\link{cf_loco}},
#' which drops each covariate and refits, PermuCATE conditionally permutes each
#' covariate and re-scores the fixed forest, yielding importance scores together
#' with p-values and confidence intervals.
#'
#' @param c.forest A fitted causal forest object from the \code{grf} package.
#' @param loss Character; the CATE risk used to score importance. \code{"R"}
#'   (default) is the Robinson residual loss \eqn{((Y-m)-(W-\pi)\tau)^2}, using
#'   the forest's \code{Y.hat} and \code{W.hat}. \code{"AIPW"} is the
#'   pseudo-outcome risk \eqn{(\psi-\tau)^2} with \eqn{\psi} from
#'   \code{grf::get_scores()}.
#' @param n.perm Integer number of conditional permutations per covariate.
#'   Default \code{50}.
#' @param cross.fit Logical. If \code{FALSE} (default), the light path scores the
#'   passed forest in place using its out-of-bag predictions as the risk baseline
#'   and derives influence-function SEs. This baseline keeps the light-path
#'   inference conservative (it controls false positives at some cost to power for
#'   weak effect modifiers). If \code{TRUE}, refit per fold and aggregate with the
#'   Nadeau-Bengio correction for unbiased cross-fitted inference (R-loss only).
#' @param num.folds Integer number of folds when \code{cross.fit = TRUE}. Default
#'   \code{5}.
#' @param screen Optional split-frequency pre-screening, identical in meaning to
#'   the \code{screen} argument of \code{\link{cf_loco}}: \code{FALSE} (default),
#'   \code{TRUE} (drop zero-importance covariates), or an integer \code{k} (keep
#'   top-k). Screened-out covariates receive importance 0 and p-value 1.
#' @param normalize Logical. If \code{TRUE}, clip negative importances to 0 and
#'   rescale to sum to 1; SE/z/p/CI are set to \code{NA} (raw-scale quantities do
#'   not survive normalization). Default \code{FALSE}.
#' @param conf.level Confidence level for the reported intervals. Default
#'   \code{0.95}.
#' @param seed Integer seed for reproducibility. Default \code{1995}.
#' @param verbose Logical; if \code{TRUE} (default) emit progress/screening
#'   messages.
#'
#' @return An object of class \code{"cf_perm"} with components \code{vimp} (a data
#'   frame with columns \code{Variable}, \code{Importance}, \code{SE}, \code{z},
#'   \code{p.value}, \code{CI.lower}, \code{CI.upper}), \code{loss},
#'   \code{cross.fit}, \code{n.perm}, \code{num.folds}, \code{normalized},
#'   \code{conf.level}, \code{n}, and \code{p}.
#'
#' @references
#' Paillard, J., Reyero Lobo, A. D., Kolodyazhniy, V., Thirion, B., and
#' Engemann, D.-A. (2025). Measuring Variable Importance in Heterogeneous
#' Treatment Effects with Confidence. ICML 2025. \doi{10.48550/arXiv.2408.13002}
#'
#' Chamma, A., Engemann, D.-A., and Thirion, B. (2023). Statistically Valid
#' Variable Importance Assessment through Conditional Permutations.
#' \doi{10.48550/arXiv.2309.07593}
#'
#' @seealso [cf_loco()] for the LOCO alternative, [omni_hetero()] for
#'   heterogeneity testing.
#'
#' @export
#' @examples
#' \donttest{
#' library(grf)
#' set.seed(1995)
#' n <- 300; p <- 4
#' X <- matrix(rnorm(n * p), n, p); colnames(X) <- paste0("X", seq_len(p))
#' W <- rbinom(n, 1, 0.5)
#' Y <- X[, 1] * W + rnorm(n)
#' cf <- causal_forest(X, Y, W, num.trees = 300)
#' cf_perm(cf, n.perm = 20)
#' }
cf_perm <- function(c.forest, loss = c("R", "AIPW"), n.perm = 50L,
                    cross.fit = FALSE, num.folds = 5L, screen = FALSE,
                    normalize = FALSE, conf.level = 0.95, seed = 1995,
                    verbose = TRUE) {

  loss <- match.arg(loss)
  if (!inherits(c.forest, "causal_forest")) {
    stop("c.forest must be a grf causal forest.")
  }
  if (anyNA(c.forest$X.orig)) {
    stop("cf_perm does not support missing values in the covariate matrix.")
  }
  if (cross.fit && loss == "AIPW") {
    stop("cross.fit = TRUE is only supported with loss = \"R\" in this version.")
  }
  set.seed(seed)

  X <- c.forest$X.orig
  Y <- c.forest$Y.orig
  W <- c.forest$W.orig
  m  <- c.forest$Y.hat
  pi <- c.forest$W.hat
  n  <- nrow(X)
  p  <- ncol(X)
  vnames <- colnames(X)
  if (is.null(vnames)) vnames <- paste0("V", seq_len(p))

  # --- screening (split-frequency importance) ---
  keep <- rep(TRUE, p)
  if (!isFALSE(screen)) {
    if (!isTRUE(screen) && !(is.numeric(screen) && length(screen) == 1L &&
                             screen == as.integer(screen) && screen >= 1L)) {
      stop("screen must be FALSE, TRUE, or a positive integer.")
    }
    if (is.numeric(screen) && screen >= p) {
      stop("screen must be less than the number of covariates (", p, ").")
    }
    vi.split <- as.numeric(grf::variable_importance(c.forest))
    if (isTRUE(screen)) {
      keep <- vi.split > 0
      if (!any(keep)) keep <- rep(TRUE, p)
    } else {
      ord  <- order(vi.split, decreasing = TRUE)
      keep <- seq_len(p) %in% ord[seq_len(as.integer(screen))]
    }
    if (verbose) {
      message("Screening: PermuCATE on ", sum(keep), " of ", p, " covariates (",
              paste(vnames[keep], collapse = ", "), ")")
    }
  }

  imp <- se <- z <- pval <- ci.lo <- ci.hi <- rep(NA_real_, p)

  if (!cross.fit) {
    psi <- if (loss == "AIPW") as.numeric(grf::get_scores(c.forest)) else NULL
    # OOB baseline (honest) keeps null inference conservative; permuted
    # predictions are necessarily non-OOB. Use cross.fit = TRUE for unbiased
    # cross-fitted inference.
    tau.base <- c.forest$predictions
    L0 <- .cf_perm_risk(tau.base, Y, m, W, pi, psi, loss)
    zc <- stats::qnorm(1 - (1 - conf.level) / 2)

    for (j in which(keep)) {
      Xperm <- .cf_perm_cp_sample(j, X, X, n.perm, seed = seed)
      dbar  <- numeric(n)
      for (k in seq_len(n.perm)) {
        Xk <- X
        Xk[, j] <- Xperm[, k]
        tau.k <- stats::predict(c.forest, Xk)$predictions
        Lk    <- .cf_perm_risk(tau.k, Y, m, W, pi, psi, loss)
        dbar  <- dbar + (Lk - L0) / 2
      }
      dbar    <- dbar / n.perm
      imp[j]  <- mean(dbar)
      se[j]   <- stats::sd(dbar) / sqrt(n)
      z[j]    <- imp[j] / se[j]
      pval[j] <- stats::pnorm(z[j], lower.tail = FALSE)
      ci.lo[j] <- imp[j] - zc * se[j]
      ci.hi[j] <- imp[j] + zc * se[j]
    }
  } else {
    cv <- .cf_perm_cv(c.forest, X, Y, W, keep, n.perm, num.folds, conf.level, seed)
    imp <- cv$imp; se <- cv$se; z <- cv$z
    pval <- cv$pval; ci.lo <- cv$ci.lo; ci.hi <- cv$ci.hi
  }

  # screened-out covariates: zero importance, non-significant
  imp[!keep]  <- 0
  pval[!keep] <- 1

  if (normalize) {
    impc <- ifelse(imp >= 0, imp, 0)
    imp  <- if (sum(impc) == 0) rep(1 / p, p) else impc / sum(impc)
    se[] <- z[] <- pval[] <- ci.lo[] <- ci.hi[] <- NA_real_
  }

  vimp <- data.frame(
    Variable = vnames, Importance = imp, SE = se, z = z,
    p.value = pval, CI.lower = ci.lo, CI.upper = ci.hi,
    stringsAsFactors = FALSE
  )

  structure(
    list(vimp = vimp, loss = loss, cross.fit = cross.fit, n.perm = n.perm,
         num.folds = if (cross.fit) num.folds else NA_integer_,
         normalized = normalize, conf.level = conf.level, n = n, p = p),
    class = "cf_perm"
  )
}
