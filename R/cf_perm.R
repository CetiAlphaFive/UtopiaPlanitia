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
#
# `obs.support = FALSE` runs the original body verbatim (complete-data default;
# probability_forest discrete branch included). `obs.support = TRUE` runs the
# NA-aware, observed-support body: nu_hat_j is fit on observed-label rows only
# (X_{-j} NAs are routed via grf's MIA), perturbed values are spliced back into
# observed apply rows, and NA apply rows are left NA so their loss delta is 0.
# A binary observed support draws Bernoulli(p_hat) on {lo, hi} from a
# regression_forest (probability_forest is never reached on this branch); other
# supports residual-shuffle. Columns with fewer than `min.obs` observed labels
# return an all-NA matrix for the caller to degrade to importance 0 / p 1.
.cf_perm_cp_sample <- function(j, Xfit, Xapply, n.perm, disc.max = 10L,
                               seed = 1995, obs.support = FALSE, min.obs = 5L) {
  xj.fit <- Xfit[, j]
  Xm.fit <- Xfit[, -j, drop = FALSE]
  xj.app <- Xapply[, j]
  Xm.app <- Xapply[, -j, drop = FALSE]
  na <- nrow(Xapply)

  if (!obs.support) {
    ## ---- ORIGINAL PATH (unchanged): probability_forest for discrete ----
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
    return(out)
  }

  ## ---- OBSERVED-SUPPORT PATH: regression_forest, NA-aware ----
  obs.fit <- !is.na(xj.fit)
  obs.app <- !is.na(xj.app)
  out <- matrix(NA_real_, na, n.perm)            # NA rows stay NA by construction
  if (sum(obs.fit) < min.obs) return(out)        # degenerate -> caller sets j to 0

  xj.o    <- xj.fit[obs.fit]
  lev.o   <- sort(unique(xj.o))
  is.bin  <- length(lev.o) == 2L && all(lev.o == round(lev.o))
  idx.app <- which(obs.app)

  if (is.bin) {
    lo <- lev.o[1]; hi <- lev.o[2]
    rf <- grf::regression_forest(Xm.fit[obs.fit, , drop = FALSE],
                                 as.numeric(xj.o == hi), seed = seed)
    p  <- pmin(pmax(stats::predict(rf, Xm.app[obs.app, , drop = FALSE])$predictions, 0), 1)
    for (k in seq_len(n.perm)) {
      out[idx.app, k] <- ifelse(stats::runif(length(p)) < p, hi, lo)
    }
  } else {
    rf <- grf::regression_forest(Xm.fit[obs.fit, , drop = FALSE], xj.o, seed = seed)
    nu <- stats::predict(rf, Xm.app[obs.app, , drop = FALSE])$predictions
    e  <- xj.app[obs.app] - nu
    no <- length(e)
    for (k in seq_len(n.perm)) {
      out[idx.app, k] <- nu + e[sample.int(no)]
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
#' @param allow.missing Controls how missing covariate values are handled.
#'   \code{FALSE} (default) errors if \code{X} contains any \code{NA}, instructing
#'   the user to choose a scope; with complete data it is inert and the function
#'   runs exactly as before. Set to \code{"observed"} or \code{"marginal"} to
#'   opt into observed-support conditional permutation: each covariate is scored
#'   on its observed rows (rows with \code{NA} in \eqn{X_j} pass through the
#'   forest's MIA routing and contribute a per-row loss delta of exactly zero).
#'   The two scopes differ only in the estimand: \code{"observed"} averages the
#'   per-row importance over observed rows only (importance conditional on
#'   \eqn{X_j} observed, no missingness discount), while \code{"marginal"}
#'   averages over all \eqn{n} rows so the score is auto-discounted by the
#'   covariate's missingness rate. With complete data the two scopes coincide and
#'   reproduce the default behavior.
#'
#' @return An object of class \code{"cf_perm"} with components \code{vimp} (a data
#'   frame with columns \code{Variable}, \code{Importance}, \code{SE}, \code{z},
#'   \code{p.value}, \code{CI.lower}, \code{CI.upper}), \code{loss},
#'   \code{cross.fit}, \code{n.perm}, \code{num.folds}, \code{normalized},
#'   \code{conf.level}, \code{n}, \code{p}, and \code{miss.rate} (a named numeric
#'   vector of length \code{p} giving the per-covariate missingness rate,
#'   \code{colMeans(is.na(X))}; all zeros when \code{X} is complete).
#'
#' @details
#' **Light path vs. cross-fitting.** With \code{cross.fit = FALSE} (default),
#' importance is scored on the supplied forest using its out-of-bag predictions
#' as the risk baseline. This keeps the test conservative, but the standard
#' errors are an influence-function approximation that assumes an independence
#' the in-sample re-scoring does not strictly provide, so the light-path
#' p-values should be read as approximate. For calibrated inference use
#' \code{cross.fit = TRUE}, which refits the nuisances and CATE per fold and
#' applies the Nadeau-Bengio corrected \eqn{t} test (with \code{num.folds - 1}
#' degrees of freedom; use \code{num.folds = 5} or more for a less conservative
#' reference).
#'
#' **Unsupported designs.** Clustered causal forests are not yet supported
#' (an error is raised); \code{sample.weights} are ignored and importances are
#' computed unweighted (a warning is raised).
#'
#' **Missing covariate values.** \code{grf} forests route \code{NA} natively via
#' MIA (Missingness Incorporated in Attributes), but the conditional-permutation
#' nuisance \eqn{\hat\nu_j = E[X_j \mid X_{-j}]} cannot use an \code{NA} as its
#' regression \emph{label}. With \code{allow.missing} set to \code{"observed"} or
#' \code{"marginal"}, \eqn{\hat\nu_j} is fit on the observed-label rows only
#' (\eqn{X_{-j}} missingness is still routed via MIA, never imputed), and
#' perturbed values are spliced back into observed rows. A unit with \code{NA} in
#' \eqn{X_j} has a CATE prediction that is invariant to permuting \eqn{X_j} (MIA
#' routes it identically regardless of the imputed value), so its true importance
#' contribution is exactly zero in \emph{both} paths: naturally under
#' \code{cross.fit = TRUE} (baseline and permuted risks both come from the
#' held-out-fold forest, so the per-row delta cancels), and by construction in
#' the light path (where the OOB-vs-in-sample baseline artifact is zeroed out for
#' those rows). Consequently \code{"observed"} reports the undiscounted
#' observed-support importance, while \code{"marginal"} is exactly that importance
#' discounted by the covariate's missingness rate. On this branch (and only on
#' this branch) the discrete conditional model switches from a
#' \code{probability_forest} to a \code{regression_forest}: a binary covariate
#' draws \eqn{\mathrm{Bernoulli}(\hat p)} on its two observed levels, while
#' continuous or multi-level supports residual-shuffle. This switch is scoped
#' entirely to the opt-in missingness branch, so complete-data results are
#' byte-for-byte unchanged. A covariate with fewer than \code{min.obs} (5)
#' observed values degrades to importance 0 / p-value 1 with a warning. The
#' light-path standard errors remain an influence-function approximation (see
#' above); under \code{"marginal"} the exact-zero \code{NA} rows additionally
#' shrink the SE, so those p-values are importance-conditional-on-design. Because
#' cross-variable magnitude comparison is only fair after accounting for
#' per-variable missingness, \code{print}/\code{summary} render a per-covariate
#' missingness table whenever any covariate has missing values.
#'
#' @note AIPW importance magnitudes (\code{loss = "AIPW"}) are not on a scale
#' comparable to the R-loss and should be read ordinally (ranking and
#' significance), because \code{grf::get_scores()} bakes the forest's own CATE
#' estimate into the pseudo-outcome. The default \code{loss = "R"} is
#' recommended when magnitudes are to be interpreted. AIPW importances are also
#' systematically larger than the R-loss and penalize noise covariates more
#' heavily, so AIPW and R-loss magnitudes are not directly comparable.
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
                    verbose = TRUE, allow.missing = FALSE) {

  loss <- match.arg(loss)
  if (!identical(allow.missing, FALSE)) {
    allow.missing <- match.arg(allow.missing, c("observed", "marginal"))
  }
  if (!inherits(c.forest, "causal_forest")) {
    stop("c.forest must be a grf causal forest.")
  }
  if (anyNA(c.forest$X.orig) && identical(allow.missing, FALSE)) {
    stop(
      "cf_perm: X contains missing values.\n",
      "  cf_perm scores importance on the observed support of each covariate and lets\n",
      "  NA rows pass through to the forest's MIA routing. Set `allow.missing` to choose\n",
      "  how rows with missing X_j are averaged:\n",
      '    "observed" - average over observed rows only; importance conditional on X_j\n',
      "                 observed (no missingness discount).\n",
      '    "marginal" - average over all rows; NA rows contribute exactly 0, so the score\n',
      "                 is auto-discounted by the covariate's missingness rate.\n",
      "  See ?cf_perm.",
      call. = FALSE
    )
  }
  if (cross.fit && loss == "AIPW") {
    stop("cross.fit = TRUE is only supported with loss = \"R\" in this version.")
  }
  cl <- c.forest$clusters
  if (!is.null(cl) && length(cl) > 0L && length(unique(cl)) >= 1L) {
    stop("cf_perm does not yet support clustered causal forests.")
  }
  if (!is.null(c.forest$sample.weights)) {
    warning("cf_perm ignores sample.weights; importances are unweighted.",
            call. = FALSE)
  }
  set.seed(seed)

  X <- c.forest$X.orig
  Y <- c.forest$Y.orig
  W <- c.forest$W.orig
  m  <- c.forest$Y.hat
  pi <- c.forest$W.hat
  n  <- nrow(X)
  p  <- ncol(X)

  # Active observed-support flags. When X is complete, use.obs = FALSE and every
  # path reduces to the original (complete-data) behavior.
  have.na <- anyNA(X)
  use.obs <- have.na && !identical(allow.missing, FALSE)
  scope   <- if (use.obs) allow.missing else NA_character_
  if (p < 2L) {
    stop("cf_perm requires at least 2 covariates (conditional permutation of X_j needs X_{-j}).")
  }
  if (cross.fit) {
    if (num.folds < 2L || num.folds >= n) {
      stop("num.folds must be between 2 and n - 1 when cross.fit = TRUE.")
    }
    if (length(unique(W)) <= 10L && num.folds > min(table(W))) {
      stop("num.folds (", num.folds, ") cannot exceed the smaller treatment-group ",
           "size (", min(table(W)), ") for stratified cross-fitting.")
    }
  }
  vnames <- colnames(X)
  if (is.null(vnames)) vnames <- paste0("V", seq_len(p))

  miss.rate <- colMeans(is.na(X))
  names(miss.rate) <- vnames

  # --- screening (split-frequency importance) ---
  keep <- rep(TRUE, p)
  if (!isFALSE(screen)) {
    if (!isTRUE(screen) && !(is.numeric(screen) && length(screen) == 1L &&
                             screen == as.integer(screen) && screen >= 1L)) {
      stop("screen must be FALSE, TRUE, or a positive integer.")
    }
    if (is.numeric(screen) && screen > p) {
      stop("screen must be no greater than the number of covariates (", p, ").")
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
    zc <- stats::qnorm(conf.level)

    degen <- character(0)
    for (j in which(keep)) {
      Xperm <- .cf_perm_cp_sample(j, X, X, n.perm, seed = seed,
                                  obs.support = use.obs)
      if (all(is.na(Xperm))) {
        # Too few observed values to permute: degrade to importance 0, p 1.
        degen   <- c(degen, vnames[j])
        imp[j]  <- 0
        pval[j] <- 1
        next
      }
      dbar  <- numeric(n)
      for (k in seq_len(n.perm)) {
        Xk <- X
        Xk[, j] <- Xperm[, k]
        tau.k <- stats::predict(c.forest, Xk)$predictions
        Lk    <- .cf_perm_risk(tau.k, Y, m, W, pi, psi, loss)
        dbar  <- dbar + (Lk - L0) / 2
      }
      dbar    <- dbar / n.perm
      obs <- if (use.obs) !is.na(X[, j]) else rep(TRUE, n)
      # An NA-X_j unit's prediction is invariant to permuting X_j (MIA routes it
      # identically), so its true importance contribution is exactly 0; the
      # nonzero dbar it would otherwise carry is only the OOB-vs-in-sample
      # baseline artifact (L0 uses the OOB tau.base, Lk uses the in-sample
      # predict). Force it to 0 so "marginal" is an exact auto-discount of the
      # observed-support importance by the missingness rate, while "observed" is
      # unaffected (those rows are already excluded from its average via sel).
      if (use.obs) dbar[!obs] <- 0
      sel <- if (identical(scope, "observed")) obs else rep(TRUE, n)
      imp[j]  <- mean(dbar[sel])
      se[j]   <- stats::sd(dbar[sel]) / sqrt(sum(sel))
      z[j]    <- imp[j] / se[j]
      pval[j] <- stats::pnorm(z[j], lower.tail = FALSE)
      ci.lo[j] <- imp[j] - zc * se[j]
      ci.hi[j] <- Inf
    }
    if (length(degen)) {
      warning("cf_perm: covariate(s) ", paste(degen, collapse = ", "),
              " had fewer than the minimum observed values to permute; ",
              "importance set to 0 and p-value to 1.", call. = FALSE)
    }
  } else {
    cv <- .cf_perm_cv(c.forest, X, Y, W, keep, n.perm, num.folds, conf.level,
                      seed, verbose, use.obs = use.obs, scope = scope)
    imp <- cv$imp; se <- cv$se; z <- cv$z
    pval <- cv$pval; ci.lo <- cv$ci.lo; ci.hi <- cv$ci.hi
  }

  # screened-out covariates: zero importance, non-significant
  imp[!keep]  <- 0
  pval[!keep] <- 1

  if (normalize) {
    impc <- ifelse(imp >= 0, imp, 0)
    if (sum(impc) == 0) {
      warning("cf_perm: all non-negative importances are zero; returning ",
              "uniform importance over screened-in covariates.", call. = FALSE)
      imp[keep]  <- 1 / sum(keep)
      imp[!keep] <- 0
    } else {
      imp <- impc / sum(impc)
    }
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
         normalized = normalize, conf.level = conf.level, n = n, p = p,
         miss.rate = miss.rate, miss.scope = scope),
    class = "cf_perm"
  )
}

# Fold ids: stratified on a discrete treatment, simple random for continuous W.
.cf_perm_folds <- function(W, num.folds) {
  n <- length(W)
  fold <- integer(n)
  uW <- unique(W)
  if (length(uW) <= 10L) {
    for (lev in uW) {
      idx <- which(W == lev)
      fold[idx] <- sample(rep_len(seq_len(num.folds), length(idx)))
    }
  } else {
    fold <- sample(rep_len(seq_len(num.folds), n))
  }
  fold
}

# Cross-fit PermuCATE (R-loss). Returns named list of per-covariate vectors.
.cf_perm_cv <- function(c.forest, X, Y, W, keep, n.perm, num.folds,
                        conf.level, seed, verbose = TRUE,
                        use.obs = FALSE, scope = NA_character_) {
  n <- nrow(X)
  p <- ncol(X)
  fold <- .cf_perm_folds(W, num.folds)

  Psi <- matrix(NA_real_, num.folds, p)   # per-fold, per-covariate importance
  n1  <- numeric(num.folds)               # train sizes
  n2  <- numeric(num.folds)               # test sizes

  for (f in seq_len(num.folds)) {
    if (verbose) message("cf_perm cross-fit: fold ", f, " of ", num.folds)
    tr <- which(fold != f)
    te <- which(fold == f)
    n1[f] <- length(tr)
    n2[f] <- length(te)

    cf.k <- grf::causal_forest(
      X[tr, , drop = FALSE], Y[tr], W[tr],
      num.trees             = c.forest$`_num_trees`,
      sample.fraction       = c.forest$tunable.params$sample.fraction,
      mtry                  = c.forest$tunable.params$mtry,
      min.node.size         = c.forest$tunable.params$min.node.size,
      honesty.fraction      = c.forest$tunable.params$honesty.fraction,
      honesty.prune.leaves  = c.forest$tunable.params$honesty.prune.leaves,
      alpha                 = c.forest$tunable.params$alpha,
      imbalance.penalty     = c.forest$tunable.params$imbalance.penalty,
      ci.group.size         = c.forest$ci.group.size,
      seed                  = seed
    )
    m.te  <- stats::predict(grf::regression_forest(X[tr, , drop = FALSE], Y[tr], seed = seed),
                            X[te, , drop = FALSE])$predictions
    pi.te <- stats::predict(grf::regression_forest(X[tr, , drop = FALSE], W[tr], seed = seed),
                            X[te, , drop = FALSE])$predictions
    tau.te <- stats::predict(cf.k, X[te, , drop = FALSE])$predictions
    L0.te  <- .cf_perm_risk(tau.te, Y[te], m.te, W[te], pi.te, NULL, "R")

    Xte <- X[te, , drop = FALSE]
    Xtr <- X[tr, , drop = FALSE]
    for (j in which(keep)) {
      Xperm <- .cf_perm_cp_sample(j, Xtr, Xte, n.perm, seed = seed,
                                  obs.support = use.obs)
      if (all(is.na(Xperm))) {
        # Too few observed train labels for j in this fold: leave NA, aggregate
        # with na.rm. A covariate degenerate in every fold is handled below.
        Psi[f, j] <- NA_real_
        next
      }
      dbar  <- numeric(length(te))
      for (k in seq_len(n.perm)) {
        Xk <- Xte
        Xk[, j] <- Xperm[, k]
        tau.k <- stats::predict(cf.k, Xk)$predictions
        Lk    <- .cf_perm_risk(tau.k, Y[te], m.te, W[te], pi.te, NULL, "R")
        dbar  <- dbar + (Lk - L0.te) / 2
      }
      # Scope-aware averaging over the test fold (only matters when use.obs).
      sel.te <- if (identical(scope, "observed")) {
        !is.na(Xte[, j])
      } else {
        rep(TRUE, length(te))
      }
      Psi[f, j] <- if (any(sel.te)) mean((dbar / n.perm)[sel.te]) else NA_real_
    }
  }

  # na.rm honors fold-degenerate covariates under observed-support. With complete
  # data no kept column carries NA, so this reduces to colMeans(Psi)/var(Psi).
  imp <- colMeans(Psi, na.rm = TRUE)
  # Nadeau-Bengio corrected variance of the cross-validated mean. The corrected
  # resampled statistic is a t with (num.folds - 1) df, so reference t, not normal.
  rho <- mean(n2) / mean(n1)
  s2  <- apply(Psi, 2L, stats::var, na.rm = TRUE)
  se  <- sqrt((1 / num.folds + rho) * s2)
  z   <- imp / se
  zero.se <- !is.na(se) & se == 0
  if (any(zero.se)) {
    warning("cf_perm: zero cross-fit SE for some covariates (identical fold ",
            "importances); their z and p-values are set to NA. Use more folds ",
            "or a larger sample.", call. = FALSE)
    z[zero.se] <- NA_real_
  }
  df.nb <- num.folds - 1L
  pval  <- stats::pt(z, df = df.nb, lower.tail = FALSE)
  tc    <- stats::qt(conf.level, df = df.nb)
  ci.lo <- imp - tc * se
  ci.hi <- rep(Inf, length(imp))
  ci.hi[is.na(se)] <- NA_real_

  # Covariate degenerate in EVERY fold (all-NA Psi column among kept) -> 0 / p 1.
  vn <- colnames(X)
  if (is.null(vn)) vn <- paste0("V", seq_len(ncol(X)))
  degen <- which(keep & apply(Psi, 2L, function(col) all(is.na(col))))
  if (length(degen)) {
    warning("cf_perm: covariate(s) ", paste(vn[degen], collapse = ", "),
            " had fewer than the minimum observed values in every fold; ",
            "importance set to 0 and p-value to 1.", call. = FALSE)
    imp[degen]   <- 0
    pval[degen]  <- 1
    se[degen]    <- NA_real_
    z[degen]     <- NA_real_
    ci.lo[degen] <- NA_real_
    ci.hi[degen] <- NA_real_
  }

  list(imp = imp, se = se, z = z, pval = pval, ci.lo = ci.lo, ci.hi = ci.hi)
}
