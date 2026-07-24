#' Sorted Group Average Treatment Effects (GATES) for Causal Forests
#'
#' Estimates Chernozhukov et al. (2018) Sorted Group Average Treatment Effects
#' using a fitted \code{grf} causal forest. Units are sorted by the forest's
#' out-of-bag CATE predictions, partitioned into quantile groups, and group-level
#' average effects are estimated via the weighted GATES regression of Chernozhukov
#' et al. (2020).
#'
#' @param c.forest A fitted causal forest object from the \code{grf} package.
#' @param quantile.cutoffs Numeric vector of interior quantile cutoffs in
#'   \eqn{(0, 1)}, strictly increasing. Default \code{c(0.25, 0.5, 0.75)} yields
#'   four quartile groups (\code{G1}--\code{G4}). Group \code{G1} is the lowest
#'   predicted-CATE slice; \code{GK} is the highest.
#' @param HT Logical. If \code{TRUE}, apply the Horvitz-Thompson (GATES2)
#'   transformation from Chernozhukov et al. (2020). Default \code{FALSE}.
#' @param subtract.from Character scalar, either \code{"most"} (default) or
#'   \code{"least"}. Controls which group anchors differenced generic targets
#'   (e.g. \code{GK - G1} when \code{"most"} and \code{subtracted = 1}).
#' @param subtracted Integer vector of group indices to difference against the
#'   anchor group. Default \code{1} yields \code{GK - G1} when
#'   \code{subtract.from = "most"}.
#' @param monotonize Logical. If \code{TRUE} (default), rearrange group point
#'   estimates and confidence bounds to be monotonically increasing following
#'   Chernozhukov, Fernandez-Val, and Galichon (2009).
#' @param conf.level Confidence level for two-sided intervals. Default \code{0.95}.
#' @param seed Integer seed for the quantile-group tie-break path when many
#'   predicted CATEs are tied. Default \code{1995}.
#'
#' @return An object of class `"cf_gates"` with components:
#'   \describe{
#'     \item{groups}{Data frame of per-group GATES estimates (\code{G1..GK}) with
#'       \code{estimate}, \code{std.err}, \code{lower}, \code{upper},
#'       \code{p.left}, and \code{p.right}.}
#'     \item{diff}{Data frame of differenced generic targets (e.g. \code{G4-G1}).}
#'     \item{ate}{List from [grf::average_treatment_effect()] on the full sample
#'       (used by [plot_gates()] for the reference line).}
#'     \item{membership}{Logical matrix of group indicators (columns = group labels).}
#'     \item{quantile.cutoffs, HT, monotonize, conf.level, n, K}{Metadata.}
#'   }
#'
#' @details
#' **Proxies.** The GATES regression uses the forest's Robinson-residual nuisances
#' and OOB CATE predictions as GenericML proxies: \eqn{\hat m(X) =} \code{Y.hat},
#' \eqn{\hat e(X) =} \code{W.hat}, and \eqn{\hat S(X) =} OOB \code{predictions}.
#' The \eqn{X_1} block defaults to the baseline proxy \code{B} only (GenericML
#' default \code{setup_X1(funs_Z = "B")}).
#'
#' **Scope.** This is a single-forest, in-sample scoring path (like [cf_perm()]
#' or the BLP payload in [omni_hetero()]), not the full GenericML auxiliary/main
#' sample-splitting and median aggregation over many splits. Use for exploratory
#' heterogeneity summaries on an already-fitted forest.
#'
#' **Treatment.** Binary \code{W} only (\code{length(unique(W)) == 2}).
#'
#' @references
#' Chernozhukov, V., Demirer, M., Duflo, E., and Fernandez-Val, I. (2020).
#' Generic Machine Learning Inference on Heterogeneous Treatment Effects in
#' Randomized Experiments. \emph{Econometrica}, forthcoming.
#' \doi{10.3982/ECTA19303}
#'
#' Chernozhukov, V., Fernandez-Val, I., and Galichon, A. (2009). Improving
#' Point and Interval Estimators of Monotone Functions by Rearrangement.
#' \emph{Biometrika}, 96(3), 559--575. \doi{10.1093/biomet/asp030}
#'
#' @seealso [plot_gates()], [omni_hetero()], [grf::average_treatment_effect()]
#'
#' @import grf
#' @export
#' @examples
#' \donttest{
#' library(grf)
#' set.seed(1995)
#' n <- 400; p <- 5
#' X <- matrix(rnorm(n * p), n, p)
#' colnames(X) <- paste0("X", seq_len(p))
#' W <- rbinom(n, 1, 0.5)
#' Y <- X[, 1] * W + rnorm(n)
#' cf <- causal_forest(X, Y, W, num.trees = 200)
#' g <- gates(cf)
#' print(g)
#' }
gates <- function(c.forest,
                  quantile.cutoffs = c(0.25, 0.5, 0.75),
                  HT = FALSE,
                  subtract.from = c("most", "least"),
                  subtracted = 1,
                  monotonize = TRUE,
                  conf.level = 0.95,
                  seed = 1995) {

  if (!inherits(c.forest, "causal_forest")) {
    stop("c.forest must be a grf causal forest.", call. = FALSE)
  }

  subtract.from <- match.arg(subtract.from)
  if (!is.logical(HT) || length(HT) != 1L || is.na(HT)) {
    stop("HT must be a single logical value.", call. = FALSE)
  }
  if (!is.logical(monotonize) || length(monotonize) != 1L || is.na(monotonize)) {
    stop("monotonize must be a single logical value.", call. = FALSE)
  }
  if (!is.numeric(conf.level) || length(conf.level) != 1L ||
        !is.finite(conf.level) || conf.level <= 0 || conf.level >= 1) {
    stop("conf.level must be a single numeric in (0, 1).", call. = FALSE)
  }
  if (!is.numeric(quantile.cutoffs) || length(quantile.cutoffs) < 1L ||
        any(!is.finite(quantile.cutoffs)) ||
        any(quantile.cutoffs <= 0 | quantile.cutoffs >= 1) ||
        is.unsorted(quantile.cutoffs, strictly = TRUE)) {
    stop("quantile.cutoffs must be a strictly increasing numeric vector ",
         "with all values in (0, 1).", call. = FALSE)
  }
  if (!is.numeric(subtracted) || !all(subtracted == round(subtracted)) ||
        any(subtracted < 1)) {
    stop("subtracted must be a vector of positive integers.", call. = FALSE)
  }
  subtracted <- as.integer(subtracted)

  Y <- c.forest$Y.orig
  D <- c.forest$W.orig
  n <- length(Y)
  if (length(unique(D)) != 2L) {
    stop("gates() requires a binary treatment (length(unique(W)) == 2).",
         call. = FALSE)
  }

  propensity_scores <- c.forest$W.hat
  proxy_BCA <- c.forest$Y.hat
  proxy_CATE <- c.forest$predictions

  if (any(propensity_scores <= 0 | propensity_scores >= 1, na.rm = TRUE)) {
    stop("Propensity scores must lie strictly in (0, 1) for GATES weights. ",
         "Check overlap or refit with propensity clipping.", call. = FALSE)
  }

  membership <- .gates_quantile_group(proxy_CATE, quantile.cutoffs, seed = seed)
  K <- ncol(membership)

  if (any(subtracted >= K)) {
    stop("All entries of subtracted must be less than the number of groups (K = ",
         K, ").", call. = FALSE)
  }

  fit <- .gates_fit(
    Y = Y,
    D = D,
    propensity_scores = propensity_scores,
    proxy_BCA = proxy_BCA,
    proxy_CATE = proxy_CATE,
    membership = membership,
    HT = HT,
    subtract.from = subtract.from,
    subtracted = subtracted,
    monotonize = monotonize,
    conf.level = conf.level
  )

  ate <- grf::average_treatment_effect(c.forest)
  z_ate <- stats::qnorm(1 - (1 - conf.level) / 2)
  ate_ci <- list(
    estimate = ate[["estimate"]],
    std.err  = ate[["std.err"]],
    lower    = ate[["estimate"]] - z_ate * ate[["std.err"]],
    upper    = ate[["estimate"]] + z_ate * ate[["std.err"]]
  )

  structure(
    list(
      groups = fit$groups,
      diff = fit$diff,
      ate = ate_ci,
      membership = membership,
      quantile.cutoffs = quantile.cutoffs,
      HT = HT,
      monotonize = monotonize,
      conf.level = conf.level,
      subtract.from = subtract.from,
      subtracted = subtracted,
      n = n,
      K = K
    ),
    class = "cf_gates"
  )
}


# -- internal helpers ---------------------------------------------------------

#' @keywords internal
#' @noRd
.gates_breaks_format <- function(breaks, dig.lab = 3L) {
  nb <- length(breaks)
  for (dig in dig.lab:max(12L, dig.lab)) {
    ch.br <- formatC(0 + breaks, digits = dig, width = 1L)
    if (all(ch.br[-1L] != ch.br[-nb])) break
  }
  ch.br
}

#' Quantile groups for GATES (adapted from GenericML quantile_group).
#' @keywords internal
#' @noRd
.gates_quantile_group <- function(x, cutoffs, seed = 1995) {
  num_groups <- length(cutoffs) + 1L
  n <- length(x)
  group_size_min <- 2L
  grouping_unfinished <- TRUE
  ct <- 0L
  set.seed(seed)

  while (grouping_unfinished) {
    q <- stats::quantile(x, cutoffs, na.rm = TRUE)
    qnam <- .gates_breaks_format(q)
    out <- matrix(NA, n, num_groups)
    legal_grouping <- rep(TRUE, num_groups)
    groupnam <- rep(NA_character_, num_groups)

    for (k in seq_len(num_groups)) {
      if (k == 1L) {
        bool_k <- x < q[k]
        groupnam[k] <- paste0("(-Inf, ", qnam[k], ")")
      } else if (k == num_groups) {
        bool_k <- x >= q[k - 1L]
        groupnam[k] <- paste0("[", qnam[k - 1L], ", Inf)")
      } else {
        bool_k <- q[k - 1L] <= x & x < q[k]
        groupnam[k] <- paste0("[", qnam[k - 1L], ", ", qnam[k], ")")
      }
      if (sum(bool_k) < group_size_min) legal_grouping[k] <- FALSE
      out[, k] <- bool_k
    }

    colnames(out) <- groupnam
    if (all(legal_grouping)) {
      grouping_unfinished <- FALSE
    } else {
      x <- x + stats::rnorm(n, sd = 0.001)
    }
    ct <- ct + 1L
    if (ct > 3L) {
      stop("quantile.cutoffs do not yield groups with at least two ",
           "observations each. Use fewer or more interior cutoffs.",
           call. = FALSE)
    }
  }

  out
}

#' @keywords internal
#' @noRd
.gates_x1_block <- function(proxy_BCA, proxy_CATE, propensity_scores) {
  Z_mat <- cbind(B = proxy_BCA, S = proxy_CATE, p = propensity_scores)
  as.data.frame(Z_mat[, "B", drop = FALSE])
}

#' @keywords internal
#' @noRd
.gates_generic_targets <- function(coef_mat, vcov_gamma, K,
                                   subtract.from, subtracted,
                                   monotonize, conf.level) {
  gammanam <- paste0("gamma.", seq_len(K))
  z <- stats::qnorm(1 - (1 - conf.level) / 2)

  estimates <- coef_mat[gammanam, "Estimate", drop = TRUE]
  ses <- coef_mat[gammanam, "Std. Error", drop = TRUE]
  zvals <- coef_mat[gammanam, "z value", drop = TRUE]

  p.left  <- stats::pnorm(zvals, lower.tail = TRUE)
  p.right <- stats::pnorm(zvals, lower.tail = FALSE)
  ci.lo <- estimates - z * ses
  ci.up <- estimates + z * ses

  diff_rows <- lapply(subtracted, function(j) {
    if (subtract.from == "least") {
      diff_est <- estimates[1L] - estimates[j]
      diff_lab <- paste0("G1-G", j)
      v11 <- vcov_gamma[1L, 1L]
      vjj <- vcov_gamma[j, j]
      v1j <- vcov_gamma[1L, j]
    } else {
      diff_est <- estimates[K] - estimates[j]
      diff_lab <- paste0("G", K, "-G", j)
      v11 <- vcov_gamma[K, K]
      vjj <- vcov_gamma[j, j]
      v1j <- vcov_gamma[K, j]
    }
    diff_se <- sqrt(v11 + vjj - 2 * v1j)
    zstat <- diff_est / diff_se
    data.frame(
      group = diff_lab,
      estimate = unname(diff_est),
      std.err = diff_se,
      lower = unname(diff_est) - z * diff_se,
      upper = unname(diff_est) + z * diff_se,
      z = zstat,
      p.left = stats::pnorm(zstat, lower.tail = TRUE),
      p.right = stats::pnorm(zstat, lower.tail = FALSE),
      stringsAsFactors = FALSE
    )
  })
  diff_df <- do.call(rbind, diff_rows)

  if (isTRUE(monotonize)) {
    ord <- order(estimates, decreasing = FALSE)
    nam <- names(estimates)
    estimates <- estimates[ord]
    ses <- ses[ord]
    zvals <- zvals[ord]
    p.left <- structure(p.left[ord], names = nam)
    p.right <- structure(p.right[ord], names = nam)
    ci.lo <- structure(sort(ci.lo, decreasing = FALSE), names = nam)
    ci.up <- structure(sort(ci.up, decreasing = FALSE), names = nam)
    names(estimates) <- nam[ord]
    vcov_gamma <- vcov_gamma[ord, ord, drop = FALSE]
    rownames(vcov_gamma) <- colnames(vcov_gamma) <- gammanam
  }

  group_labels <- paste0("G", seq_len(K))
  groups_df <- data.frame(
    group = group_labels,
    estimate = as.numeric(estimates),
    std.err = as.numeric(ses),
    lower = as.numeric(ci.lo),
    upper = as.numeric(ci.up),
    z = as.numeric(zvals),
    p.left = as.numeric(p.left),
    p.right = as.numeric(p.right),
    stringsAsFactors = FALSE
  )

  list(
    groups = groups_df,
    diff = diff_df,
    vcov_gamma = vcov_gamma
  )
}

#' @keywords internal
#' @noRd
.gates_coef_table <- function(lm_obj, gammanam) {
  sm <- summary(lm_obj)$coefficients
  rows <- intersect(rownames(sm), gammanam)
  out <- sm[rows, c("Estimate", "Std. Error", "t value"), drop = FALSE]
  colnames(out) <- c("Estimate", "Std. Error", "z value")
  out
}

#' @keywords internal
#' @noRd
.gates_fit <- function(Y, D, propensity_scores, proxy_BCA, proxy_CATE,
                       membership, HT, subtract.from, subtracted,
                       monotonize, conf.level) {

  groups <- 1 * membership
  K <- ncol(groups)
  gammanam <- paste0("gamma.", seq_len(K))

  if (!HT) {
    weights <- 1 / (propensity_scores * (1 - propensity_scores))
    X1 <- .gates_x1_block(proxy_BCA, proxy_CATE, propensity_scores)
    X <- data.frame(X1, (D - propensity_scores) * groups)
    colnames(X) <- c(colnames(X1), gammanam)
    gates.obj <- stats::lm(Y ~ ., data = data.frame(Y = Y, X), weights = weights)
  } else {
    H <- (D - propensity_scores) / (propensity_scores * (1 - propensity_scores))
    X1 <- .gates_x1_block(proxy_BCA, proxy_CATE, propensity_scores)
    X1H <- X1 * H
    colnames(X1H) <- paste0(colnames(X1), ".H")
    X <- data.frame(X1H, groups)
    colnames(X) <- c(colnames(X1H), gammanam)
    form <- stats::as.formula(
      paste0("YH ~ ", paste(colnames(X), collapse = " + "), " + 0")
    )
    gates.obj <- stats::lm(form, data = data.frame(YH = Y * H, X))
  }

  vcov_mat <- stats::vcov(gates.obj)
  vcov_gamma <- vcov_mat[gammanam, gammanam, drop = FALSE]
  coef_mat <- .gates_coef_table(gates.obj, gammanam)

  .gates_generic_targets(
    coef_mat = coef_mat,
    vcov_gamma = vcov_gamma,
    K = K,
    subtract.from = subtract.from,
    subtracted = subtracted,
    monotonize = monotonize,
    conf.level = conf.level
  )
}
