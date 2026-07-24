#' Sorted Group Average Treatment Effects (GATES) for Causal Forests
#'
#' Estimates Chernozhukov et al. (2018) Sorted Group Average Treatment Effects
#' using a fitted \code{grf} causal forest. Units are sorted by predicted CATE
#' (out-of-bag by default, or cross-fitted when \code{cross.fit = TRUE}),
#' partitioned into quantile groups, and group-level effects are estimated with
#' [grf::average_treatment_effect()] on each subset. Differenced targets use the
#' same group table (after optional monotonization). When \code{HT = TRUE}, the
#' Horvitz-Thompson GATES regression path is used instead (no grf subgroup ATEs).
#'
#' @param c.forest A fitted causal forest object from the \code{grf} package.
#' @param quantile.cutoffs Numeric vector of interior quantile cutoffs in
#'   \eqn{(0, 1)}, strictly increasing. Default \code{c(0.25, 0.5, 0.75)} yields
#'   four quartile groups (\code{G1}--\code{G4}). With \code{monotonize = FALSE},
#'   \code{G1} is the lowest predicted-CATE quantile slice and \code{GK} the highest.
#'   With \code{monotonize = TRUE}, labels are rearranged so \code{G1} is the
#'   smallest group effect and \code{GK} the largest (GenericML convention).
#' @param HT Logical. If \code{TRUE}, estimate groups via the Horvitz-Thompson
#'   GATES regression (GATES2) instead of grf subgroup ATEs. Default \code{FALSE}.
#' @param cross.fit Logical. If \code{TRUE}, form CATE groups with a 50/50
#'   cross-fit (forest on A, group by predicted CATE on B, ATEs on B with a
#'   forest trained only on B) instead of OOB predictions from \code{c.forest}.
#'   Default \code{FALSE}. Ignored when \code{HT = TRUE}.
#' @param subtract.from Character scalar, either \code{"most"} (default) or
#'   \code{"least"}. Controls which group anchors differenced generic targets.
#' @param subtracted Integer vector of group indices to difference against the
#'   anchor group. Default \code{1} yields \code{GK - G1} when
#'   \code{subtract.from = "most"}.
#' @param monotonize Logical. If \code{TRUE} (default), rearrange group point
#'   estimates and confidence bounds to be monotonically increasing following
#'   Chernozhukov, Fernandez-Val, and Galichon (2009). Differenced targets are
#'   computed **after** monotonization so they match the printed/plotted table.
#' @param conf.level Confidence level for two-sided intervals. Default \code{0.95}.
#' @param seed Integer seed for fold assignment and quantile-group tie-breaks.
#'   Default \code{1995}.
#'
#' @return An object of class `"cf_gates"` with components:
#'   \describe{
#'     \item{groups}{Data frame per \code{G1..GK}: grf subgroup \code{estimate} /
#'       \code{std.err} (or GATES regression when \code{HT = TRUE}), CIs,
#'       one-sided \code{p.left}, \code{p.right}, and GenericML-style adjusted
#'       \code{p.left.adj}, \code{p.right.adj}.}
#'     \item{diff}{Differenced targets consistent with \code{groups}.}
#'     \item{ate}{Overall ATE from [grf::average_treatment_effect()] (reference line).}
#'     \item{membership}{Logical group matrix (full \code{n} when \code{cross.fit = FALSE},
#'       evaluation half only when \code{cross.fit = TRUE}).}
#'     \item{cross.fit, HT, monotonize, conf.level, quantile.cutoffs, n, K, eval.n}{Metadata.}
#'   }
#'
#' @details
#' **Group effects (default path).** Each \code{Gk} reports
#' \code{grf::average_treatment_effect(c.forest, subset = G_k)} — doubly robust
#' subgroup ATEs with grf's analytical standard errors (honoring \code{clusters}
#' and \code{sample.weights} stored on the forest). These are not identical to
#' the GATES regression \eqn{\gamma_k} coefficients, but share the same quantile
#' grouping by predicted CATE and are the package-native way to attach valid grf
#' inference to sorted bins.
#'
#' **OOB vs cross-fit.** By default, groups use OOB \code{predictions} from the
#' supplied forest (same-sample scoring; exploratory). \code{cross.fit = TRUE}
#' separates grouping from ATE estimation to reduce winner's-curse optimism on
#' contrasts such as \code{GK - G1}.
#'
#' **Treatment.** Binary \code{W} coded \code{0/1} only.
#'
#' @references
#' Chernozhukov, V., Demirer, M., Duflo, E., and Fernandez-Val, I. (2020).
#' Generic Machine Learning Inference on Heterogeneous Treatment Effects in
#' Randomized Experiments. \emph{Econometrica}, forthcoming.
#' \doi{10.3982/ECTA19303}
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
                  cross.fit = FALSE,
                  subtract.from = c("most", "least"),
                  subtracted = 1,
                  monotonize = TRUE,
                  conf.level = 0.95,
                  seed = 1995) {

  if (!inherits(c.forest, "causal_forest")) {
    stop("c.forest must be a grf causal forest.", call. = FALSE)
  }

  subtract.from <- match.arg(subtract.from)
  .gates_check_args(HT, cross.fit, monotonize, conf.level,
                    quantile.cutoffs, subtracted)

  W <- c.forest$W.orig
  n <- length(c.forest$Y.orig)
  .gates_check_binary_w(W)

  if (isTRUE(HT) && isTRUE(cross.fit)) {
    warning("cross.fit is ignored when HT = TRUE (GATES regression path).",
            call. = FALSE)
  }

  set.seed(seed)

  if (isTRUE(HT)) {
    fit <- .gates_fit_regression(
      c.forest = c.forest,
      quantile.cutoffs = quantile.cutoffs,
      proxy_CATE = c.forest$predictions,
      subtract.from = subtract.from,
      subtracted = subtracted,
      monotonize = monotonize,
      conf.level = conf.level,
      HT = TRUE
    )
    membership <- fit$membership
    eval_n <- n
  } else if (isTRUE(cross.fit)) {
    fit <- .gates_crossfit(
      c.forest = c.forest,
      quantile.cutoffs = quantile.cutoffs,
      subtract.from = subtract.from,
      subtracted = subtracted,
      monotonize = monotonize,
      conf.level = conf.level,
      seed = seed
    )
    membership <- fit$membership
    eval_n <- fit$eval.n
  } else {
    propensity_scores <- c.forest$W.hat
    if (any(propensity_scores <= 0 | propensity_scores >= 1, na.rm = TRUE)) {
      stop("Propensity scores must lie strictly in (0, 1). ",
           "Check overlap or refit with propensity clipping.", call. = FALSE)
    }
    membership <- .gates_quantile_group(c.forest$predictions, quantile.cutoffs)
    fit <- .gates_fit_grf(
      c.forest = c.forest,
      membership = membership,
      subtract.from = subtract.from,
      subtracted = subtracted,
      monotonize = monotonize,
      conf.level = conf.level
    )
    eval_n <- n
  }

  K <- ncol(membership)
  if (any(subtracted >= K)) {
    stop("All entries of subtracted must be less than the number of groups (K = ",
         K, ").", call. = FALSE)
  }

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
      cross.fit = isTRUE(cross.fit) && !isTRUE(HT),
      monotonize = monotonize,
      conf.level = conf.level,
      subtract.from = subtract.from,
      subtracted = subtracted,
      n = n,
      eval.n = eval_n,
      K = K
    ),
    class = "cf_gates"
  )
}


# -- internal helpers ---------------------------------------------------------

#' @keywords internal
#' @noRd
.gates_check_args <- function(HT, cross.fit, monotonize, conf.level,
                              quantile.cutoffs, subtracted) {
  if (!is.logical(HT) || length(HT) != 1L || is.na(HT)) {
    stop("HT must be a single logical value.", call. = FALSE)
  }
  if (!is.logical(cross.fit) || length(cross.fit) != 1L || is.na(cross.fit)) {
    stop("cross.fit must be a single logical value.", call. = FALSE)
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
}

#' @keywords internal
#' @noRd
.gates_check_binary_w <- function(W) {
  Wu <- sort(unique(W[!is.na(W)]))
  if (length(Wu) != 2L || !all(Wu == c(0, 1))) {
    stop("gates() requires a binary treatment coded as 0 and 1 ",
         "(got unique non-NA values: ",
         paste(Wu, collapse = ", "), ").", call. = FALSE)
  }
}

#' @keywords internal
#' @noRd
.gates_adjust_p <- function(p) {
  pmin(1, 2 * p)
}

#' @keywords internal
#' @noRd
.gates_add_pcols <- function(df) {
  df$p.left.adj  <- .gates_adjust_p(df$p.left)
  df$p.right.adj <- .gates_adjust_p(df$p.right)
  df
}

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
#' Caller must set RNG seed before calling if tie-break jitter is needed.
#' @keywords internal
#' @noRd
.gates_quantile_group <- function(x, cutoffs) {
  num_groups <- length(cutoffs) + 1L
  n <- length(x)
  group_size_min <- 2L
  grouping_unfinished <- TRUE
  ct <- 0L

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
.gates_monotonize_table <- function(groups_df) {
  ord <- order(groups_df$estimate, decreasing = FALSE)
  out <- groups_df[ord, , drop = FALSE]
  out$group <- paste0("G", seq_len(nrow(out)))
  rownames(out) <- NULL
  out
}

#' @keywords internal
#' @noRd
.gates_diff_from_groups <- function(groups_df, subtract.from, subtracted,
                                    conf.level, vcov_gamma = NULL) {
  K <- nrow(groups_df)
  z <- stats::qnorm(1 - (1 - conf.level) / 2)

  diff_rows <- lapply(subtracted, function(j) {
    if (subtract.from == "least") {
      i_hi <- 1L
      i_lo <- j
      lab <- paste0("G1-G", j)
    } else {
      i_hi <- K
      i_lo <- j
      lab <- paste0("G", K, "-G", j)
    }
    diff_est <- groups_df$estimate[i_hi] - groups_df$estimate[i_lo]
    if (!is.null(vcov_gamma)) {
      v11 <- vcov_gamma[i_hi, i_hi]
      vjj <- vcov_gamma[i_lo, i_lo]
      v1j <- vcov_gamma[i_hi, i_lo]
      diff_se <- sqrt(v11 + vjj - 2 * v1j)
    } else {
      diff_se <- sqrt(groups_df$std.err[i_hi]^2 + groups_df$std.err[i_lo]^2)
    }
    zstat <- diff_est / diff_se
    data.frame(
      group = lab,
      estimate = diff_est,
      std.err = diff_se,
      lower = diff_est - z * diff_se,
      upper = diff_est + z * diff_se,
      z = zstat,
      p.left = stats::pnorm(zstat, lower.tail = TRUE),
      p.right = stats::pnorm(zstat, lower.tail = FALSE),
      stringsAsFactors = FALSE
    )
  })

  .gates_add_pcols(do.call(rbind, diff_rows))
}

#' @keywords internal
#' @noRd
.gates_fit_grf <- function(c.forest, membership, subtract.from, subtracted,
                           monotonize, conf.level) {
  K <- ncol(membership)
  z <- stats::qnorm(1 - (1 - conf.level) / 2)
  rows <- vector("list", K)

  for (k in seq_len(K)) {
    sub <- membership[, k]
    nk <- sum(sub)
    if (nk < 2L) {
      stop("gates(): quantile group G", k, " contains fewer than two ",
           "observations after grouping. Use coarser quantile.cutoffs ",
           "or increase n.", call. = FALSE)
    }
    res <- tryCatch(
      grf::average_treatment_effect(c.forest, subset = sub),
      error = function(e) {
        stop("gates(): grf could not estimate a subgroup ATE for G", k,
             " (", nk, " units): ", conditionMessage(e),
             ". Try cross.fit = TRUE, coarser cutoffs, or a larger sample.",
             call. = FALSE)
      }
    )
    est <- res[["estimate"]]
    se  <- res[["std.err"]]
    zval <- est / se
    rows[[k]] <- data.frame(
      group = paste0("G", k),
      estimate = est,
      std.err = se,
      lower = est - z * se,
      upper = est + z * se,
      z = zval,
      p.left = stats::pnorm(zval, lower.tail = TRUE),
      p.right = stats::pnorm(zval, lower.tail = FALSE),
      stringsAsFactors = FALSE
    )
  }

  groups_df <- .gates_add_pcols(do.call(rbind, rows))
  if (isTRUE(monotonize)) {
    groups_df <- .gates_monotonize_table(groups_df)
  }
  diff_df <- .gates_diff_from_groups(
    groups_df, subtract.from, subtracted, conf.level
  )

  list(groups = groups_df, diff = diff_df, membership = membership)
}

#' @keywords internal
#' @noRd
.gates_cf_args <- function(c.forest, idx = NULL) {
  tp <- c.forest$tunable.params
  cls <- if (length(c.forest$clusters) == 0L) NULL else c.forest$clusters
  sw <- c.forest$sample.weights
  if (!is.null(idx)) {
    if (!is.null(cls)) cls <- cls[idx]
    if (!is.null(sw)) sw <- sw[idx]
  }
  list(
    num.trees = c.forest[["_num_trees"]],
    sample.weights = sw,
    clusters = cls,
    equalize.cluster.weights = c.forest$equalize.cluster.weights,
    sample.fraction = tp$sample.fraction,
    mtry = tp$mtry,
    min.node.size = tp$min.node.size,
    honesty.fraction = tp$honesty.fraction,
    honesty.prune.leaves = tp$honesty.prune.leaves,
    alpha = tp$alpha,
    imbalance.penalty = tp$imbalance.penalty,
    ci.group.size = c.forest$ci.group.size
  )
}

#' @keywords internal
#' @noRd
.gates_crossfit <- function(c.forest, quantile.cutoffs, subtract.from,
                            subtracted, monotonize, conf.level, seed) {
  X <- c.forest$X.orig
  Y <- c.forest$Y.orig
  W <- c.forest$W.orig
  n <- nrow(X)

  idx_A <- sample(seq_len(n), floor(n / 2))
  idx_B <- setdiff(seq_len(n), idx_A)
  ca <- .gates_cf_args(c.forest, idx_A)
  cb <- .gates_cf_args(c.forest, idx_B)

  cf_A <- grf::causal_forest(
    X[idx_A, , drop = FALSE], Y[idx_A], W[idx_A],
    num.trees = ca$num.trees,
    sample.weights = ca$sample.weights,
    clusters = ca$clusters,
    equalize.cluster.weights = ca$equalize.cluster.weights,
    sample.fraction = ca$sample.fraction,
    mtry = ca$mtry,
    min.node.size = ca$min.node.size,
    honesty.fraction = ca$honesty.fraction,
    honesty.prune.leaves = ca$honesty.prune.leaves,
    alpha = ca$alpha,
    imbalance.penalty = ca$imbalance.penalty,
    ci.group.size = ca$ci.group.size,
    seed = seed
  )
  tau_B <- stats::predict(cf_A, X[idx_B, , drop = FALSE])$predictions
  membership_B <- .gates_quantile_group(tau_B, quantile.cutoffs)

  cf_B <- grf::causal_forest(
    X[idx_B, , drop = FALSE], Y[idx_B], W[idx_B],
    num.trees = cb$num.trees,
    sample.weights = cb$sample.weights,
    clusters = cb$clusters,
    equalize.cluster.weights = cb$equalize.cluster.weights,
    sample.fraction = cb$sample.fraction,
    mtry = cb$mtry,
    min.node.size = cb$min.node.size,
    honesty.fraction = cb$honesty.fraction,
    honesty.prune.leaves = cb$honesty.prune.leaves,
    alpha = cb$alpha,
    imbalance.penalty = cb$imbalance.penalty,
    ci.group.size = cb$ci.group.size,
    seed = seed + 1L
  )

  fit <- .gates_fit_grf(
    c.forest = cf_B,
    membership = membership_B,
    subtract.from = subtract.from,
    subtracted = subtracted,
    monotonize = monotonize,
    conf.level = conf.level
  )
  fit$eval.n <- length(idx_B)
  fit
}

#' @keywords internal
#' @noRd
.gates_fit_regression <- function(c.forest, quantile.cutoffs, proxy_CATE,
                                  subtract.from, subtracted, monotonize,
                                  conf.level, HT) {
  Y <- c.forest$Y.orig
  D <- c.forest$W.orig
  propensity_scores <- c.forest$W.hat
  proxy_BCA <- c.forest$Y.hat

  if (any(propensity_scores <= 0 | propensity_scores >= 1, na.rm = TRUE)) {
    stop("Propensity scores must lie strictly in (0, 1) for GATES regression. ",
         "Check overlap or refit with propensity clipping.", call. = FALSE)
  }

  membership <- .gates_quantile_group(proxy_CATE, quantile.cutoffs)
  groups <- 1 * membership
  K <- ncol(groups)
  gammanam <- paste0("gamma.", seq_len(K))

  ext_w <- c.forest$sample.weights
  if (!is.null(ext_w)) ext_w <- ext_w / mean(ext_w)

  if (!HT) {
    weights <- 1 / (propensity_scores * (1 - propensity_scores))
    if (!is.null(ext_w)) weights <- weights * ext_w
    X1 <- data.frame(B = proxy_BCA)
    X <- data.frame(X1, (D - propensity_scores) * groups)
    colnames(X) <- c("B", gammanam)
    gates.obj <- stats::lm(Y ~ ., data = data.frame(Y = Y, X), weights = weights)
  } else {
    H <- (D - propensity_scores) / (propensity_scores * (1 - propensity_scores))
    X1H <- data.frame(B = proxy_BCA * H)
    colnames(X1H) <- "B.H"
    X <- data.frame(X1H, groups)
    colnames(X) <- c("B.H", gammanam)
    form <- stats::as.formula(
      paste0("YH ~ ", paste(colnames(X), collapse = " + "), " + 0")
    )
    gates.obj <- stats::lm(
      form,
      data = data.frame(YH = Y * H, X),
      weights = ext_w
    )
  }

  coef_mat <- .gates_coef_table(gates.obj, gammanam)
  if (nrow(coef_mat) < K) {
    missing <- setdiff(gammanam, rownames(coef_mat))
    stop(
      "gates(): GATES regression dropped one or more group coefficients (",
      paste(missing, collapse = ", "), "). ",
      "This usually means collinear group indicators, no treatment variation ",
      "in a quantile bin, or a group that is too small for the weighted ",
      "regression. Use coarser quantile.cutoffs, increase n, or set ",
      "cross.fit = TRUE.",
      call. = FALSE
    )
  }

  vcov_mat <- stats::vcov(gates.obj)
  vcov_gamma <- vcov_mat[gammanam, gammanam, drop = FALSE]

  z <- stats::qnorm(1 - (1 - conf.level) / 2)
  estimates <- coef_mat[gammanam, "Estimate", drop = TRUE]
  ses <- coef_mat[gammanam, "Std. Error", drop = TRUE]
  zvals <- coef_mat[gammanam, "z value", drop = TRUE]

  groups_df <- data.frame(
    group = paste0("G", seq_len(K)),
    estimate = as.numeric(estimates),
    std.err = as.numeric(ses),
    lower = as.numeric(estimates - z * ses),
    upper = as.numeric(estimates + z * ses),
    z = as.numeric(zvals),
    p.left = as.numeric(stats::pnorm(zvals, lower.tail = TRUE)),
    p.right = as.numeric(stats::pnorm(zvals, lower.tail = FALSE)),
    stringsAsFactors = FALSE
  )
  groups_df <- .gates_add_pcols(groups_df)

  if (isTRUE(monotonize)) {
    ord <- order(groups_df$estimate)
    vcov_gamma <- vcov_gamma[ord, ord, drop = FALSE]
    groups_df <- .gates_monotonize_table(groups_df)
  }

  diff_df <- .gates_diff_from_groups(
    groups_df, subtract.from, subtracted, conf.level, vcov_gamma = vcov_gamma
  )

  list(groups = groups_df, diff = diff_df, membership = membership)
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
