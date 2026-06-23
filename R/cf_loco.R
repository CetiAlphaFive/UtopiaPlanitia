#' Modified LOCO Variable Importance for Causal Forests
#'
#' Computes leave-one-covariate-out (LOCO) variable importance for causal
#' forests using the debiased method of Benard and Josse (2023).
#'
#' @param c.forest A fitted causal forest object from the \code{grf} package.
#' @param variable.groups A list of variable groups. Each element of the list
#'   should contain the variable names of a group.
#' @param group.by.corr Logical indicating whether to group variables by
#'   correlation. Default is `FALSE`.
#' @param corr.threshold A numeric value between 0 and 1 indicating the
#'   correlation threshold for grouping variables. Default is `0.5`.
#' @param normalize Logical. If `TRUE`, return VI scores normalized to sum to 1.
#'   Default is `FALSE`.
#' @param stabilize Numeric. Floor for the per-observation denominator
#'   `Var_alpha(W.centered)` in the CATE re-estimation step. Prevents
#'   division-by-zero when forest weights concentrate on units with
#'   near-identical propensity scores. Default `1e-6`. Set to `0` to disable.
#' @param screen Controls optional pre-screening of variables via split-frequency
#'   importance (`grf::variable_importance()`), which is essentially free to
#'   compute. Screening reduces the number of expensive LOCO refits.
#'   \describe{
#'     \item{`FALSE` (default)}{No screening; LOCO is run on every variable.}
#'     \item{`TRUE`}{Auto-screen. Keep only variables with non-zero
#'       split-frequency importance (drop covariates the forest never split on).}
#'     \item{Integer `k`}{Keep the top-k variables by split-frequency
#'       importance.}
#'   }
#'   Screened-out variables receive importance = 0 in the output.
#' @param seed An integer seed for reproducibility. Default is `1995`.
#' @param verbose Logical. If `TRUE` (default), after the refits print the
#'   conditioning-variable correlation matrix and warn when any pair of
#'   covariates is correlated above `|r| = 0.5` (suppressed when variables are
#'   grouped via `group.by.corr` or `variable.groups`). Default `TRUE`.
#' @return An object of class `"cf_loco"` with components:
#'   \describe{
#'     \item{vimp}{Data frame with columns `Variable` and `Importance`.}
#'     \item{normalized}{Logical indicating whether scores are normalized.}
#'     \item{n}{Number of observations.}
#'     \item{p}{Number of covariates.}
#'   }
#'
#' @details
#' **What LOCO measures.** Leave-one-covariate-out (LOCO) importance drops
#' each covariate (or group of covariates) from the model, refits the forest
#' without it, and measures how much worse the individualized predictions
#' become. A large score means the variable is important for predicting
#' heterogeneous treatment effects; a score near zero means the forest can
#' recover the same CATEs without that variable.
#'
#' **Debiasing.** Naive LOCO is biased because refitting a forest introduces
#' retrain variance. This implementation uses the debiased estimator of
#' Benard and Josse (2023): a full-data refit is used as a bias correction
#' term, and per-observation CATE re-estimation is performed via forest
#' weights rather than raw OOB predictions.
#'
#' **Computational cost.** The function refits `p + 1` causal forests (one
#' per dropped variable plus one full-data refit), each with the same
#' hyperparameters as the original. For large datasets or many covariates,
#' consider using the `screen` argument to reduce the number of refits.
#'
#' **Normalization.** When `normalize = TRUE`, negative importance scores
#' (which can occur when dropping a variable improves predictions) are set
#' to zero, and the remaining scores are rescaled to sum to one. This gives
#' a proportional importance interpretation but discards directional
#' information.
#'
#' **Variable grouping.** Highly correlated covariates can be grouped so
#' they are dropped together. Use `group.by.corr = TRUE` for automatic
#' grouping via a correlation threshold, or supply a manual list via
#' `variable.groups`.
#'
#' @references
#' Benard, C. and Josse, J. (2023). Variable Importance for Causal Forests:
#' Breaking Down the Heterogeneity of Treatment Effects.
#' \doi{10.48550/arXiv.2308.03369}
#'
#' Athey, S., Tibshirani, J., and Wager, S. (2019). Generalized Random
#' Forests. *Annals of Statistics*, 47(2), 1148--1178.
#' \doi{10.1214/18-AOS1709}
#'
#' @seealso [loco()] for LOCO importance with ranger models,
#'   [plot.cf_loco()] and [summary.cf_loco()] for visualizing and
#'   summarizing results, [omni_hetero()] for heterogeneity testing.
#'
#' @export
#' @examples
#' \donttest{
#' library(grf)
#' set.seed(1995)
#' n <- 200; p <- 5
#' X <- matrix(rnorm(n * p), n, p)
#' colnames(X) <- paste0("X", seq_len(p))
#' W <- rbinom(n, 1, 0.5)
#' Y <- X[, 1] * W + rnorm(n)
#' cf <- causal_forest(X, Y, W, num.trees = 100)
#' vi <- cf_loco(cf)
#' summary(vi)
#' plot(vi)
#'
#' # Normalized importance (sums to 1)
#' vi_norm <- cf_loco(cf, normalize = TRUE)
#' summary(vi_norm)
#' }
cf_loco <- function(c.forest, variable.groups = NULL, group.by.corr = FALSE, corr.threshold = 0.5, normalize = FALSE, screen = FALSE, stabilize = 1e-6, seed = 1995, verbose = TRUE) {

  set.seed(seed) # Set seed for reproducibility

  # check input c.forest is a grf causal forest
  if (!inherits(c.forest, "causal_forest")) {
    stop('c.forest must be a grf causal forest.')
  }

  # get initial data
  X <- c.forest$X.orig
  Y <- c.forest$Y.orig
  W <- c.forest$W.orig
  p <- ncol(X)
  n <- nrow(X)

  # get centered outcome and treatment assignment
  Y.centered <- Y - c.forest$Y.hat
  W.centered <- W - c.forest$W.hat

  # get oob predictions
  tau.hat <- c.forest$predictions

  # group variables by correlation if requested
  if (group.by.corr) {
    corr_matrix <- stats::cor(X)
    variable.groups <- list()
    grouped <- rep(FALSE, p)
    for (i in 1:p) {
      if (!grouped[i]) {
        group <- which(abs(corr_matrix[i, ]) > corr.threshold)
        variable.groups[[length(variable.groups) + 1]] <- colnames(X)[group]
        grouped[group] <- TRUE
      }
    }
  }

  # set variable groups
  if (is.null(variable.groups)) {
    # when variable.groups is NULL, each input variable defines a group
    index.groups <- as.list(1:p)
    variable.names <- colnames(as.data.frame(X))
  } else {
    # check provided variable groups are valid (non empty list, all variable names are contained in the data, and not all variables in one group)
    non.empty.list <- is.list(variable.groups) & length(variable.groups) > 0
    if (!non.empty.list) {
      stop('variable.groups must be a non-empty list.')
    }
    names.valid <- all(unlist(variable.groups) %in% colnames(X))
    if (!names.valid) {
      stop('Variable names provided in variable.groups not all found in input data.')
    }
    groups.valid <- max(sapply(variable.groups, function(group) { length(unique(group)) })) < p
    if (!groups.valid) {
      stop('A group cannot contain all input variables.')
    }
    # get group of variable indices
    index.groups <- lapply(variable.groups, function(group) {
      unique(sapply(group, function(j) { which(colnames(X) == j) }))
    })
    variable.names <- sapply(variable.groups, paste, collapse = ", ")
  }

  # --- variable screening via split-frequency importance ---
  n.groups <- length(index.groups)
  all.variable.names <- variable.names
  all.index.groups <- index.groups
  screened <- rep(FALSE, n.groups)  # TRUE = screened out (gets importance 0)

  # validate screen argument
  if (!isFALSE(screen)) {
    if (!isTRUE(screen) && !(is.numeric(screen) && length(screen) == 1L &&
                              screen == as.integer(screen) && screen >= 1L)) {
      stop("screen must be FALSE, TRUE, or a positive integer.")
    }
    if (is.numeric(screen) && screen >= n.groups) {
      stop("screen must be less than the number of variable groups (", n.groups, ").")
    }
  }

  if (!isFALSE(screen)) {
    # compute split-frequency importance (essentially free)
    vi.split <- as.numeric(grf::variable_importance(c.forest))
    # aggregate to group level: max importance among member variables
    vi.group <- vapply(all.index.groups, function(idx) max(vi.split[idx]), numeric(1))
  }

  if (isTRUE(screen)) {
    # auto-screen: drop covariates the forest never split on (importance exactly 0)
    keep <- vi.group > 0
    if (sum(keep) == 0L) {
      keep <- rep(TRUE, n.groups)  # safety: all zero -> keep all
      message("Screening: all variables have zero split-frequency importance; ",
              "keeping all ", n.groups, ".")
    } else {
      message("Screening: LOCO on ", sum(keep), " of ", n.groups, " variables (",
              paste(all.variable.names[keep], collapse = ", "), ")")
    }
    screened <- !keep
    index.groups <- all.index.groups[keep]
    variable.names <- all.variable.names[keep]

  } else if (is.numeric(screen)) {
    # explicit top-k
    k <- as.integer(screen)
    ord <- order(vi.group, decreasing = TRUE)
    keep.idx <- ord[seq_len(k)]
    keep <- seq_len(n.groups) %in% keep.idx
    screened <- !keep
    message("Screening: LOCO on top ", k, " of ", n.groups, " variables (",
            paste(all.variable.names[keep], collapse = ", "), ")")
    index.groups <- all.index.groups[keep]
    variable.names <- all.variable.names[keep]
  }

  # compute vimp for all input variables using the settings of the initial causal forest
  In <- sapply(index.groups, function(index) {
    set.seed(seed) # Set seed for reproducibility within the loop
    c.forest.drop.Xj <- grf::causal_forest(X[, -index, drop = FALSE], Y, W, Y.hat = c.forest$Y.hat, W.hat = c.forest$W.hat,
                                           num.trees = c.forest$`_num_trees`,
                                           sample.weights = c.forest$sample.weights,
                                           clusters = c.forest$clusters,
                                           equalize.cluster.weights = c.forest$equalize.cluster.weights,
                                           sample.fraction = c.forest$tunable.params$sample.fraction,
                                           mtry = min(c.forest$tunable.params$mtry, ncol(X) - length(index)),
                                           min.node.size = c.forest$tunable.params$min.node.size,
                                           honesty.fraction = c.forest$tunable.params$honesty.fraction,
                                           honesty.prune.leaves = c.forest$tunable.params$honesty.prune.leaves,
                                           alpha = c.forest$tunable.params$alpha,
                                           imbalance.penalty = c.forest$tunable.params$imbalance.penalty,
                                           ci.group.size = c.forest$ci.group.size)
    alpha <- grf::get_forest_weights(c.forest.drop.Xj)
    vimp.Xj <- compute_vimp(alpha, Y.centered, W.centered, tau.hat, stabilize)
    return(vimp.Xj)
  })

  # compute retrain bias
  set.seed(seed) # Set seed for reproducibility
  c.forest0 <- grf::causal_forest(X, Y, W, Y.hat = c.forest$Y.hat, W.hat = c.forest$W.hat,
                                  num.trees = c.forest$`_num_trees`,
                                  sample.weights = c.forest$sample.weights,
                                  clusters = c.forest$clusters,
                                  equalize.cluster.weights = c.forest$equalize.cluster.weights,
                                  sample.fraction = c.forest$tunable.params$sample.fraction,
                                  mtry = c.forest$tunable.params$mtry,
                                  min.node.size = c.forest$tunable.params$min.node.size,
                                  honesty.fraction = c.forest$tunable.params$honesty.fraction,
                                  honesty.prune.leaves = c.forest$tunable.params$honesty.prune.leaves,
                                  alpha = c.forest$tunable.params$alpha,
                                  imbalance.penalty = c.forest$tunable.params$imbalance.penalty,
                                  ci.group.size = c.forest$ci.group.size)
  alpha <- grf::get_forest_weights(c.forest0)
  In0 <- compute_vimp(alpha, Y.centered, W.centered, tau.hat, stabilize)

  # compute debiased importance
  In <- In - In0

  # reassemble full vector with zeros for screened-out variables
  if (any(screened)) {
    In.full <- numeric(n.groups)
    In.full[!screened] <- In
    In <- In.full
    variable.names <- all.variable.names
  }

  if(normalize){
    # H5 (audit-20260511): negative values clipped to zero, remaining
    # values rescaled to sum to 1. When every score is <= 0 (raw LOCO
    # detected no useful variation -- e.g. pure-noise DGPs), the sum is
    # zero and a naive divide yields NaN; we instead return uniform 1/p
    # with a warning so downstream code never sees NaN.
    In <- ifelse(In >= 0, abs(In), 0)
    if (sum(In) == 0) {
      warning("cf_loco: all non-negative LOCO scores are zero; returning ",
              "uniform 1/p importance.", call. = FALSE)
      In_out <- rep(1 / length(In), length(In))
    } else {
      In_out <- In / sum(In)
    }
  } else {
    In_out <- In
  }

  # --- correlation diagnostic (conditioning variables) ---
  if (verbose && p > 1L) {
    cov.names <- colnames(X)
    if (is.null(cov.names)) cov.names <- paste0("V", seq_len(p))
    cm <- suppressWarnings(stats::cor(X))
    dimnames(cm) <- list(cov.names, cov.names)
    message("Conditioning-variable correlation matrix:")
    print(round(cm, 2))

    grouping.active <- isTRUE(group.by.corr) || !is.null(variable.groups)
    if (!grouping.active) {
      cm.off <- cm
      diag(cm.off) <- NA_real_
      hit <- upper.tri(cm.off) & !is.na(cm.off) & abs(cm.off) > 0.5
      if (any(hit)) {
        idx <- which(hit, arr.ind = TRUE)
        pair.str <- apply(idx, 1L, function(rc) {
          sprintf("%s & %s (r = %.2f)",
                  cov.names[rc[1L]], cov.names[rc[2L]], cm[rc[1L], rc[2L]])
        })
        warning("cf_loco: the following conditioning variables are correlated ",
                "above |r| = 0.5: ", paste(pair.str, collapse = "; "),
                ". LOCO importance is divided among correlated covariates and ",
                "may understate their individual relevance. Consider grouping ",
                "them via `group.by.corr = TRUE` or interpreting their ",
                "importances jointly.", call. = FALSE)
      }
    }
  }

  result <- data.frame(Variable = variable.names, Importance = In_out)

  out <- structure(
    list(
      vimp = result,
      normalized = normalize,
      n = n,
      p = p
    ),
    class = "cf_loco"
  )
  out
}
