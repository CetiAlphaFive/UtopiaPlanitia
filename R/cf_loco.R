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
#'     \item{`FALSE` (default)}{No screening. If estimated runtime is high and
#'       the session is interactive, the user is prompted to screen.}
#'     \item{`TRUE`}{Auto-screen. Keep variables with split-frequency importance
#'       above the mean.}
#'     \item{Integer `k`}{Keep the top-k variables by split-frequency
#'       importance.}
#'   }
#'   Screened-out variables receive importance = 0 in the output.
#' @param seed An integer seed for reproducibility. Default is `1995`.
#' @return An object of class `"cf_loco"` with components:
#'   \describe{
#'     \item{vimp}{Data frame with columns `Variable` and `Importance`.}
#'     \item{normalized}{Logical indicating whether scores are normalized.}
#'     \item{n}{Number of observations.}
#'     \item{p}{Number of covariates.}
#'   }
#'
#' @references
#' Benard, C. and Josse, J. (2023). Variable Importance for Causal Forests:
#' Breaking Down the Heterogeneity of Treatment Effects.
#' \doi{10.48550/arXiv.2308.03369}
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
#' }
cf_loco <- function(c.forest, variable.groups = NULL, group.by.corr = FALSE, corr.threshold = 0.5, normalize = FALSE, screen = FALSE, stabilize = 1e-6, seed = 1995) {

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

  if (!isFALSE(screen) || interactive()) {
    # compute split-frequency importance (essentially free)
    vi.split <- as.numeric(grf::variable_importance(c.forest))

    # aggregate to group level: max importance among member variables
    vi.group <- vapply(all.index.groups, function(idx) max(vi.split[idx]), numeric(1))
  }

  if (isTRUE(screen)) {
    # auto-screen: keep variables with importance > mean
    keep <- vi.group > mean(vi.group)
    if (sum(keep) == 0L) keep <- rep(TRUE, n.groups)  # safety: keep all if none above mean
    screened <- !keep
    k <- sum(keep)
    message("Screening: LOCO on ", k, " of ", n.groups, " variables (",
            paste(all.variable.names[keep], collapse = ", "), ")")
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

  } else if (isFALSE(screen) && interactive()) {
    # prompt if runtime looks extreme
    n.fits <- n.groups + 1L
    cost <- n.fits * n * c.forest$`_num_trees`
    if (cost > 5e6) {
      keep <- vi.group > mean(vi.group)
      if (sum(keep) == 0L) keep <- rep(TRUE, n.groups)
      k <- sum(keep)
      ans <- readline(paste0(
        "cf_loco will refit ", n.fits, " forests (n=", n,
        ", trees=", c.forest$`_num_trees`,
        "). Screen to top ", k, " variables? (y/n): "))
      if (tolower(trimws(ans)) == "y") {
        screened <- !keep
        message("Screening: LOCO on ", k, " of ", n.groups, " variables (",
                paste(all.variable.names[keep], collapse = ", "), ")")
        index.groups <- all.index.groups[keep]
        variable.names <- all.variable.names[keep]
      }
    }
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
    # normalize importance values to sum to 1, using absolute values
    In <- ifelse(In >= 0, abs(In), 0)
    In_out <- In / sum(In)
  } else {
    In_out <- In
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
