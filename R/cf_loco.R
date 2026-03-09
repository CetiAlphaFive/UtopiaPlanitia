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
cf_loco <- function(c.forest, variable.groups = NULL, group.by.corr = FALSE, corr.threshold = 0.5, normalize = FALSE, seed = 1995) {

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
    vimp.Xj <- compute_vimp(alpha, Y.centered, W.centered, tau.hat)
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
  In0 <- compute_vimp(alpha, Y.centered, W.centered, tau.hat)

  # compute debiased importance
  In <- In - In0

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
