#' Modified LOCO Variable Importance for Causal Forests
#'
#' Computes variable importance for causal forests using the method here: https://arxiv.org/abs/2308.03369
#'
#' @param c.forest A fitted causal forest object from the grf package.
#' @param variable.groups A list of variable groups. Each element of the list should contain the variable names of a group.
#' @param group.by.corr A logical indicating whether to group variables by correlation. Default is FALSE.
#' @param corr.threshold A numeric value between 0 and 1 indicating the correlation threshold for grouping variables. Default is 0.5.
#' @param noramlize Return the VI scores on the same scale as grf's native scores, on the range from 0 to 1 and summing to 1. Default is TRUE.
#' @param seed An integer seed for reproducibility.
#' @return A data frame with the original variable names and their variable importance scores.
#' @export
cf_loco <- function(c.forest, variable.groups = NULL, group.by.corr = FALSE, corr.threshold = 0.5, normalize = T,seed = 1234) {

  set.seed(seed) # Set seed for reproducibility

  # check input c.forest is a grf causal forest
  is.causal.forest <- all(class(c.forest) == c('causal_forest', 'grf'))
  if (!is.causal.forest) {
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
    c.forest.drop.Xj <- grf::causal_forest(X[, -index, drop = F], Y, W, Y.hat = c.forest$Y.hat, W.hat = c.forest$W.hat,
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
  return(result)
}
