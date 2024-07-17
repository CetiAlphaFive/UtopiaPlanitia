#' Left-hand permutation for feature importance
#'
#' Computes feature importance for a causal forest using left-hand permutation
#'
#' @param forest A fitted causal forest object from the grf package.
#' @param var_select A character vector of variable names to assess. Default is the column names of the original feature matrix.
#' @param n_perm An integer specifying the number of permutations. Default is 100.
#' @param n_cv An integer specifying the number of cross-validation runs. Default is 100.
#' @return A list containing a summary data frame of results and a data frame for plotting.
#' @export
left_perm <- function(forest, var_select = colnames(as.data.frame(forest$X.orig)), n_perm = 100, n_cv = 100) {

  # extract parameters from the original forest model
  params <- forest$tunable.params

  # initialize matrix to store real VI scores from cross-validation
  cv_vi_scores <- matrix(NA, n_cv, length(var_select))
  colnames(cv_vi_scores) <- var_select

  # initialize progress bar for cross-validation
  cat("Cross-validation runs progress:\n")
  pb_cv <- txtProgressBar(min = 0, max = n_cv, style = 3)

  # loop for cross-validation
  for (i in 1:n_cv) {
    # set a new seed for each run
    set.seed(i + 1995)

    # fit the causal forest
    forest_cv <- grf::causal_forest(X = forest$X.orig,
                                    Y = forest$Y.orig,
                                    W = forest$W.orig,
                                    num.trees = forest$`_num_trees`,
                                    sample.fraction = params$sample.fraction,
                                    mtry = params$mtry,
                                    min.node.size = params$min.node.size,
                                    honesty.fraction = params$honesty.fraction,
                                    honesty.prune.leaves = params$honesty.prune.leaves,
                                    alpha = params$alpha,
                                    imbalance.penalty = params$imbalance.penalty,
                                    seed = i + 1995)

    # extract VI scores
    cv_vi_scores[i, ] <- grf::variable_importance(forest_cv)

    # update progress bar
    setTxtProgressBar(pb_cv, i)
  }

  # close the progress bar for cross-validation
  close(pb_cv)

  # average the VI scores over cross-validation runs
  original_vi_scores <- colMeans(cv_vi_scores)

  # initialize matrix to store null VI scores
  null_vi_scores <- matrix(NA, n_perm, length(var_select))
  colnames(null_vi_scores) <- var_select

  # initialize progress bar for permutations
  cat("Permutation runs progress:\n")
  pb_perm <- txtProgressBar(min = 0, max = n_perm, style = 3)

  # loop for permutations
  for (i in 1:n_perm) {
    # set a new seed for each permutation
    set.seed(i + 1995 + n_cv)

    permuted_Y <- sample(forest$Y.orig)
    permuted_forest <- grf::causal_forest(X = forest$X.orig,
                                          Y = permuted_Y,
                                          W = forest$W.orig,
                                          num.trees = forest$`_num_trees`,
                                          sample.fraction = params$sample.fraction,
                                          mtry = params$mtry,
                                          min.node.size = params$min.node.size,
                                          honesty.fraction = params$honesty.fraction,
                                          honesty.prune.leaves = params$honesty.prune.leaves,
                                          alpha = params$alpha,
                                          imbalance.penalty = params$imbalance.penalty,
                                          seed = i + 1995 + n_cv)
    null_vi_scores[i, ] <- grf::variable_importance(permuted_forest)

    # update progress bar
    setTxtProgressBar(pb_perm, i)
  }

  # close the progress bar for permutations
  close(pb_perm)

  # compute means and standard deviations of null VI scores
  null_means <- colMeans(null_vi_scores)
  null_sd <- apply(null_vi_scores, 2, stats::sd)

  # calculate z-scores and p-values for one-sided test
  z_scores <- (original_vi_scores - null_means) / null_sd
  p_values <- stats::pnorm(z_scores, lower.tail = FALSE)  # one-sided test

  # create results data frame
  results <- data.frame(
    Feature = var_select,
    Original_VI_Score = original_vi_scores,
    Null_Mean = null_means,
    Null_SD = null_sd,
    Z_Score = z_scores,
    P_Value = p_values
  )

  # prepare data for plotting
  null_vi_scores_long <- data.frame(
    Feature = rep(var_select, each = n_perm),
    VI_Score = as.vector(null_vi_scores),
    Type = rep("Null", n_perm * length(var_select))
  )

  original_vi_scores_long <- data.frame(
    Feature = rep(var_select, each = n_cv),
    VI_Score = as.vector(cv_vi_scores),
    Type = rep("Original", n_cv * length(var_select))
  )

  # Combine both data frames
  plot_df <- rbind(original_vi_scores_long, null_vi_scores_long)

  return(list(summary = results, plot_df = plot_df))
}
