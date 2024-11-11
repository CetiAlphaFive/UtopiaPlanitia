#' Permutation Variable Importance for Generalized Random Forests
#'
#' Computes the permutation variable importance for each feature in a generalized random forest model.
#'
#' @param forest A fitted random forest model object from the \code{grf} package.
#' @param n.perm An integer specifying the number of permutations to perform for each feature. Default is 50.
#' @return A data frame containing the RMSE differences for each feature across permutations.
#'
#' @details
#' This function computes the permutation variable importance by permuting each feature and measuring the increase in RMSE. The importance is calculated as the difference between the RMSE with the permuted feature and the original RMSE.
#'
#' For reproducible results, set a random seed before calling the function using \code{set.seed()}.
#'
#' @examples
#' \dontrun{
#' # Toy setup
#' n <- 500
#' p <- 5
#' X <- as.data.frame(matrix(rnorm(n * p, 0, 1), n, p))
#' Y <- X[, 1] + rnorm(n)
#' rf <- grf::regression_forest(Y = Y, X = X)
#'
#' # Set seed for reproducibility
#' set.seed(1995)
#'
#' # Run the permutation test
#' perm_results <- perm_test(forest = rf, n.perm = 50)
#'
#' # View the results
#' head(perm_results)
#' }
#'
#' @export
perm_test <- function(forest, n.perm = 50) {

  # Extract convenience objects
  X <- forest$X.orig
  Y <- forest$Y.orig
  n <- nrow(X)
  p <- ncol(X)

  # Calculate initial fit in RMSE
  Y_hat <- forest$predictions
  RMSE <- sqrt(mean((Y - Y_hat)^2))

  # Precompute permutation indices for each feature and permutation
  perm_indices <- lapply(1:p, function(j) {
    replicate(n.perm, sample.int(n), simplify = FALSE)
  })

  # Function to compute RMSE difference for a given permutation and feature
  compute_rmse_diff <- function(perm_idx, j) {
    # Permute feature j using precomputed indices
    idx <- perm_indices[[j]][[perm_idx]]
    X_perm <- X
    X_perm[, j] <- X[idx, j]

    # Predict using the permuted data
    Y_hat_perm <- predict(forest, newdata = X_perm, estimate.variance = FALSE)$predictions

    # Calculate RMSE difference
    RMSE_perm <- sqrt(mean((Y - Y_hat_perm)^2))
    diff <- RMSE_perm - RMSE

    return(diff)
  }

  # Create all combinations of permutations and features
  combos <- expand.grid(perm_idx = 1:n.perm, j = 1:p)

  # Compute RMSE differences using mapply
  RMSE_diffs_vector <- mapply(compute_rmse_diff, combos$perm_idx, combos$j)

  # Reshape the vector into a matrix
  RMSE_diffs <- matrix(RMSE_diffs_vector, nrow = n.perm, ncol = p, byrow = FALSE)

  # Clean up formatting for output
  RMSE_diffs <- as.data.frame(RMSE_diffs)
  colnames(RMSE_diffs) <- colnames(X)

  # Return the RMSE differences
  return(RMSE_diffs)
}
