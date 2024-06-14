#' Model Diagnostics for Causal Forest
#'
#' Generates a multi-panel diagnostics figure for a causal forest model.
#'
#' @param c.forest A fitted causal forest model object from the \code{grf} package.
#' @return A list containing the following elements:
#' \item{out_check}{A ggplot2 object showing the density plot comparing real Y and predicted Y.}
#' \item{bal_perm_plot}{A ggplot2 object showing the treatment propensity plot.}
#' \item{cate_distro_plot}{A ggplot2 object showing the density plot of the distribution of CATEs.}
#' \item{rmse_out}{The Root Mean Squared Error (RMSE) for the random forest model predictions.}
#' @export
#' @examples
#' \dontrun{
#' cf_model <- causal_forest(X, Y, W)
#' diagnostics <- cf_model_diagnostics(cf_model)
#' }
plot_diag <- function(c.forest) {

  # Check if ggplot2 and ggExtra are installed
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required but not installed. Please install it to use this function.")
  }
  if (!requireNamespace("gridExtra", quietly = TRUE)) {
    stop("Package 'gridExtra' is required but not installed. Please install it to use this function.")
  }
  if (!requireNamespace("MLbalance", quietly = TRUE)) {
    stop("Package 'MLbalance' is required but not installed. Please install it to use this function.")
  }

  requireNamespace(gridExtra)

  # Outcome check: compare real Y and Y.hat
  plot.df1 <- data.frame(Y = c(c.forest$Y.orig, c.forest$Y.hat),
                         Type = c(rep("Y", length(c.forest$Y.orig)),
                                  rep("Y_hat(rf)", length(c.forest$Y.hat))))

  p1 <- plot.df1 |>
    ggplot2::ggplot(ggplot2::aes(x = Y, fill = Type)) +
    ggplot2::geom_density(alpha = 0.5) +
    ggplot2::labs(title = "Outcome Check: Real Y vs Predicted Y")

  # Calculate RMSE
  rmse <- function(y, y_hat) sqrt(mean((y - y_hat)^2))
  rmse_out <- rmse(c.forest$Y.orig, c.forest$Y.hat)

  cat("RMSE for outcome model:", rmse_out, "\n")

  # Distribution of CATEs
  plot.df2 <- data.frame(cates = c.forest$predictions, Type = "Regression Forest (Honest)")

  p2 <- plot.df2 |>
    ggplot2::ggplot(ggplot2::aes(x = cates, fill = Type)) +
    ggplot2::geom_density(alpha = 0.5) +
    ggplot2::labs(title = "Distribution of CATEs")

  # Treatment propensity check using MLbalance
  p3 <- MLbalance::random_check(W_real = c.forest$W.orig, X = c.forest$X.orig)$plot

  # Arrange plots in a multi-panel figure
  gridExtra::grid.arrange(p1, p3, p2, ncol = 1)

  list(
    outcome_check_plot = p1,
    treatment_propensity_plot = p3,
    cates_distribution_plot = p2,
    rmse_rf = rmse_rf
  )
}
