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
#' @importFrom rlang .data
#' @export
#' @examples
#' \donttest{
#' library(grf)
#' set.seed(1995)
#' n <- 200; p <- 5
#' X <- matrix(rnorm(n * p), n, p)
#' W <- rbinom(n, 1, 0.5)
#' Y <- X[, 1] * W + rnorm(n)
#' cf <- causal_forest(X, Y, W, num.trees = 100)
#' plot_diag(cf)
#' }
plot_diag <- function(c.forest) {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required but not installed.")
  }
  if (!requireNamespace("gridExtra", quietly = TRUE)) {
    stop("Package 'gridExtra' is required but not installed.")
  }
  if (!requireNamespace("MLbalance", quietly = TRUE)) {
    stop("Package 'MLbalance' is required but not installed.")
  }
  if (!requireNamespace("ggdist", quietly = TRUE)) {
    stop("Package 'ggdist' is required but not installed.")
  }

  # mlbalance theme to match
  g_theme <- function() {
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 14, face = "bold", hjust = 0.5),
      panel.background = ggplot2::element_rect(fill = "white", colour = "white",
                                                linewidth = 0.5, linetype = "solid"),
      axis.line = ggplot2::element_line(linewidth = .5, color = "black"),
      axis.title.x = ggplot2::element_text(size = 12, face = "bold"),
      axis.title.y = ggplot2::element_text(size = 12, face = "bold"),
      axis.text.x = ggplot2::element_text(color = "grey30", size = 10),
      axis.text.y = ggplot2::element_text(color = "grey30", size = 10),
      panel.grid.major.x = ggplot2::element_line(colour = "grey80"),
      plot.caption = ggplot2::element_text(hjust = 0),
      text = ggplot2::element_text(size = 12, family = "serif"),
      legend.position.inside = c(.1, .75),
      axis.ticks = ggplot2::element_blank(),
      legend.background = ggplot2::element_blank(),
      panel.spacing = grid::unit(2, "lines")
    )
  }

  # Outcome check: compare real Y and Y.hat
  plot.df1 <- data.frame(Y = c(c.forest$Y.orig, c.forest$Y.hat),
                         Type = c(rep("Y", length(c.forest$Y.orig)),
                                  rep("Y_hat", length(c.forest$Y.hat))))

  p1 <- plot.df1 |>
    ggplot2::ggplot(ggplot2::aes(x = .data[["Y"]], fill = .data[["Type"]])) +
    ggplot2::geom_density(alpha = 0.5) +
    ggplot2::labs(title = expression("Outcome Check: Y vs." ~ hat(Y)),
                  y = "Density", x = "Y") +
    g_theme() +
    ggplot2::scale_fill_manual(values = c("dodgerblue1", "darkorange1"))

  # Calculate RMSE
  rmse <- function(y, y_hat) sqrt(mean((y - y_hat)^2))
  rmse_out <- rmse(c.forest$Y.orig, c.forest$Y.hat)

  cat("RMSE for outcome model:", rmse_out, "\n")

  # Distribution of CATEs
  plot.df2 <- data.frame(cates = c.forest$predictions,
                         Type = "Regression Forest (Honest)")

  p2 <- plot.df2 |>
    ggplot2::ggplot(ggplot2::aes(x = .data[["cates"]], fill = .data[["Type"]])) +
    ggdist::stat_histinterval(alpha = 0.9, fill = "darkorange1",
                              outline_bars = TRUE, slab_color = "black") +
    ggplot2::labs(title = expression(hat(CATE) ~ "Distribution"),
                  y = "Density",
                  x = expression(hat(CATE) ~ "Estimates")) +
    g_theme()

  # Treatment propensity check using MLbalance
  p3 <- MLbalance::random_check(W_real = c.forest$W.orig,
                                X = c.forest$X.orig)$plot +
    ggplot2::ggtitle("Balance Permutation Test")

  # Arrange plots in a multi-panel figure
  gridExtra::grid.arrange(p1, p3, p2, ncol = 2)

  list(
    outcome_check_plot = p1,
    treatment_propensity_plot = p3,
    cates_distribution_plot = p2,
    rmse_out = rmse_out
  )
}
