#' Rank Plot of Individual CATE Estimates
#'
#' Generates a rank plot of individual CATE (Conditional Average Treatment Effect)
#' estimates from a fitted causal forest model.
#'
#' @param c.forest A fitted causal forest model object from the \code{grf} package.
#' @param show_ate_region Logical indicating whether to show the ATE region (confidence interval). Default is TRUE.
#' @return A ggplot2 object with the rank plot of individual CATE estimates.
#' @importFrom rlang .data
#' @importFrom stats predict
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
#' rank_plot(cf)
#' }
rank_plot <- function(c.forest, show_ate_region = TRUE) {

  if (!inherits(c.forest, "causal_forest")) {
    stop("c.forest must be a causal_forest object from the grf package.")
  }
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required but not installed.")
  }
  if (!requireNamespace("ggdist", quietly = TRUE)) {
    stop("Package 'ggdist' is required but not installed.")
  }

  # Calculate ATE and intervals once
  ate.result <- grf::average_treatment_effect(c.forest)
  ate     <- ate.result[["estimate"]]
  a.upper <- ate + 1.96 * ate.result[["std.err"]]
  a.lower <- ate - 1.96 * ate.result[["std.err"]]

  # Get CATE predictions and variance estimates
  preds <- predict(c.forest, estimate.variance = TRUE)

  # Create data frame for plotting
  plot.df <- data.frame(cates = preds$predictions, variance = preds$variance)
  plot.df$upper <- plot.df$cates + 1.96 * sqrt(plot.df$variance)
  plot.df$lower <- plot.df$cates - 1.96 * sqrt(plot.df$variance)

  # Rank by magnitude of CATEs
  plot.df <- plot.df[order(plot.df$cates), ]
  plot.df$rank <- seq_len(nrow(plot.df))

  # Set limits for colors
  limits <- c(min(plot.df$cates), max(plot.df$cates))

  p <- ggplot2::ggplot(data = plot.df,
                       ggplot2::aes(x = .data[["rank"]], y = .data[["cates"]],
                                    ymin = .data[["lower"]], ymax = .data[["upper"]])) +
    ggdist::geom_pointinterval(shape = 1,
                               ggplot2::aes(color = .data[["cates"]])) +
    ggplot2::labs(color = "CATE") +
    ggplot2::xlab("") +
    ggplot2::ylab("Individual CATE Estimate (95% Confidence Intervals)") +
    ggplot2::scale_color_gradient2(low = "#fd647c", mid = "#e6e6e6",
                                   high = "#3d900e", midpoint = 0,
                                   limits = limits) +
    ggplot2::theme(
      text = ggplot2::element_text(size = 12, family = "serif"),
      panel.background = ggplot2::element_rect(fill = "#e6e6e6"),
      plot.title = ggplot2::element_text(hjust = 0.5),
      legend.position = "bottom",
      panel.border = ggplot2::element_blank(),
      axis.text = ggplot2::element_text(),
      axis.title = ggplot2::element_text(size = ggplot2::rel(1.2)),
      strip.text = ggplot2::element_text(hjust = 0),
      strip.background = ggplot2::element_rect(fill = NA, color = NA),
      legend.key = ggplot2::element_blank(),
      complete = TRUE
    ) +
    ggplot2::scale_x_continuous(expand = c(0, 0))

  if (show_ate_region) {
    p <- p +
      ggplot2::geom_hline(ggplot2::aes(yintercept = ate), color = "black", linewidth = .5) +
      ggplot2::geom_hline(ggplot2::aes(yintercept = a.upper), linetype = "dotted", color = "black") +
      ggplot2::geom_hline(ggplot2::aes(yintercept = a.lower), linetype = "dotted", color = "black") +
      ggplot2::geom_ribbon(ggplot2::aes(xmin = -Inf, xmax = Inf, ymin = a.lower, ymax = a.upper),
                           fill = "#5ab0c0", alpha = 0.2)
    }

  p
}
