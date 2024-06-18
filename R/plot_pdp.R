#' Partial Dependence Plot for Causal Forest
#'
#' Generates a partial dependence plot for a causal forest model.
#'
#' @param c.forest A fitted causal forest model object from the \code{grf} package.
#' @param x_var The name of the variable to be plotted on the x-axis.
#' @param curve_fitter Logical indicating whether to include a curve fitter. Default is TRUE.
#' @param method The smoothing method to be used by \code{geom_smooth} if \code{curve_fitter} is TRUE. Default is "loess".
#' @param show_ate_region Logical indicating whether to show the ATE region (confidence interval). Default is TRUE.
#' @param x.limits x axis limits specified as c() vector. Defaults to range of X.
#' @param y.limits y axis limits specified as c() vector. Defaults to range of Y.
#' @return A ggplot2 object with the partial dependence plot.
#' @export
#' @examples
#' \dontrun{
#' cf_model <- causal_forest(X, Y, W)
#' pdp_plot(cf_model, x_var = "X1")
#' }
plot_pdp <- function(c.forest, x_var, curve_fitter = TRUE, method = "loess", show_ate_region = TRUE,x.limits = NULL,y.limits = NULL) {
  # Check if ggplot2 and ggExtra are installed
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required but not installed. Please install it to use this function.")
  }
  if (!requireNamespace("ggExtra", quietly = TRUE)) {
    stop("Package 'ggExtra' is required but not installed. Please install it to use this function.")
  }

  requireNamespace(ggplot2)
  requireNamespace(ggExtra)

  plot.df <- data.frame(cates = c.forest$predictions, c.forest$X.orig)

  ate <- grf::average_treatment_effect(c.forest)[["estimate"]]
  upper <- grf::average_treatment_effect(c.forest)[["estimate"]] + 1.96 * grf::average_treatment_effect(c.forest)[["std.err"]]
  lower <- grf::average_treatment_effect(c.forest)[["estimate"]] - 1.96 * grf::average_treatment_effect(c.forest)[["std.err"]]

  p <- plot.df |>
    ggplot2::ggplot(ggplot2::aes_string(x = x_var, y = "cates")) +
    ggplot2::geom_point(shape = 1, color = "#ea794e") +
    ggplot2::labs(y = "CATE") +
    ggplot2::theme(
      text = ggplot2::element_text(size = 12, family = "serif"),
      panel.background = ggplot2::element_rect(fill = '#e6e6e6'),
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
    ggplot2::scale_x_continuous(expand = c(0, 0),limits = x.limits) +
    ggplot2::scale_y_continuous(limits = y.limits)


  if (show_ate_region) {
    p <- p +
      ggplot2::geom_hline(ggplot2::aes(yintercept = ate), color = "black", linewidth = .5) +
      ggplot2::geom_hline(ggplot2::aes(yintercept = upper), linetype = "dotted") +
      ggplot2::geom_hline(ggplot2::aes(yintercept = lower), linetype = "dotted") +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper), fill = "#5ab0c0", alpha = 0.2)
  }

  if (curve_fitter) {
    p <- p + ggplot2::geom_smooth(method = method, color = "#5ab0c0", span = .75, size = 1.5)
  }

  ggExtra::ggMarginal(p, type = "histogram", fill = "#e6e6e6", color = "white")
}

# Example usage
# Assume `cf_model` is a fitted causal forest model
# pdp_plot(cf_model, x_var = "X1")
