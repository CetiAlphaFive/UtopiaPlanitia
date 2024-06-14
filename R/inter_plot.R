#' Interaction Plot for Causal Forest
#'
#' Generates an interaction plot for a causal forest model.
#'
#' @param c.forest A fitted causal forest model object from the \code{grf} package.
#' @param x_var The name of the variable to be plotted on the x-axis.
#' @param y_var The name of the variable to be plotted on the y-axis.
#' @param bin_count Number of bins for the hexagonal binning. Default is 50.
#' @param limits The limits of the color scale, by default the minimum and maximum of the CATEs.
#' @return A ggplot2 object with the interaction plot.
#' @export
#' @examples
#' \dontrun{
#' cf_model <- causal_forest(X, Y, W)
#' inter_plot(cf_model, x_var = "X1", y_var = "X2")
#' }
inter_plot <- function(c.forest, x_var, y_var, bin_count = 50, limits = c(min(c.forest$predictions), max(c.forest$predictions))) {

  # Check if ggplot2 and ggridges are installed
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required but not installed. Please install it to use this function.")
  }
  if (!requireNamespace("ggExtra", quietly = TRUE)) {
    stop("Package 'ggExtra' is required but not installed. Please install it to use this function.")
  }

  requireNamespace("ggplot2")
  requireNamespace("ggExtra")

  plot.df <- data.frame(cates = c.forest$predictions, c.forest$X.orig)

  p <- plot.df |>
    ggplot2::ggplot(aes_string(x = x_var, y = y_var, z = "cates")) +
    stat_summary_hex(bins = bin_count) +
    geom_point(alpha = 0) +
    scale_fill_gradient2(low = "#fd647c", mid = "#e6e6e6", high = "#3d900e", limits = limits) +
    labs(fill = "CATE") +
    theme(
      text = element_text(size = 12, family = "serif"),
      panel.background = element_rect(fill = '#e6e6e6'),
      plot.title = element_text(hjust = 0.5),
      legend.position = "bottom",
      panel.border = element_blank(),
      axis.text = element_text(),
      axis.title = element_text(size = rel(1.2)),
      strip.text = element_text(hjust = 0),
      strip.background = element_rect(fill = NA, color = NA),
      legend.key = element_blank(),
      complete = TRUE
    )

  ggExtra::ggMarginal(p, type = "histogram", fill = "#e6e6e6", color = "white")
}

# Example usage
# Assume `cf_model` is a fitted causal forest model
# inter_plot(cf_model, x_var = "X1", y_var = "X2")
