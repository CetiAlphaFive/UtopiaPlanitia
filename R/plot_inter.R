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
#' @importFrom rlang .data
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
#' plot_inter(cf, x_var = "X1", y_var = "X2")
#' }
plot_inter <- function(c.forest, x_var, y_var, bin_count = 50,
                       limits = c(min(c.forest$predictions),
                                  max(c.forest$predictions))) {

  if (!inherits(c.forest, "causal_forest")) {
    stop("c.forest must be a causal_forest object from the grf package.")
  }
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required but not installed.")
  }
  if (!requireNamespace("ggExtra", quietly = TRUE)) {
    stop("Package 'ggExtra' is required but not installed.")
  }
  if (!requireNamespace("hexbin", quietly = TRUE)) {
    stop("Package 'hexbin' is required but not installed.")
  }
  if (!(x_var %in% colnames(c.forest$X.orig))) {
    stop(x_var, " is not in the covariate matrix.")
  }
  if (!(y_var %in% colnames(c.forest$X.orig))) {
    stop(y_var, " is not in the covariate matrix.")
  }

  plot.df <- data.frame(cates = c.forest$predictions, c.forest$X.orig)

  p <- plot.df |>
    ggplot2::ggplot(ggplot2::aes(x = .data[[x_var]], y = .data[[y_var]],
                                 z = .data[["cates"]])) +
    ggplot2::stat_summary_hex(bins = bin_count) +
    ggplot2::geom_point(alpha = 0) +
    ggplot2::scale_fill_gradient2(low = "#fd647c", mid = "#e6e6e6",
                                  high = "#3d900e", limits = limits) +
    ggplot2::labs(fill = "CATE") +
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
    )

  ggExtra::ggMarginal(p, type = "histogram", fill = "#e6e6e6", color = "white")
}
