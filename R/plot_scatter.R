#' CATE Scatter Plot for Causal Forest
#'
#' Plots individual out-of-bag CATE estimates against a covariate with an
#' optional loess smoother and ATE confidence region. This is **not** a
#' Friedman (2001) partial dependence plot — see [plot_pdp()] for that.
#'
#' @param c.forest A fitted causal forest model object from the \code{grf} package.
#' @param x_var The name of the variable to be plotted on the x-axis.
#' @param curve_fitter Logical indicating whether to include a curve fitter. Default is TRUE.
#' @param method The smoothing method to be used by \code{geom_smooth} if \code{curve_fitter} is TRUE. Default is "loess".
#' @param show_ate_region Logical indicating whether to show the ATE region (confidence interval). Default is TRUE.
#' @param x.limits x axis limits specified as c() vector. Defaults to range of X.
#' @param y.limits y axis limits specified as c() vector. Defaults to range of Y.
#' @return A ggplot2 object with the scatter plot.
#' @importFrom rlang .data
#' @export
#' @examplesIf rlang::is_installed(c("ggplot2", "ggExtra"))
#' library(grf)
#' set.seed(1995)
#' n <- 200; p <- 5
#' X <- matrix(rnorm(n * p), n, p)
#' colnames(X) <- paste0("X", seq_len(p))
#' W <- rbinom(n, 1, 0.5)
#' Y <- X[, 1] * W + rnorm(n)
#' cf <- causal_forest(X, Y, W, num.trees = 100)
#' plot_scatter(cf, x_var = "X1")
plot_scatter <- function(c.forest, x_var, curve_fitter = TRUE, method = "loess",
                         show_ate_region = TRUE, x.limits = NULL, y.limits = NULL) {

  if (!inherits(c.forest, "causal_forest")) {
    stop("c.forest must be a causal_forest object from the grf package.")
  }
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required but not installed.")
  }
  if (!requireNamespace("ggExtra", quietly = TRUE)) {
    stop("Package 'ggExtra' is required but not installed.")
  }
  if (!(x_var %in% colnames(c.forest$X.orig))) {
    stop(x_var, " is not in the covariate matrix. ",
         "See colnames(c.forest$X.orig) for available variables.")
  }

  plot.df <- data.frame(cates = c.forest$predictions, c.forest$X.orig)

  ate.result <- grf::average_treatment_effect(c.forest)
  ate   <- ate.result[["estimate"]]
  upper <- ate + 1.96 * ate.result[["std.err"]]
  lower <- ate - 1.96 * ate.result[["std.err"]]

  p <- plot.df |>
    ggplot2::ggplot(ggplot2::aes(x = .data[[x_var]], y = .data[["cates"]])) +
    ggplot2::geom_point(shape = 1, color = "#ea794e") +
    ggplot2::labs(y = "CATE") +
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
    ggplot2::scale_x_continuous(expand = c(0, 0), limits = x.limits) +
    ggplot2::scale_y_continuous(limits = y.limits)

  if (show_ate_region) {
    p <- p +
      ggplot2::geom_hline(ggplot2::aes(yintercept = ate), color = "black", linewidth = .5) +
      ggplot2::geom_hline(ggplot2::aes(yintercept = upper), linetype = "dotted") +
      ggplot2::geom_hline(ggplot2::aes(yintercept = lower), linetype = "dotted") +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper), fill = "#5ab0c0", alpha = 0.2)
  }

  if (curve_fitter) {
    p <- p + ggplot2::geom_smooth(method = method, color = "#5ab0c0", span = .75, linewidth = 1.5)
  }

  .wrap_marginal(p, type = "histogram", fill = "#e6e6e6", color = "white")
}
