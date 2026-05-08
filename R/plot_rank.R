#' Rank Plot of Individual CATE Estimates
#'
#' Generates a rank plot of individual CATE (Conditional Average Treatment
#' Effect) estimates from a fitted causal forest, sorted by magnitude with
#' 95\% confidence intervals.
#'
#' @param c.forest A fitted causal forest object from the \code{grf} package.
#' @param show_ate_region Logical. If `TRUE` (default), draws a horizontal
#'   band showing the ATE +/- 1.96 standard errors. Units whose CIs fall
#'   entirely outside this band have strong evidence of individual-level
#'   heterogeneity.
#' @return A `ggplot2` object. Individual CATEs are plotted on the y-axis,
#'   ranked by magnitude on the x-axis, with color indicating effect
#'   direction and magnitude (red = negative, green = positive).
#'
#' @details
#' Each point represents one unit's out-of-bag CATE estimate, with a 95\%
#' confidence interval derived from the forest's variance estimates
#' (`estimate.variance = TRUE` in [grf::predict.causal_forest()]). Units
#' are sorted from most negative to most positive treatment effect.
#'
#' **Interpretation.** A flat rank plot (all CIs overlapping the ATE band)
#' suggests homogeneous effects. A steep S-curve with CIs separating from
#' the ATE band suggests meaningful heterogeneity — some units benefit much
#' more (or less) than average. The color gradient reinforces this: red
#' points are units harmed or unaffected by treatment, green points are
#' units that benefit most.
#'
#' @references
#' Athey, S. and Wager, S. (2019). Estimating Treatment Effects with Causal
#' Forests: An Application. *Observational Studies*, 5, 37--51.
#'
#' Wager, S. and Athey, S. (2018). Estimation and Inference of Heterogeneous
#' Treatment Effects Using Random Forests. *Journal of the American
#' Statistical Association*, 113(523), 1228--1242.
#' \doi{10.1080/01621459.2017.1319839}
#'
#' @seealso [plot_scatter()] for CATEs against a single covariate,
#'   [plot_pdp()] for partial dependence, [plot.causal_forest()] to call
#'   this via `plot(cf, type = "rank")`.
#'
#' @importFrom rlang .data
#' @importFrom stats predict
#' @export
#' @examplesIf rlang::is_installed(c("ggplot2", "ggdist"))
#' library(grf)
#' set.seed(1995)
#' n <- 200; p <- 5
#' X <- matrix(rnorm(n * p), n, p)
#' W <- rbinom(n, 1, 0.5)
#' Y <- X[, 1] * W + rnorm(n)
#' cf <- causal_forest(X, Y, W, num.trees = 100)
#' plot_rank(cf)
plot_rank <- function(c.forest, show_ate_region = TRUE) {

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
  plot.df <- data.frame(cates    = preds$predictions,
                        variance = preds$variance.estimates)
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

#' @rdname plot_rank
#' @export
rank_plot <- function(c.forest, show_ate_region = TRUE) {
  .Deprecated("plot_rank")
  plot_rank(c.forest, show_ate_region = show_ate_region)
}
