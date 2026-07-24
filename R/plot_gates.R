#' Plot GATES Estimates
#'
#' Visualizes sorted group average treatment effect estimates from [gates()]
#' as a point-and-interval plot by heterogeneity group, with optional reference
#' lines for the overall average treatment effect and zero.
#'
#' @param x An object of class `"cf_gates"` returned by [gates()].
#' @param groups Character vector of group labels to plot. Default `"all"` plots
#'   every row in `x$groups` and `x$diff`. Otherwise a subset of names like
#'   `c("G1", "G4", "G4-G1")`.
#' @param show.ate Logical. If `TRUE` (default), draw horizontal reference lines
#'   for the overall ATE and its confidence interval from `x$ate`.
#' @param y.limits Optional numeric vector of length two for the y-axis limits.
#'   Default spans the plotted intervals (and ATE band when `show.ate = TRUE`),
#'   always including zero.
#' @param title Optional plot title. Default `"Sorted Group Average Treatment Effects"`.
#' @param ... Unused (for consistency with other plotters).
#'
#' @return A `utopia_plot` object (ggplot grob tagged for [print.utopia_plot()]).
#'
#' @details
#' Shares the package house styling with [plot_pdp()] (serif type, gray panel,
#' centered title). Groups are ordered on the x-axis from lowest to highest
#' predicted CATE (`G1` .. `GK`).
#'
#' @references
#' Chernozhukov, V., Demirer, M., Duflo, E., and Fernandez-Val, I. (2020).
#' Generic Machine Learning Inference on Heterogeneous Treatment Effects in
#' Randomized Experiments. \emph{Econometrica}, forthcoming.
#'
#' @seealso [gates()], [print.cf_gates()]
#'
#' @export
#' @examplesIf rlang::is_installed("ggplot2")
#' library(grf)
#' set.seed(1995)
#' n <- 400; p <- 5
#' X <- matrix(rnorm(n * p), n, p)
#' colnames(X) <- paste0("X", seq_len(p))
#' W <- rbinom(n, 1, 0.5)
#' Y <- X[, 1] * W + rnorm(n)
#' cf <- causal_forest(X, Y, W, num.trees = 200)
#' g <- gates(cf)
#' plot_gates(g)
plot_gates <- function(x,
                       groups = "all",
                       show.ate = TRUE,
                       y.limits = NULL,
                       title = "Sorted Group Average Treatment Effects",
                       ...) {

  if (!inherits(x, "cf_gates")) {
    stop("x must be an object of class 'cf_gates' (from gates()).",
         call. = FALSE)
  }
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required but not installed.", call. = FALSE)
  }
  if (!is.logical(show.ate) || length(show.ate) != 1L) {
    stop("show.ate must be a single logical value.", call. = FALSE)
  }

  df <- rbind(
    x$groups[, c("group", "estimate", "lower", "upper")],
    x$diff[, c("group", "estimate", "lower", "upper")]
  )
  avail <- df$group

  if (length(groups) == 1L && identical(groups, "all")) {
    groups_plot <- avail
  } else {
    bad <- setdiff(groups, avail)
    if (length(bad) > 0L) {
      stop("Unknown group label(s): ", paste(bad, collapse = ", "),
           ". Available: ", paste(avail, collapse = ", "), call. = FALSE)
    }
    groups_plot <- groups
  }

  df <- df[df$group %in% groups_plot, , drop = FALSE]
  if (nrow(df) == 0L) {
    stop("No groups left to plot after subsetting.", call. = FALSE)
  }

  base_order <- c(x$groups$group, x$diff$group)
  df$group <- factor(df$group, levels = intersect(base_order, df$group))

  if (is.null(y.limits)) {
    y.limits <- range(c(0, df$lower, df$upper), na.rm = TRUE)
    if (isTRUE(show.ate)) {
      y.limits <- range(c(y.limits, x$ate$lower, x$ate$upper), na.rm = TRUE)
    }
    pad <- diff(y.limits) * 0.08
    if (pad == 0) pad <- 0.1
    y.limits <- c(y.limits[1] - pad, y.limits[2] + pad)
  }

  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data[["group"]],
                                        y = .data[["estimate"]])) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dotted", color = "black") +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = .data[["lower"]], ymax = .data[["upper"]]),
      width = 0.15, color = "#5ab0c0", linewidth = 0.8) +
    ggplot2::geom_point(color = "#ea794e", size = 2.5) +
    ggplot2::labs(x = "Group by predicted CATE", y = "Treatment effect",
                  title = title) +
    ggplot2::scale_y_continuous(limits = y.limits) +
    .utopia_pdp_theme()

  if (isTRUE(show.ate)) {
    p <- p +
      ggplot2::geom_hline(yintercept = x$ate$estimate,
                          color = "black", linewidth = 0.5) +
      ggplot2::geom_hline(yintercept = x$ate$lower, linetype = "dashed") +
      ggplot2::geom_hline(yintercept = x$ate$upper, linetype = "dashed")
  }

  .as_utopia_plot(p)
}
