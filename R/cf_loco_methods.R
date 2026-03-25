#' Summarize LOCO Variable Importance
#'
#' Prints a formatted summary of LOCO variable importance results.
#' Equivalent to calling `print()` on the object.
#'
#' @param object An object of class `"cf_loco"` returned by [cf_loco()].
#' @param ... Additional arguments (currently unused).
#' @return The `cf_loco` object (invisibly).
#'
#' @seealso [cf_loco()], [print.cf_loco()], [plot.cf_loco()]
#'
#' @method summary cf_loco
#' @export
summary.cf_loco <- function(object, ...) {
  print(object, ...)
}

#' Print LOCO Variable Importance
#'
#' Displays a table of LOCO variable importance scores sorted from most to
#' least important. Shows sample size, number of covariates, and whether
#' scores are normalized.
#'
#' @param x An object of class `"cf_loco"` returned by [cf_loco()].
#' @param ... Additional arguments (currently unused).
#' @return The `cf_loco` object (invisibly).
#'
#' @seealso [cf_loco()], [summary.cf_loco()], [plot.cf_loco()]
#'
#' @method print cf_loco
#' @export
print.cf_loco <- function(x, ...) {

  cat("LOCO Variable Importance (Benard and Josse, 2023)\n")
  cat("  n =", x$n, " p =", x$p, "\n")
  cat("  Normalized:", x$normalized, "\n\n")

  vimp <- x$vimp[order(-x$vimp$Importance), ]
  rownames(vimp) <- NULL
  vimp$Importance <- round(vimp$Importance, 6)
  print(vimp, row.names = FALSE)

  invisible(x)
}

#' Plot LOCO Variable Importance
#'
#' Draws a horizontal lollipop chart of LOCO variable importance scores,
#' sorted from least to most important (bottom to top).
#'
#' @param x An object of class `"cf_loco"` returned by [cf_loco()].
#' @param ... Additional arguments (currently unused).
#' @return A `ggplot` object.
#'
#' @details
#' Variables are displayed as horizontal segments ending in points, with
#' length proportional to importance. When `normalize = TRUE` was used in
#' [cf_loco()], the y-axis label changes to "Importance (normalized)".
#'
#' @seealso [cf_loco()] to compute the scores, [summary.cf_loco()] for
#'   tabular output.
#'
#' @examplesIf rlang::is_installed("ggplot2")
#' library(grf)
#' set.seed(1995)
#' n <- 200; p <- 5
#' X <- matrix(rnorm(n * p), n, p)
#' colnames(X) <- paste0("X", seq_len(p))
#' W <- rbinom(n, 1, 0.5)
#' Y <- X[, 1] * W + rnorm(n)
#' cf <- causal_forest(X, Y, W, num.trees = 100)
#' vi <- cf_loco(cf)
#' plot(vi)
#'
#' @importFrom rlang .data
#' @method plot cf_loco
#' @export
plot.cf_loco <- function(x, ...) {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required but not installed.")
  }

  vimp <- x$vimp
  vimp$Variable <- factor(vimp$Variable,
                          levels = vimp$Variable[order(vimp$Importance)])

  y.lab <- if (x$normalized) "Importance (normalized)" else "Importance"

  ggplot2::ggplot(vimp, ggplot2::aes(x = .data[["Variable"]],
                                     y = .data[["Importance"]])) +
    ggplot2::geom_segment(ggplot2::aes(xend = .data[["Variable"]], y = 0,
                                       yend = .data[["Importance"]]),
                          linewidth = 0.8, color = "grey50") +
    ggplot2::geom_point(size = 3, color = "#5ab0c0") +
    ggplot2::coord_flip() +
    ggplot2::labs(x = NULL, y = y.lab,
                  title = "LOCO Variable Importance") +
    ggplot2::theme(
      text = ggplot2::element_text(size = 12, family = "serif"),
      panel.background = ggplot2::element_rect(fill = "#e6e6e6"),
      plot.title = ggplot2::element_text(hjust = 0.5),
      panel.border = ggplot2::element_blank(),
      axis.text = ggplot2::element_text(),
      axis.title = ggplot2::element_text(size = ggplot2::rel(1.2)),
      legend.key = ggplot2::element_blank(),
      complete = TRUE
    )
}
