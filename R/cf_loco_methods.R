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
#' Draws a horizontal variable-importance bar chart of LOCO scores, sorted
#' from most to least important (largest at the top).
#'
#' @param x An object of class `"cf_loco"` returned by [cf_loco()].
#' @param fill Bar fill color (default `"#1f78b4"`, the house blue).
#' @param ... Additional arguments (currently unused).
#' @return A `ggplot` object.
#'
#' @details
#' Shares the package house styling with [plot_pdp()] (serif type, gray panel,
#' centered title, legend at the bottom). The mark is a colored horizontal bar
#' (`alpha = 0.75`) ending in a black tip point, over a gray zero reference
#' line, with the x-axis hugged to the data. Variables are sorted descending so
#' the most important sits at the top. When `normalize = TRUE` was used in
#' [cf_loco()], the title becomes "LOCO Variable Importance (normalized)". The
#' bar color is controlled by `fill`.
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
plot.cf_loco <- function(x, fill = "#1f78b4", ...) {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required but not installed.")
  }

  vimp <- x$vimp
  vimp$Variable <- factor(vimp$Variable,
                          levels = vimp$Variable[order(vimp$Importance)])

  xb <- .utopia_vi_xbreaks(vimp$Importance)

  title <- if (x$normalized) {
    "LOCO Variable Importance (normalized)"
  } else {
    "LOCO Variable Importance"
  }

  ggplot2::ggplot(vimp, ggplot2::aes(x = .data[["Importance"]],
                                     y = .data[["Variable"]])) +
    ggplot2::geom_vline(xintercept = 0, color = "gray60", linewidth = 0.4) +
    ggplot2::geom_col(alpha = 0.75, fill = fill, width = 0.65) +
    ggplot2::geom_point(size = 2, shape = 16, color = "black") +
    ggplot2::scale_x_continuous(
      breaks = xb$breaks, limits = xb$limits,
      expand = ggplot2::expansion(mult = c(0, 0.02))) +
    ggplot2::labs(title = title, x = NULL, y = NULL) +
    .utopia_pdp_theme()
}
