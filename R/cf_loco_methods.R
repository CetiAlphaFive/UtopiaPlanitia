#' Summarize LOCO Variable Importance
#'
#' @param object An object of class `"cf_loco"` returned by [cf_loco()].
#' @param ... Additional arguments (currently unused).
#' @return The `cf_loco` object (invisibly).
#' @method summary cf_loco
#' @export
summary.cf_loco <- function(object, ...) {
  print(object, ...)
}

#' Print LOCO Variable Importance
#'
#' @param x An object of class `"cf_loco"` returned by [cf_loco()].
#' @param ... Additional arguments (currently unused).
#' @return The `cf_loco` object (invisibly).
#' @method print cf_loco
#' @export
print.cf_loco <- function(x, ...) {

  cat("LOCO Variable Importance (Bodory et al., 2023)\n")
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
#' Draws a horizontal lollipop chart of LOCO variable importance scores.
#'
#' @param x An object of class `"cf_loco"` returned by [cf_loco()].
#' @param ... Additional arguments (currently unused).
#' @return A `ggplot` object.
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
