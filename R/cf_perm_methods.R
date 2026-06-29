#' Print PermuCATE Variable Importance
#'
#' @param x An object of class `"cf_perm"` returned by [cf_perm()].
#' @param ... Additional arguments (currently unused).
#' @return The `cf_perm` object (invisibly).
#' @seealso [cf_perm()], [plot.cf_perm()]
#' @method print cf_perm
#' @export
print.cf_perm <- function(x, ...) {
  cat("PermuCATE Variable Importance (Paillard et al., 2025)\n")
  cat("  n =", x$n, " p =", x$p, " loss =", x$loss,
      " cross.fit =", x$cross.fit, " n.perm =", x$n.perm, "\n")
  cat("  Normalized:", x$normalized, "\n\n")

  v <- x$vimp[order(-x$vimp$Importance), ]
  rownames(v) <- NULL
  if (!all(is.na(v$p.value))) {
    stars <- as.character(cut(v$p.value,
      breaks = c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf),
      labels = c("***", "**", "*", ".", "")))
    stars[is.na(stars)] <- ""
    v$sig <- stars
    v$p.value <- signif(v$p.value, 3)
  }
  v$Importance <- round(v$Importance, 6)
  print(v, row.names = FALSE)
  invisible(x)
}

#' Summarize PermuCATE Variable Importance
#'
#' @param object An object of class `"cf_perm"` returned by [cf_perm()].
#' @param ... Additional arguments passed to [print.cf_perm()].
#' @return The `cf_perm` object (invisibly).
#' @seealso [cf_perm()]
#' @method summary cf_perm
#' @export
summary.cf_perm <- function(object, ...) {
  print(object, ...)
}

#' Plot PermuCATE Variable Importance
#'
#' Draws a horizontal lollipop chart of PermuCATE importance scores. When
#' confidence bounds are available (not normalized), a one-sided lower
#' confidence bound is drawn as a whisker (the upper bound is unbounded by
#' construction). Points are colored by significance at the object's
#' `conf.level` (a covariate is significant when its lower confidence bound
#' exceeds zero).
#'
#' @param x An object of class `"cf_perm"` returned by [cf_perm()].
#' @param ... Additional arguments (currently unused).
#' @return A `ggplot` object.
#' @importFrom rlang .data
#' @method plot cf_perm
#' @export
plot.cf_perm <- function(x, ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required but not installed.")
  }
  v <- x$vimp
  v$Variable <- factor(v$Variable, levels = v$Variable[order(v$Importance)])
  v$Significant <- !is.na(v$CI.lower) & v$CI.lower > 0

  g <- ggplot2::ggplot(v, ggplot2::aes(x = .data[["Variable"]],
                                       y = .data[["Importance"]])) +
    ggplot2::geom_segment(ggplot2::aes(xend = .data[["Variable"]], y = 0,
                                       yend = .data[["Importance"]]),
                          linewidth = 0.8, color = "grey50") +
    ggplot2::geom_point(ggplot2::aes(color = .data[["Significant"]]), size = 3) +
    ggplot2::coord_flip() +
    ggplot2::labs(x = NULL, y = "PermuCATE importance",
                  title = "PermuCATE Variable Importance") +
    ggplot2::theme(
      text = ggplot2::element_text(size = 12, family = "serif"),
      panel.background = ggplot2::element_rect(fill = "#e6e6e6"),
      plot.title = ggplot2::element_text(hjust = 0.5),
      panel.border = ggplot2::element_blank(),
      complete = TRUE
    )

  # One-sided lower confidence bound (CI.upper is +Inf by construction).
  if (!x$normalized && any(is.finite(v$CI.lower))) {
    g <- g + ggplot2::geom_linerange(
      ggplot2::aes(ymin = .data[["CI.lower"]], ymax = .data[["Importance"]]),
      data = v[is.finite(v$CI.lower), , drop = FALSE],
      color = "grey40")
  }
  g
}
