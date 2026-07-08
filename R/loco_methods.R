#' Summarize LOCO Variable Importance
#'
#' Prints a formatted summary of `loco()` results. Equivalent to calling
#' `print()` on the object.
#'
#' @param object An object of class `"loco"` returned by [loco()].
#' @param ... Additional arguments passed to [print.loco()].
#' @return The `loco` object (invisibly).
#' @seealso [loco()], [print.loco()], [plot.loco()]
#' @method summary loco
#' @export
summary.loco <- function(object, ...) {
  print(object, ...)
}

#' Print LOCO Variable Importance
#'
#' Displays a table of `loco()` variable importance scores sorted from most
#' to least important. Shows sample size, number of covariates, test method,
#' loss, and whether the result came from split-sample or OOB mode.
#'
#' @param x An object of class `"loco"` returned by [loco()].
#' @param ... Additional arguments (currently unused).
#' @return The `loco` object (invisibly).
#' @seealso [loco()], [summary.loco()], [plot.loco()]
#' @method print loco
#' @export
print.loco <- function(x, ...) {
  cat("LOCO Variable Importance\n")
  cat("  n =", x$n, " p =", x$p, " method =", x$method, " loss =", x$loss, "\n")
  cat("  Mode:", if (x$split) "split-sample" else "OOB (no inference)", "\n\n")

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
  if (isTRUE(x$group) && "Members" %in% names(v)) {
    v$Members <- vapply(v$Members, function(m) paste(m, collapse = ", "),
                        character(1))
  }
  print(v, row.names = FALSE)
  invisible(x)
}

#' Plot LOCO Variable Importance
#'
#' Draws a horizontal variable-importance bar chart of `loco()` scores,
#' sorted from most to least important (largest at the top).
#'
#' @param x An object of class `"loco"` returned by [loco()].
#' @param fill.sig Bar fill for significant covariates (default
#'   `"darkorange"`). Non-significant covariates use `"gray70"`.
#' @param ... Additional arguments (currently unused).
#' @return A `ggplot` object.
#'
#' @details
#' Shares the package house styling with [plot_pdp()] (serif type, gray
#' panel, centered title, legend at the bottom). Colored horizontal bars
#' (`alpha = 0.75`) end in a black diamond tip point over a gray zero
#' reference line, with the x-axis hugged to the data. Bars are filled by
#' significance: a covariate is significant when its (one-sided) `p.value`
#' is below `alpha` (the value supplied to [loco()]); significant bars use
#' `fill.sig`, others `"gray70"`. In split-sample mode a two-sided
#' confidence-interval whisker is drawn from `CI.lower` to `CI.upper`
#' (the diamond tip sits at `Importance`, which is the interval midpoint by
#' construction). In OOB mode (`split = FALSE` in the original [loco()]
#' call) there is no confidence interval or p-value, so no whisker is drawn,
#' every bar takes the non-significant fill, and the title gains "(OOB)".
#'
#' @seealso [loco()] to compute the scores, [summary.loco()] for tabular
#'   output.
#'
#' @examplesIf rlang::is_installed(c("ggplot2", "ranger"))
#' set.seed(1995)
#' dat <- data.frame(y = rnorm(150), x1 = rnorm(150), x2 = rnorm(150),
#'                   x3 = rnorm(150))
#' dat$y <- dat$x1 + 0.5 * dat$x2 + rnorm(150, sd = 0.5)
#' mod <- ranger::ranger(y ~ ., data = dat, num.trees = 100)
#' vi <- loco(mod, split = FALSE)
#' plot(vi)
#' summary(vi)
#'
#' @importFrom rlang .data
#' @method plot loco
#' @export
plot.loco <- function(x, fill.sig = "darkorange", ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required but not installed.")
  }
  v <- x$vimp
  v$Variable <- factor(v$Variable, levels = v$Variable[order(v$Importance)])
  v$Significant <- !is.na(v$p.value) & v$p.value < x$alpha

  finite.bounds <- c(v$CI.lower[is.finite(v$CI.lower)],
                     v$CI.upper[is.finite(v$CI.upper)])
  xb <- .utopia_vi_xbreaks(c(v$Importance, finite.bounds))

  title <- if (!x$split) {
    "LOCO Variable Importance (OOB)"
  } else {
    "LOCO Variable Importance"
  }

  g <- ggplot2::ggplot(v, ggplot2::aes(x = .data[["Importance"]],
                                       y = .data[["Variable"]])) +
    ggplot2::geom_vline(xintercept = 0, color = "gray60", linewidth = 0.4) +
    ggplot2::geom_col(ggplot2::aes(fill = .data[["Significant"]]),
                      alpha = 0.75, width = 0.65)

  has_ci <- is.finite(v$CI.lower) & is.finite(v$CI.upper)
  if (any(has_ci)) {
    g <- g + ggplot2::geom_segment(
      ggplot2::aes(x = .data[["CI.lower"]], xend = .data[["CI.upper"]],
                   y = .data[["Variable"]], yend = .data[["Variable"]]),
      data = v[has_ci, , drop = FALSE],
      color = "black", linewidth = 0.5)
  }

  g +
    ggplot2::geom_point(size = 4, shape = 18, color = "black") +
    ggplot2::scale_fill_manual(
      name = "Significant",
      values = c(`FALSE` = "gray70", `TRUE` = fill.sig),
      labels = c(`FALSE` = "No", `TRUE` = "Yes")) +
    ggplot2::guides(fill = "none") +
    ggplot2::scale_x_continuous(
      breaks = xb$breaks, limits = xb$limits,
      expand = ggplot2::expansion(mult = c(0, 0.02))) +
    ggplot2::labs(title = title, x = NULL, y = NULL) +
    .utopia_pdp_theme()
}
