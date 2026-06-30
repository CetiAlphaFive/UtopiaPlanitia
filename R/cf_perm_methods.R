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

  # Per-covariate missingness table, shown only when X carried NAs. Guarded with
  # is.null() so objects from older versions (without miss.rate) print unchanged.
  mr <- x$miss.rate
  if (!is.null(mr) && any(mr > 0)) {
    scope <- if (!is.null(x$miss.scope) && !is.na(x$miss.scope)) {
      x$miss.scope
    } else {
      "observed"
    }
    idx <- which(mr > 0)
    tab <- data.frame(
      Variable  = names(mr)[idx],
      Miss.rate = round(unname(mr[idx]), 3),
      n.missing = round(unname(mr[idx]) * x$n),
      stringsAsFactors = FALSE
    )
    cat("\nMissing covariate values (observed-support permutation; scope = ",
        scope, "):\n", sep = "")
    print(tab, row.names = FALSE)
  }
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
#' Draws a horizontal variable-importance bar chart of PermuCATE scores,
#' sorted from most to least important (largest at the top).
#'
#' @param x An object of class `"cf_perm"` returned by [cf_perm()].
#' @param fill.sig Bar fill for significant covariates (default `"#1f78b4"`,
#'   the house blue). Non-significant covariates use `"gray70"`.
#' @param ... Additional arguments (currently unused).
#' @return A `ggplot` object.
#'
#' @details
#' Styled to the package's house variable-importance look: a white
#' `theme_few()`-equivalent panel (hand-rolled from [ggplot2::theme_bw()], no
#' `ggthemes` dependency), colored horizontal bars (`alpha = 0.75`) ending in a
#' black tip point, a gray zero reference line, inward serif x-axis ticks, and
#' faint x-only gridlines. Bars are filled by significance at the object's
#' `conf.level` (a covariate is significant when its lower confidence bound
#' exceeds zero): significant bars use `fill.sig`, others `"gray70"`. When
#' confidence bounds are available (not normalized), a one-sided lower
#' confidence bound is drawn as a horizontal whisker from `CI.lower` to the
#' importance tip (the upper bound is `+Inf` by construction, so there is no
#' upper whisker). For normalized objects the bounds are `NA`, so no whisker is
#' drawn, all bars take the non-significant fill, and the title gains
#' "(normalized)".
#'
#' @seealso [cf_perm()] to compute the scores, [summary.cf_perm()] for tabular
#'   output.
#'
#' @importFrom rlang .data
#' @method plot cf_perm
#' @export
plot.cf_perm <- function(x, fill.sig = "#1f78b4", ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required but not installed.")
  }
  v <- x$vimp
  v$Variable <- factor(v$Variable, levels = v$Variable[order(v$Importance)])
  v$Significant <- !is.na(v$CI.lower) & v$CI.lower > 0

  # Span x-breaks over importances and any finite lower bounds so the whisker
  # is never clipped.
  finite.lower <- v$CI.lower[is.finite(v$CI.lower)]
  xb <- .utopia_vi_xbreaks(c(v$Importance, finite.lower))

  title <- if (x$normalized) {
    "PermuCATE Variable Importance (normalized)"
  } else {
    "PermuCATE Variable Importance"
  }

  g <- ggplot2::ggplot(v, ggplot2::aes(x = .data[["Importance"]],
                                       y = .data[["Variable"]])) +
    ggplot2::geom_vline(xintercept = 0, color = "gray60", linewidth = 0.4) +
    ggplot2::geom_col(ggplot2::aes(fill = .data[["Significant"]]),
                      alpha = 0.75)

  # One-sided lower confidence bound (CI.upper is +Inf by construction), drawn
  # over the bar so the inferential whisker stays visible.
  if (!x$normalized && any(is.finite(v$CI.lower))) {
    g <- g + ggplot2::geom_segment(
      ggplot2::aes(x = .data[["CI.lower"]], xend = .data[["Importance"]],
                   y = .data[["Variable"]], yend = .data[["Variable"]]),
      data = v[is.finite(v$CI.lower), , drop = FALSE],
      color = "grey40", linewidth = 0.5)
  }

  g +
    ggplot2::geom_point(size = 2.5, shape = 16, color = "black") +
    ggplot2::scale_fill_manual(
      values = c(`FALSE` = "gray70", `TRUE` = fill.sig)) +
    ggplot2::scale_x_continuous(
      breaks = xb$breaks, limits = xb$limits,
      expand = ggplot2::expansion(mult = c(0, 0.02))) +
    ggplot2::labs(title = title, x = NULL, y = NULL) +
    .utopia_theme_vi()
}
