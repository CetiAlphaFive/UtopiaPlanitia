#' Print GATES Results
#'
#' @param x An object of class `"cf_gates"` returned by [gates()].
#' @param ... Additional arguments (currently unused).
#' @return The `cf_gates` object (invisibly).
#' @seealso [gates()], [plot_gates()]
#' @method print cf_gates
#' @export
print.cf_gates <- function(x, ...) {
  cat("Sorted Group Average Treatment Effects (GATES)\n")
  cat("  n =", x$n, " K =", x$K,
      " HT =", x$HT,
      " conf.level =", x$conf.level, "\n")
  cat("  Quantile cutoffs:", paste(format(x$quantile.cutoffs, digits = 3),
                                   collapse = ", "), "\n\n")

  g <- x$groups
  g$estimate <- round(g$estimate, 6)
  g$std.err  <- round(g$std.err, 6)
  g$lower    <- round(g$lower, 6)
  g$upper    <- round(g$upper, 6)
  g$p.left   <- signif(g$p.left, 3)
  g$p.right  <- signif(g$p.right, 3)
  cat("Group effects (lowest to highest predicted CATE):\n")
  print(g[, c("group", "estimate", "std.err", "lower", "upper")],
        row.names = FALSE)

  if (nrow(x$diff) > 0L) {
    d <- x$diff
    d$estimate <- round(d$estimate, 6)
    d$std.err  <- round(d$std.err, 6)
    d$lower    <- round(d$lower, 6)
    d$upper    <- round(d$upper, 6)
    cat("\nDifferenced targets:\n")
    print(d[, c("group", "estimate", "std.err", "lower", "upper")],
          row.names = FALSE)
  }

  invisible(x)
}

#' Summarize GATES Results
#'
#' @param object An object of class `"cf_gates"` returned by [gates()].
#' @param ... Additional arguments passed to [print.cf_gates()].
#' @return The `cf_gates` object (invisibly).
#' @method summary cf_gates
#' @export
summary.cf_gates <- function(object, ...) {
  print(object, ...)
}
