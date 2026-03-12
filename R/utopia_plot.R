#' Wrap a ggMarginal plot for reliable printing
#'
#' Tags a `ggExtraPlot` with the `"utopia_plot"` class so that the package's
#' own [print.utopia_plot()] method is dispatched, avoiding the
#' `dev.interactive()` gate in ggExtra's default print method.
#'
#' @param p A ggplot object to wrap with [ggExtra::ggMarginal()].
#' @param ... Arguments passed to [ggExtra::ggMarginal()].
#' @return An object of class `c("utopia_plot", "ggExtraPlot", ...)`.
#' @keywords internal
#' @noRd
.wrap_marginal <- function(p, ...) {
  m <- ggExtra::ggMarginal(p, ...)
  class(m) <- c("utopia_plot", class(m))
  m
}

#' Print a UtopiaPlanitia plot
#'
#' Ensures `grid.newpage()` is always called before drawing, which fixes
#' silent‐plot issues in VS Code and other environments where
#' `grDevices::dev.interactive()` returns `FALSE`.
#'
#' @param x A `utopia_plot` object.
#' @param ... Ignored.
#' @return Invisibly returns `x`.
#' @export
print.utopia_plot <- function(x, ...) {
  grid::grid.newpage()
  grid::grid.draw(x)
  invisible(x)
}
