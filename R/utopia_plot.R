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

#' Wrap a plain ggplot (no marginal histograms) as a `utopia_plot`.
#'
#' Converts the ggplot to a grob and tags it `"utopia_plot"` so
#' [print.utopia_plot()] renders it via `grid::grid.draw()`. Used by the
#' discrete subgroup plots, where ggMarginal histograms are meaningless.
#' @param p A ggplot object.
#' @return An object of class `c("utopia_plot", "gtable", "grob", ...)`.
#' @keywords internal
#' @noRd
.as_utopia_plot <- function(p) {
  g <- ggplot2::ggplotGrob(p)
  class(g) <- c("utopia_plot", class(g))
  g
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
#'
#' @seealso [plot_pdp()] and [plot_scatter()], which return `utopia_plot`
#'   objects.
#'
#' @keywords internal
#' @export
print.utopia_plot <- function(x, ...) {
  grid::grid.newpage()
  grid::grid.draw(x)
  invisible(x)
}
