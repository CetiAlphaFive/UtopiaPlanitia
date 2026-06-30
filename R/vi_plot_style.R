#' House variable-importance theme (theme_few equivalent)
#'
#' Internal helper shared by [plot.cf_loco()] and [plot.cf_perm()]. Replicates
#' the Tufte-style `ggthemes::theme_few()` look from base `ggplot2::theme_bw()`
#' (white panel, thin gray border, x-only faint gridlines, inward serif ticks,
#' bold centered title, no legend) so neither method has to depend on
#' `ggthemes`. Callers must guard with `requireNamespace("ggplot2")`.
#'
#' @param base_size Base font size passed to [ggplot2::theme_bw()].
#' @return A `ggplot2` theme object.
#' @keywords internal
#' @noRd
.utopia_theme_vi <- function(base_size = 12) {
  ggplot2::theme_bw(base_size = base_size) +
    ggplot2::theme(
      panel.grid.minor   = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(color = "gray90",
                                                 linetype = "solid"),
      panel.border       = ggplot2::element_rect(color = "gray70", fill = NA,
                                                 linewidth = 0.5),
      axis.text.y        = ggplot2::element_text(size = 9, color = "black"),
      axis.text.x        = ggplot2::element_text(family = "serif", size = 9,
                                                 color = "black"),
      axis.title         = ggplot2::element_blank(),
      plot.title         = ggplot2::element_text(hjust = 0.5, face = "bold",
                                                 size = 13, color = "black"),
      legend.position    = "none",
      axis.ticks.length  = ggplot2::unit(-0.15, "cm"),  # inward ticks (Tufte)
      axis.ticks.x       = ggplot2::element_line(color = "black"),
      axis.ticks.y       = ggplot2::element_blank(),
      plot.margin        = ggplot2::margin(t = 4, r = 8, b = 2, l = 2,
                                           unit = "pt")
    )
}

#' Pretty x-axis breaks for VI bars
#'
#' Internal helper. Builds `pretty(n = 3)` breaks over a span that always
#' includes 0 (so LOCO panels with negatives get a zero break), padded ~2% so
#' bars/whiskers are not clipped. Ported verbatim from the Dem-Sat appendix
#' `generate_vi_plot()` recipe.
#'
#' @param v Numeric vector of values to span (e.g. importances, CI bounds).
#' @return A list with `breaks` (numeric) and `limits` (`range(breaks)`).
#' @keywords internal
#' @noRd
.utopia_vi_xbreaks <- function(v) {
  xspan <- range(c(0, v), na.rm = TRUE)
  pad   <- diff(xspan) * 0.02
  if (xspan[1] < 0) xspan[1] <- xspan[1] - pad
  xspan[2] <- xspan[2] + pad
  brks <- pretty(xspan, n = 3)
  list(breaks = brks, limits = range(brks))
}
