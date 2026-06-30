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
#' Internal helper. The axis is hugged to the data: limits span the values
#' together with 0 (so a zero reference is always in frame and LOCO/PermuCATE
#' negatives are shown), padded ~4%; tick breaks are `pretty(n = 4)` filtered to
#' that range. Hugging (rather than `range(pretty())`) avoids large dead margins
#' when one covariate dominates the importances.
#'
#' @param v Numeric vector of values to span (e.g. importances, CI bounds).
#' @return A list with `breaks` (numeric, in-range pretty ticks) and `limits`
#'   (the padded data span including 0).
#' @keywords internal
#' @noRd
.utopia_vi_xbreaks <- function(v) {
  v     <- v[is.finite(v)]
  xspan <- range(c(0, v))                            # always include 0
  pad   <- diff(xspan) * 0.04
  if (pad == 0) pad <- 0.04
  lims  <- c(xspan[1] - if (xspan[1] < 0) pad else 0,
             xspan[2] + pad)
  brks  <- pretty(xspan, n = 4)
  brks  <- brks[brks >= lims[1] & brks <= lims[2]]   # keep only in-range ticks
  list(breaks = brks, limits = lims)
}
