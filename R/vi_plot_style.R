# Internal plotting helpers shared by plot.cf_loco() / plot.cf_perm().
# The shared theme is .utopia_pdp_theme() (R/plot_pdp.R) so the VI plots match
# the PDP panels; this file only carries the x-break helper.

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
