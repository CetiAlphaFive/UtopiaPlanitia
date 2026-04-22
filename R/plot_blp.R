#' BLP calibration panel (internal)
#'
#' Builds the partial-residual scatter for the Chernozhukov et al. (2018)
#' calibration test. Called by [plot.omni_hetero()]; not intended for
#' direct use.
#'
#' @param x An `"omni_hetero"` object carrying the BLP payload.
#' @param bins Integer. Number of quantile bins of the predicted CATE used
#'   for the binned-mean overlay. Default `5`.
#' @param point_alpha Alpha for the per-unit scatter. Default `0.25`.
#'
#' @return A `ggplot` object.
#'
#' @importFrom rlang .data
#' @keywords internal
#' @noRd
plot_blp_panel <- function(x, bins = 5, point_alpha = 0.25,
                           include_caption = TRUE) {

  blp <- attr(x, "blp")
  if (is.null(blp)) {
    stop("No BLP payload attached to this omni_hetero object. ",
         "Re-run omni_hetero() to regenerate it.")
  }

  # Partial-residual y: response minus the fitted contribution of the
  # constant (mean-forest) regressor. Leaves only the slope component so
  # the fitted line passes through the origin.
  y_partial <- blp$dr - blp$beta_mean * blp$mean_pred
  x_centered <- blp$centered

  pts <- data.frame(x = x_centered, y = y_partial)

  grid <- seq(min(pts$x), max(pts$x), length.out = 200)
  fit_df <- data.frame(
    x   = grid,
    y   = blp$beta_diff * grid,
    lwr = blp$beta_diff * grid - 1.96 * abs(grid) * blp$se_diff,
    upr = blp$beta_diff * grid + 1.96 * abs(grid) * blp$se_diff
  )

  bins <- max(2L, as.integer(bins))
  probs <- seq(0, 1, length.out = bins + 1)
  breaks <- stats::quantile(x_centered, probs = probs, na.rm = TRUE,
                            names = FALSE)
  breaks <- unique(breaks)
  bin_df <- NULL
  if (length(breaks) >= 3L) {
    bin_id <- cut(x_centered, breaks = breaks, include.lowest = TRUE,
                  labels = FALSE)
    bin_centers <- tapply(x_centered, bin_id, mean)
    bin_means   <- tapply(y_partial, bin_id, mean)
    bin_ses     <- tapply(y_partial, bin_id,
                          function(v) stats::sd(v) / sqrt(length(v)))
    bin_df <- data.frame(
      x   = as.numeric(bin_centers),
      y   = as.numeric(bin_means),
      lwr = as.numeric(bin_means) - 1.96 * as.numeric(bin_ses),
      upr = as.numeric(bin_means) + 1.96 * as.numeric(bin_ses)
    )
  }

  beta_txt <- formatC(blp$beta_diff, format = "f", digits = 3)
  se_txt   <- formatC(blp$se_diff,   format = "f", digits = 3)
  p_txt    <- if (blp$p_diff < 0.001) "p < 0.001" else
    paste0("p = ", formatC(blp$p_diff, format = "f", digits = 3))

  # One-sided test: H0: beta_diff <= 0 (no / wrong-direction heterogeneity).
  verdict <- if (blp$p_diff <= 0.05) {
    "reject H0 of no heterogeneity"
  } else {
    "fail to reject H0 of no heterogeneity"
  }

  verdict_short <- if (blp$p_diff <= 0.05) "reject H0" else "fail to reject H0"
  subtitle <- paste0(
    "slope = ", beta_txt, " (SE ", se_txt, "), ",
    p_txt, " one-sided; ", verdict_short, ".\n",
    "near dashed = calibrated; near dotted = no heterogeneity."
  )

  # BLP fit + overlay.
  p <- ggplot2::ggplot() +
    ggplot2::geom_hline(yintercept = 0, linetype = "dotted",
                        colour = "grey40") +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed",
                         colour = "grey40") +
    ggplot2::geom_point(
      data = pts,
      ggplot2::aes(x = .data[["x"]], y = .data[["y"]]),
      colour = "dodgerblue3", alpha = point_alpha, size = 0.8
    ) +
    ggplot2::geom_ribbon(
      data = fit_df,
      ggplot2::aes(x = .data[["x"]],
                   ymin = .data[["lwr"]], ymax = .data[["upr"]]),
      fill = "darkorange1", alpha = 0.2
    ) +
    ggplot2::geom_line(
      data = fit_df,
      ggplot2::aes(x = .data[["x"]], y = .data[["y"]]),
      colour = "darkorange3", linewidth = 0.8
    )

  if (!is.null(bin_df)) {
    p <- p +
      ggplot2::geom_errorbar(
        data = bin_df,
        ggplot2::aes(x = .data[["x"]],
                     ymin = .data[["lwr"]], ymax = .data[["upr"]]),
        width = 0, colour = "black", linewidth = 0.5
      ) +
      ggplot2::geom_point(
        data = bin_df,
        ggplot2::aes(x = .data[["x"]], y = .data[["y"]]),
        colour = "black", size = 1.6
      )
  }

  n_bins <- if (is.null(bin_df)) 0L else nrow(bin_df)
  caption <- format_note(paste0(
    "Best Linear Predictor calibration test of Chernozhukov et al. (2018). ",
    "Regresses the doubly-robust (AIPW) treatment-effect score on the ",
    "forest's out-of-bag predicted Conditional Average Treatment Effect ",
    "(CATE), centered at its mean. Blue points: one partial residual per ",
    "unit. Orange line: fitted slope with 95% Wald band; p-value is ",
    "one-sided (null: slope <= 0). Dashed line (slope = 1) marks perfect ",
    "CATE calibration; dotted line (slope = 0) marks the null of no ",
    "heterogeneity. Black points: mean partial residual inside each of ",
    n_bins, " equal-count bins of predicted CATE, with +/- 1.96 standard ",
    "errors of the bin mean -- the empirical analogue of the fitted line."
  ), width = 80)

  p +
    ggplot2::labs(
      title = "BLP calibration test (Chernozhukov et al., 2018)",
      subtitle = subtitle,
      x = "Predicted CATE (centered)",
      y = "Partial DR score",
      caption = if (include_caption) caption else NULL
    ) +
    blp_rate_theme()
}

#' Shared theme used by the BLP and RATE panels
#'
#' Sized for a 7.5 x 5 inch half-page figure. Keep fonts identical across
#' panels so side-by-side layouts have matching visual weight.
#'
#' @keywords internal
#' @noRd
blp_rate_theme <- function() {
  ggplot2::theme(
    plot.title       = ggplot2::element_text(size = 10, face = "bold"),
    plot.subtitle    = ggplot2::element_text(size = 7),
    plot.caption     = ggplot2::element_text(hjust = 0, size = 6.5,
                                             colour = "black"),
    panel.background = ggplot2::element_rect(fill = "white",
                                             colour = "white"),
    axis.line        = ggplot2::element_line(linewidth = 0.4,
                                             colour = "black"),
    axis.title       = ggplot2::element_text(size = 8, face = "bold"),
    axis.text        = ggplot2::element_text(colour = "grey30", size = 7),
    panel.grid.major = ggplot2::element_line(colour = "grey90",
                                             linewidth = 0.3),
    panel.grid.minor = ggplot2::element_blank(),
    axis.ticks       = ggplot2::element_blank(),
    text             = ggplot2::element_text(family = "serif"),
    plot.margin      = ggplot2::margin(4, 6, 4, 6)
  )
}

#' Format a figure note
#'
#' Prefixes the body with "Note: " and wraps at a panel-appropriate width
#' so standalone readers can decode the figure without the surrounding
#' prose.
#'
#' @param body Character scalar (the note body, abbreviations already
#'   spelled out).
#' @param width Integer width for `strwrap`. Left (BLP) panel uses ~80;
#'   right-column panels (TOC, folds) use ~65.
#'
#' @keywords internal
#' @noRd
format_note <- function(body, width = 80) {
  wrapped <- strwrap(paste0("Note: ", body), width = width)
  paste(wrapped, collapse = "\n")
}
