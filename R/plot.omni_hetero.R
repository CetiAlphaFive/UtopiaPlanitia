#' Plot an Omnibus Heterogeneity Test Object
#'
#' Visualizes the two preferred tests from [omni_hetero()]: the
#' Chernozhukov et al. (2018) calibration (Best Linear Predictor) test and
#' the sequential RATE test of Wager (2024), plus the OOB TOC curve from
#' Yadlowsky et al. (2024). The default layout targets a 7.5 x 5 inch
#' half-page figure: BLP scatter on the left, stacked TOC / per-fold
#' forest plot on the right, and a shared explanatory note below.
#'
#' @param x An `"omni_hetero"` object (from [omni_hetero()]).
#' @param which Character. One of `"both"` (default), `"blp"`, `"rate"`,
#'   `"toc"`, or `"folds"`. Selects which panel(s) to render.
#'   `"rate"` returns the TOC curve stacked above the per-fold forest
#'   plot as a `gtable`, with a shared note below.
#' @param bins Integer. Number of quantile bins of the predicted CATE used
#'   for the BLP binned-mean overlay. Default `5`. Ignored for RATE panels.
#' @param point_alpha Alpha for the BLP per-unit scatter. Default `0.25`.
#' @param ... Unused (for S3 consistency).
#'
#' @return When `which = "both"` or `"rate"`, a `gtable` object from
#'   [gridExtra::arrangeGrob()]; the grid is drawn as a side-effect when a
#'   graphics device is open. When `which = "blp"`, `"toc"`, or `"folds"`,
#'   a single `ggplot` object with its own caption.
#'
#' @details
#' The BLP panel renders the calibration regression
#' \deqn{\psi_i = \beta_1 \bar\tau + \beta_2 (\hat\tau_i - \bar\tau) + \epsilon_i}
#' (\eqn{\psi_i} = AIPW score; \eqn{\hat\tau_i} = OOB CATE) as a
#' partial-residual scatter for \eqn{\beta_2}.
#'
#' The Sequential RATE panel renders the per-fold t-statistics computed
#' inside [omni_hetero()]'s k-fold routine (Wager 2024) as a forest plot,
#' with the aggregate statistic \eqn{\sum_k t_k / \sqrt{K-1}} shown as a
#' diamond row. Each per-fold \eqn{t_k} is asymptotically \eqn{N(0,1)}
#' under H0; the aggregate is also \eqn{N(0,1)} under H0 by the
#' sequential validation construction of Wager (2024) (folds are *not*
#' independent -- training sets overlap -- but the martingale argument
#' yields the same limiting distribution).
#'
#' The OOB TOC panel draws TOC(q) = ATE among the top-q units ranked by
#' predicted CATE, minus the overall ATE, with a 95% pointwise band and
#' reports the AUTOC (or QINI) summary from
#' [grf::rank_average_treatment_effect()].
#'
#' @references
#' Chernozhukov, V., Demirer, M., Duflo, E., and Fernandez-Val, I. (2018).
#' Generic Machine Learning Inference on Heterogeneous Treatment Effects in
#' Randomized Experiments. NBER Working Paper 24678.
#'
#' Yadlowsky, S., Fleming, S., Shah, N., Brunskill, E., and Wager, S.
#' (2024). Evaluating Treatment Prioritization Rules via Rank-Weighted
#' Average Treatment Effects. JASA, forthcoming.
#'
#' Wager, S. (2024). Sequential Validation of Treatment Heterogeneity.
#' \doi{10.48550/arXiv.2405.05534}
#'
#' @seealso [omni_hetero()], [grf::test_calibration()],
#'   [grf::rank_average_treatment_effect()].
#'
#' @method plot omni_hetero
#' @export
#' @examplesIf rlang::is_installed(c("ggplot2", "gridExtra"))
#' library(grf)
#' set.seed(1995)
#' n <- 300; p <- 5
#' X <- matrix(rnorm(n * p), n, p)
#' W <- rbinom(n, 1, 0.5)
#' Y <- X[, 1] * W + rnorm(n)
#' cf <- causal_forest(X, Y, W, num.trees = 200)
#' oh <- omni_hetero(cf)
#' plot(oh)                  # BLP left, TOC + fold forest stacked right
#' plot(oh, which = "blp")
#' plot(oh, which = "rate")
#' plot(oh, which = "toc")
#' plot(oh, which = "folds")
plot.omni_hetero <- function(x,
                             which = c("both", "blp", "rate", "toc", "folds"),
                             bins = 5,
                             point_alpha = 0.25,
                             ...) {
  which <- match.arg(which)

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required but not installed.")
  }

  # Single-panel paths return a bare ggplot with its own caption.
  if (which == "blp") {
    return(plot_blp_panel(x, bins = bins, point_alpha = point_alpha))
  }
  if (which == "toc") {
    return(plot_rate_toc_panel(x))
  }
  if (which == "folds") {
    return(plot_rate_fold_panel(x))
  }

  if (!requireNamespace("gridExtra", quietly = TRUE)) {
    stop("Package 'gridExtra' is required for `which = \"both\"` or ",
         "\"rate\"`. Install it, or request a single-panel via ",
         "`which = \"blp\"` / `\"toc\"` / `\"folds\"`.")
  }

  # Composite paths: panels built without captions, then assembled with
  # one shared figure-level Note below the plot grid.
  g <- if (which == "rate") {
    main <- gridExtra::arrangeGrob(
      plot_rate_toc_panel(x, include_caption = FALSE),
      plot_rate_fold_panel(x, include_caption = FALSE),
      ncol = 1
    )
    gridExtra::arrangeGrob(main, figure_note_rate(x),
                           heights = grid::unit.c(grid::unit(1, "null"),
                                                  grid::unit(1.1, "in")))
  } else {
    # "both": BLP full left column, TOC top-right, folds bottom-right.
    main <- gridExtra::arrangeGrob(
      plot_blp_panel(x, bins = bins, point_alpha = point_alpha,
                     include_caption = FALSE),
      plot_rate_toc_panel(x, include_caption = FALSE),
      plot_rate_fold_panel(x, include_caption = FALSE),
      layout_matrix = rbind(c(1, 2),
                            c(1, 3)),
      widths  = grid::unit(c(1.1, 1), "null"),
      heights = grid::unit(c(1, 1),   "null")
    )
    gridExtra::arrangeGrob(main, figure_note_both(x),
                           heights = grid::unit.c(grid::unit(1, "null"),
                                                  grid::unit(1.1, "in")))
  }

  # print.gtable dumps layout text only. Draw explicitly when a device is
  # open; skip under batch Rscript so we don't spawn Rplots.pdf.
  if (grDevices::dev.cur() > 1L || interactive()) {
    grid::grid.newpage()
    grid::grid.draw(g)
  }
  invisible(g)
}

#' Shared figure-level Note for the combined BLP + RATE layout
#'
#' Builds a left-aligned black textGrob intended to be placed below the
#' plot grid by [plot.omni_hetero()]. Spells out every abbreviation so
#' the figure stands alone without surrounding prose.
#'
#' @param x An `"omni_hetero"` object.
#' @return A `grob`.
#' @keywords internal
#' @noRd
figure_note_both <- function(x) {
  rate <- attr(x, "rate")
  nd <- length(rate$dropped_folds %||% integer(0))
  drop_sentence <- if (nd > 0L) {
    " Dropped folds had degenerate Conditional Average Treatment Effect fits; aggregate normalization is unchanged, deflating it toward the null."
  } else ""

  body <- paste0(
    "LEFT panel: Best Linear Predictor calibration test (Chernozhukov ",
    "et al. 2018). Regresses doubly-robust (AIPW) treatment-effect ",
    "scores on the forest's out-of-bag predicted Conditional Average ",
    "Treatment Effect (CATE), centered at its mean. Slope near 1 ",
    "(dashed reference) = well-calibrated heterogeneity; slope near 0 ",
    "(dotted) = no detectable heterogeneity. Black points: within-bin ",
    "mean partial residuals with 95% intervals. ",
    "TOP-RIGHT: Out-of-bag Targeting Operator Characteristic (TOC) ",
    "curve (Yadlowsky et al. 2024). TOC(q) = Average Treatment Effect ",
    "among top-q units ranked by predicted CATE, minus the overall ",
    "Average Treatment Effect; curve above zero at small q = top-ranked ",
    "units gain more than average. AUTOC / QINI summarize the curve as ",
    "a scalar. Heuristic: out-of-bag-based, anti-conservative. ",
    "BOTTOM-RIGHT: Sequential Rank-Average Treatment Effect (RATE) test ",
    "(Wager 2024). Per-fold t_k = RATE_k / standard error_k from CATE ",
    "forests trained on prior folds; blue segments = 95% Wald interval ",
    "(t_k +/- 1.96). Orange diamond = aggregate t = sum(t_k) / sqrt(K-1), ",
    "standard normal under the null.",
    drop_sentence
  )
  note_grob(body, width = 145)
}

#' Shared figure-level Note for the RATE-only composite
#' @keywords internal
#' @noRd
figure_note_rate <- function(x) {
  rate <- attr(x, "rate")
  nd <- length(rate$dropped_folds %||% integer(0))
  drop_sentence <- if (nd > 0L) {
    paste0(" Dropped folds had degenerate fits; the aggregate ",
           "normalization sqrt(K - 1) is unchanged and is therefore ",
           "deflated toward the null.")
  } else ""
  body <- paste0(
    "Top: Out-of-bag Targeting Operator Characteristic (TOC) curve of ",
    "Yadlowsky et al. (2024). TOC(q) = Average Treatment Effect among ",
    "the top-q units ranked by predicted Conditional Average Treatment ",
    "Effect, minus the overall Average Treatment Effect. Ribbon: 95% ",
    "pointwise band. AUTOC/QINI summarize the curve as a scalar; ",
    "heuristic and anti-conservative under the null. Bottom: Sequential ",
    "Rank-Average Treatment Effect test of Wager (2024). Per-fold ",
    "t_k = RATE_k / standard error_k from Conditional Average Treatment ",
    "Effect forests trained on prior folds; aggregate = sum(t_k) / ",
    "sqrt(K - 1), standard normal under the null of no heterogeneity.",
    drop_sentence
  )
  note_grob(body, width = 95)
}

#' Build a left-aligned Note textGrob with "Note: " prefix
#' @keywords internal
#' @noRd
note_grob <- function(body, width = 100) {
  wrapped <- strwrap(paste0("Note: ", body), width = width)
  text <- paste(wrapped, collapse = "\n")
  grid::textGrob(
    text,
    x = grid::unit(4, "mm"),
    y = grid::unit(1, "npc") - grid::unit(1, "mm"),
    just = c("left", "top"),
    gp = grid::gpar(fontfamily = "serif", fontsize = 6.5, col = "black")
  )
}

# Small helper -- grf doesn't export `%||%` and neither does base R < 4.4.
`%||%` <- function(a, b) if (is.null(a)) b else a
