#' TOC curve panel (internal)
#'
#' Draws the Targeting Operator Characteristic curve from
#' [grf::rank_average_treatment_effect()] on the forest's OOB CATE
#' predictions. Canonical RATE visualization shown in the `grf` RATE
#' vignette (Yadlowsky et al. 2024).
#'
#' @param x An `"omni_hetero"` object carrying the RATE payload.
#'
#' @return A `ggplot` object.
#'
#' @importFrom rlang .data
#' @keywords internal
#' @noRd
plot_rate_toc_panel <- function(x, include_caption = TRUE) {

  rate <- attr(x, "rate")
  if (is.null(rate) || is.null(rate$toc_df) || nrow(rate$toc_df) == 0L) {
    return(
      ggplot2::ggplot() +
        ggplot2::annotate("text", x = 0, y = 0,
                          label = "TOC unavailable",
                          family = "serif", size = 3.2) +
        ggplot2::xlim(-1, 1) + ggplot2::ylim(-1, 1) +
        ggplot2::labs(title = "OOB TOC curve") +
        ggplot2::theme_void(base_family = "serif") +
        ggplot2::theme(
          plot.title = ggplot2::element_text(size = 10, face = "bold")
        )
    )
  }

  toc <- rate$toc_df
  toc$lwr <- toc$estimate - 1.96 * toc$std.err
  toc$upr <- toc$estimate + 1.96 * toc$std.err

  # grf labels target as "<priority> | <AUTOC/QINI>"; keep only the latter.
  raw_target <- rate$oob_rate_target
  target <- if (!is.na(raw_target)) {
    parts <- strsplit(raw_target, "\\s*\\|\\s*", perl = TRUE)[[1]]
    trimws(parts[length(parts)])
  } else {
    "RATE"
  }

  est_txt <- formatC(rate$oob_rate_est, format = "f", digits = 3)
  se_txt  <- formatC(rate$oob_rate_se,  format = "f", digits = 3)
  t_oob   <- rate$oob_rate_est / rate$oob_rate_se
  p_two   <- 2 * stats::pnorm(-abs(t_oob))
  p_txt   <- if (p_two < 0.001) "p < 0.001" else
    paste0("p = ", formatC(p_two, format = "f", digits = 3))

  subtitle <- paste0(
    target, " = ", est_txt, " (SE ", se_txt, "), ", p_txt,
    " (two-sided).\n",
    "Curve above 0 = top-ranked units gain more than average."
  )

  # Weighting gloss: uniform for AUTOC, rank-weighted for QINI (verified
  # against grf::rank_average_treatment_effect.fit source).
  caption <- format_note(paste0(
    "Targeting Operator Characteristic (TOC) curve of Yadlowsky et al. ",
    "(2024). For each fraction q in [0, 1] of units ranked top-down by ",
    "the forest's out-of-bag Conditional Average Treatment Effect (CATE), ",
    "plots TOC(q) = Average Treatment Effect among the top q minus the ",
    "overall Average Treatment Effect. Ribbon: pointwise 95% band. A ",
    "curve above zero at small q means top-ranked units gain more than ",
    "average -- evidence of heterogeneity. ", target,
    " summarizes the curve as a scalar; p-value is two-sided. Heuristic: ",
    "uses out-of-bag predictions and is anti-conservative under the null ",
    "(~30% rejection at nominal 5% per the grf RATE vignette)."
  ), width = 65)

  ggplot2::ggplot(toc,
                  ggplot2::aes(x = .data[["q"]], y = .data[["estimate"]])) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed",
                        colour = "grey40") +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = .data[["lwr"]], ymax = .data[["upr"]]),
      fill = "dodgerblue3", alpha = 0.2
    ) +
    ggplot2::geom_line(colour = "dodgerblue3", linewidth = 0.7) +
    ggplot2::geom_point(colour = "dodgerblue4", size = 1.4) +
    ggplot2::scale_x_continuous(
      limits = c(0, 1),
      breaks = seq(0, 1, by = 0.25)
    ) +
    ggplot2::labs(
      title = paste0("OOB TOC curve (", target, ")"),
      subtitle = subtitle,
      x = "q (fraction targeted)",
      y = "TOC(q)",
      caption = if (include_caption) caption else NULL
    ) +
    blp_rate_theme()
}

#' Sequential RATE fold forest panel (internal)
#'
#' Forest plot of per-fold t-statistics from [omni_hetero()]'s Sequential
#' RATE routine (Wager 2024), plus a diamond-marked aggregate row.
#'
#' @param x An `"omni_hetero"` object carrying the RATE payload.
#'
#' @return A `ggplot` object.
#'
#' @importFrom rlang .data
#' @keywords internal
#' @noRd
plot_rate_fold_panel <- function(x, include_caption = TRUE) {

  rate <- attr(x, "rate")
  if (is.null(rate)) {
    stop("No RATE payload attached to this omni_hetero object. ",
         "Re-run omni_hetero() to regenerate it.")
  }

  folds_df <- rate$folds_df

  # Degenerate case: no usable folds.
  if (is.null(folds_df) || nrow(folds_df) == 0L ||
      all(folds_df$dropped)) {
    reason_txt <- if (!is.null(rate$reason) && !is.na(rate$reason)) {
      rate$reason
    } else {
      "Sequential RATE produced no usable folds."
    }
    return(
      ggplot2::ggplot() +
        ggplot2::annotate("text", x = 0, y = 0,
                          label = paste(strwrap(reason_txt, width = 50),
                                        collapse = "\n"),
                          family = "serif", size = 3.2, hjust = 0.5) +
        ggplot2::xlim(-1, 1) + ggplot2::ylim(-1, 1) +
        ggplot2::labs(
          title = "Sequential RATE (Wager, 2024)",
          subtitle = "Unavailable at this sample size"
        ) +
        ggplot2::theme_void(base_family = "serif") +
        ggplot2::theme(
          plot.title = ggplot2::element_text(size = 10, face = "bold"),
          plot.subtitle = ggplot2::element_text(size = 7)
        )
    )
  }

  keep <- folds_df
  # Short codes for drop reasons (full reasons surface in warnings at
  # omni_hetero call time).
  short_code <- function(reason) {
    if (grepl("near-constant", reason, fixed = TRUE)) return("const. CATE")
    if (grepl("std.err",       reason, fixed = TRUE)) return("SE = 0")
    if (grepl("t-statistic",   reason, fixed = TRUE)) return("NaN t")
    "degenerate"
  }
  keep$label <- paste0("Fold ", keep$fold,
                       ifelse(keep$dropped, " [dropped]", ""))
  dropped_note <- if (any(keep$dropped)) {
    short <- vapply(keep$drop_reason[keep$dropped], short_code, character(1))
    byf   <- paste0("F", keep$fold[keep$dropped], ": ", short)
    paste0(" Dropped - ", paste(byf, collapse = "; "), ".")
  } else {
    ""
  }
  # Per-fold Wald-style interval is not standard for a test statistic.
  # Drop per-point segments; rely on the dashed 0 + dotted +/-1.96
  # reference lines to convey the rejection region.
  keep$type <- ifelse(keep$dropped, "dropped", "fold")

  # Aggregate row: draw with +/- 1.96 95% under-null band (SD = 1 under H0,
  # so +/-1.96 corresponds to the null-consistency interval).
  agg_row <- data.frame(
    fold = NA_integer_,
    n_test = NA_integer_,
    estimate = NA_real_,
    std.err = NA_real_,
    t_stat = rate$sequential_t,
    dropped = FALSE,
    drop_reason = NA_character_,
    label = "Aggregate",
    type = "aggregate"
  )
  keep$type <- factor(as.character(keep$type),
                      levels = c("fold", "dropped", "aggregate"))
  agg_row$type <- factor(agg_row$type, levels = c("fold", "dropped",
                                                   "aggregate"))
  plot_df <- rbind(keep[, intersect(names(keep), names(agg_row))], agg_row)

  lvl <- c(rev(keep$label), "Aggregate")
  plot_df$label <- factor(plot_df$label, levels = lvl)

  # Wald intervals: per-fold t_k +/- 1.96 (95% CI for true t under the
  # alternative, using plug-in SE of 1); aggregate +/- 1.96 (same under H0).
  plot_df$xmin <- plot_df$t_stat - 1.96
  plot_df$xmax <- plot_df$t_stat + 1.96

  p_val <- rate$p_value
  p_txt <- if (is.na(p_val)) {
    "p = NA"
  } else if (p_val < 0.001) {
    "p < 0.001"
  } else {
    paste0("p = ", formatC(p_val, format = "f", digits = 3))
  }
  verdict <- if (is.na(p_val)) "unavailable" else
             if (p_val <= 0.05) "reject H0" else "fail to reject H0"
  t_seq_txt <- if (is.na(rate$sequential_t)) "NA" else
    formatC(rate$sequential_t, format = "f", digits = 3)
  K  <- rate$num_folds
  nd <- length(rate$dropped_folds)

  drop_warn <- if (nd > 0L) paste0(" (", nd, " dropped)") else ""
  subtitle <- paste0(
    "agg. t = ", t_seq_txt, ", K = ", K, drop_warn,
    "; ", p_txt, " two-sided; ", verdict, ".\n",
    "|t_k| > 1.96 (dotted) = fold individually significant."
  )

  caption <- format_note(paste0(
    "Sequential Rank-Average Treatment Effect (RATE) test of Wager ",
    "(2024). For each fold k in 2..K, a Conditional Average Treatment ",
    "Effect forest is trained on folds 1..k-1 and its RATE is evaluated ",
    "on fold k, giving per-fold t-statistic t_k = RATE_k / standard ",
    "error_k. Blue dots and segments: t_k with 95% Wald interval ",
    "(t_k +/- 1.96). Orange diamond and segment: aggregate ",
    "t = sum(t_k) / sqrt(K - 1) with 95% under-null band; ",
    "asymptotically standard normal under the null hypothesis of no ",
    "heterogeneity, by the sequential validation construction. Dashed ",
    "line at 0: null. Dotted lines at +/- 1.96: per-fold 5% rejection ",
    "threshold.",
    if (nd > 0L) paste0(" Dropped folds had degenerate fits on the ",
                        "held-out sample (no reliable t_k); the aggregate ",
                        "normalization sqrt(K - 1) is unchanged, so the ",
                        "aggregate is deflated toward the null.") else "",
    dropped_note
  ), width = 65)

  # Base plot: dropped folds appear on y-axis (factor level retained) but
  # contribute no point/segment since t_stat is NA.
  plot_df_show <- plot_df[!is.na(plot_df$t_stat), ]
  ggplot2::ggplot(plot_df_show,
                  ggplot2::aes(x = .data[["t_stat"]],
                               y = .data[["label"]])) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed",
                        colour = "grey40") +
    ggplot2::geom_vline(xintercept = c(-1.96, 1.96), linetype = "dotted",
                        colour = "grey70") +
    # 95% Wald intervals: t_k +/- 1.96 for each fold; aggregate +/- 1.96.
    ggplot2::geom_segment(
      ggplot2::aes(x = .data[["xmin"]], xend = .data[["xmax"]],
                   y = .data[["label"]], yend = .data[["label"]],
                   colour = .data[["type"]]),
      linewidth = 0.6
    ) +
    ggplot2::geom_point(
      ggplot2::aes(colour = .data[["type"]],
                   shape  = .data[["type"]],
                   size   = .data[["type"]])
    ) +
    ggplot2::scale_colour_manual(
      values = c(fold = "dodgerblue3",
                 aggregate = "darkorange3",
                 dropped = "grey60"),
      guide = "none"
    ) +
    ggplot2::scale_shape_manual(
      values = c(fold = 16, aggregate = 18, dropped = 4),
      guide = "none"
    ) +
    ggplot2::scale_size_manual(
      values = c(fold = 2.2, aggregate = 3.6, dropped = 2),
      guide = "none"
    ) +
    ggplot2::scale_y_discrete(drop = FALSE) +
    ggplot2::labs(
      title = "Sequential RATE (Wager, 2024)",
      subtitle = subtitle,
      x = "per-fold t-statistic",
      y = NULL,
      caption = if (include_caption) caption else NULL
    ) +
    blp_rate_theme() +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank()
    )
}

#' RATE combined panel (internal)
#'
#' Stacks the OOB TOC curve (top) and the Sequential RATE fold forest
#' plot (bottom) into a single `gtable`.
#'
#' @param x An `"omni_hetero"` object.
#'
#' @return A `gtable` from [gridExtra::arrangeGrob()].
#'
#' @keywords internal
#' @noRd
plot_rate_panel <- function(x) {
  if (!requireNamespace("gridExtra", quietly = TRUE)) {
    stop("Package 'gridExtra' is required for the combined RATE panel. ",
         "Install it, or call plot_rate_toc_panel / plot_rate_fold_panel ",
         "directly.")
  }
  gridExtra::arrangeGrob(plot_rate_toc_panel(x),
                          plot_rate_fold_panel(x),
                          ncol = 1)
}
