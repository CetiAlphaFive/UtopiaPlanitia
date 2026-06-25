#' Partial Dependence Plot for Causal Forest
#'
#' Computes and plots the partial dependence of the CATE on one or two
#' covariates from a fitted causal forest. Unlike [plot_scatter()], which shows
#' individual OOB CATEs, this marginalizes over the remaining covariates by
#' averaging predictions across a held-out sample at each grid point
#' (Friedman, 2001).
#'
#' @param c.forest A fitted causal forest model object from the \code{grf} package.
#' @param x_var Character. Name of the primary covariate.
#' @param y_var Character or `NULL`. If supplied, a 2-way PDP (tile plot) is
#'   produced; otherwise a 1-way PDP (line plot).
#' @param grid_size Integer. Number of grid points per variable (default 50).
#'   For 2-way PDP the grid has `grid_size^2` points, so moderate values are
#'   recommended.
#' @param n_max Integer. Maximum rows of the covariate matrix used for
#'   averaging. If `nrow(X.orig) > n_max`, a random subsample is drawn.
#'   Default is 2000 for 1-way PDP and 500 for 2-way PDP (PD averaging
#'   converges fast; pass `n_max` explicitly to override).
#' @param show_ate_region Logical. Draw ATE +/- 1.96 SE band? Only used for
#'   1-way PDP (default `TRUE`).
#' @param show_scatter Logical. Overlay individual OOB CATEs on the 1-way
#'   PDP? (default `TRUE`).
#' @param trim Logical. For 2-way PDP only: mask grid points outside the
#'   convex hull of the training data to avoid extrapolation into unsupported
#'   regions (default `TRUE`). Uses [grDevices::chull()]. Ignored for 1-way
#'   PDP.
#' @param x.limits x axis limits specified as c() vector. Defaults to range of the grid.
#' @param y.limits y axis limits specified as c() vector. Defaults to range of the grid.
#' @param color.var Character or `NULL`. Name of a covariate to split the 1-way
#'   PDP by. A separate PD curve is drawn for each level. Only for 1-way PDP.
#' @param color.cat Character vector or `NULL`. Labels for the levels of
#'   `color.var`, in order of sorted unique values. If `NULL`, the raw values
#'   are used as labels.
#' @param color.lab Character or `NULL`. Legend title for the grouping variable.
#'   Defaults to `color.var`.
#' @param xlab Character or `NULL`. Custom x-axis label. Defaults to `x_var`.
#' @param num.threads Integer or `NULL`. Number of threads for `predict()`.
#'   Passed directly to [grf::predict.causal_forest()]. `NULL` (default) uses
#'   all available threads.
#' @param subgroup Logical. If `TRUE`, ignore the partial-dependence machinery
#'   and instead plot the doubly-robust (AIPW) subgroup average treatment effect
#'   at each observed value of `x_var` (and, if `y_var` is given, each cell of the
#'   `x_var` x `y_var` cross), via [grf::average_treatment_effect()] with
#'   `subset =`. Intended for binary or low-cardinality integer covariates.
#'   1-way: points with 95% CIs per level. 2-way: a heatmap of subgroup ATEs with
#'   a `*` on cells whose 95% CI excludes 0. Levels are `sort(unique(.))` with no
#'   binning; a level/cell with too few units to estimate is warned and dropped.
#'   When `TRUE`, `grid_size`, `n_max`, `trim`, `show_scatter`, `color.var`,
#'   `color.cat`, `color.lab`, `x.limits`, and `num.threads` are ignored;
#'   `show_ate_region` (1-way reference band), `y.limits` (1-way only), and `xlab`
#'   still apply.
#'   Default `FALSE`.
#'
#' @return A `utopia_plot` object. For the PDP modes this wraps a `ggExtraPlot`
#'   with marginal histograms; for `subgroup = TRUE` it wraps a plain ggplot grob
#'   (no marginal histograms).
#'
#' @details
#' **Computational cost.** Each grid point requires a full `predict()` call on
#' the (possibly subsampled) covariate matrix. For 2-way PDPs, grid points
#' outside the convex hull of the training data are filtered *before*
#' prediction when `trim = TRUE`, which typically removes 30-50\% of the grid.
#' Combined with a lower default `n_max` (500 vs 2000), this yields a
#' ~6-7x speedup over evaluating the full `grid_size^2` grid.
#'
#' @references
#' Friedman, J. H. (2001). Greedy Function Approximation: A Gradient Boosting
#' Machine. *Annals of Statistics*, 29(5), 1189--1232.
#'
#' @seealso [plot_scatter()] for individual OOB CATEs (not marginalized),
#'   [plot.causal_forest()] to call this via `plot(cf, type = "pdp")`.
#'
#' @importFrom rlang .data
#' @export
#' @examplesIf rlang::is_installed(c("ggplot2", "ggExtra"))
#' library(grf)
#' set.seed(1995)
#' n <- 200; p <- 5
#' X <- matrix(rnorm(n * p), n, p)
#' colnames(X) <- paste0("X", seq_len(p))
#' W <- rbinom(n, 1, 0.5)
#' Y <- X[, 1] * W + rnorm(n)
#' cf <- causal_forest(X, Y, W, num.trees = 100)
#' plot_pdp(cf, x_var = "X1")
#' plot_pdp(cf, x_var = "X1", y_var = "X2", grid_size = 20)
#' # Discrete subgroup ATE instead of a PDP curve
#' Xb <- X; Xb[, 1] <- rbinom(n, 1, 0.5)
#' cfb <- causal_forest(Xb, Y, W, num.trees = 100)
#' plot_pdp(cfb, x_var = "X1", subgroup = TRUE)
plot_pdp <- function(c.forest, x_var, y_var = NULL,
                     grid_size = 50, n_max = 2000,
                     show_ate_region = TRUE, show_scatter = TRUE,
                     trim = TRUE,
                     x.limits = NULL, y.limits = NULL,
                     color.var = NULL, color.cat = NULL,
                     color.lab = NULL, xlab = NULL,
                     num.threads = NULL,
                     subgroup = FALSE) {

  if (!inherits(c.forest, "causal_forest")) {
    stop("c.forest must be a causal_forest object from the grf package.")
  }
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required but not installed.")
  }

  X.orig <- c.forest$X.orig
  all.vars <- c(x_var, y_var, color.var)
  for (v in all.vars) {
    if (!(v %in% colnames(X.orig))) {
      stop(v, " is not in the covariate matrix. ",
           "See colnames(c.forest$X.orig) for available variables.")
    }
  }
  if (!is.null(color.var) && !is.null(y_var)) {
    stop("color.var grouping is only supported for 1-way PDP.")
  }

  if (subgroup) {
    if (is.null(y_var)) {
      return(.plot_subgroup_1way(c.forest, x_var, show_ate_region,
                                 y.limits, xlab))
    }
    return(.plot_subgroup_2way(c.forest, x_var, y_var, xlab))
  }

  if (!requireNamespace("ggExtra", quietly = TRUE)) {
    stop("Package 'ggExtra' is required but not installed.")
  }

  # Lower default n_max for 2-way PDP (PD averaging converges by ~500 rows)
  n_max_explicit <- "n_max" %in% names(match.call())
  if (!is.null(y_var) && !n_max_explicit) {
    n_max <- 500L
  }

  # Subsample for speed
  set.seed(1995)
  n <- nrow(X.orig)
  idx <- if (n > n_max) sample.int(n, n_max) else seq_len(n)
  X.sub <- X.orig[idx, , drop = FALSE]

  if (is.null(y_var)) {
    .plot_pdp_1way(c.forest, x_var, X.sub, grid_size,
                   show_ate_region, show_scatter, x.limits, y.limits,
                   color.var, color.cat, color.lab, xlab, num.threads)
  } else {
    .plot_pdp_2way(c.forest, x_var, y_var, X.sub, grid_size,
                   trim, x.limits, y.limits, num.threads)
  }
}


# -- internal helpers (not exported) ------------------------------------------

#' Extract a single column as a vector, regardless of whether `X` is a
#' matrix, base data.frame, or tibble. Tibbles ignore `drop = TRUE`, so
#' `X[, v]` would otherwise return a 1-column tibble and break downstream
#' `data.frame()` construction.
#' @keywords internal
#' @noRd
.col_vec <- function(X, v) {
  if (is.data.frame(X)) X[[v]] else X[, v]
}

#' @keywords internal
#' @noRd
.compute_pd <- function(c.forest, vars, grid, X.sub, num.threads = NULL) {
  n.sub <- nrow(X.sub)
  n.grid <- nrow(grid)
  # Stack n.grid copies of X.sub, overwriting target columns with grid values
  X.big <- X.sub[rep(seq_len(n.sub), times = n.grid), , drop = FALSE]
  for (v in vars) {
    X.big[, v] <- rep(grid[[v]], each = n.sub)
  }
  preds <- stats::predict(c.forest, newdata = X.big,
                           num.threads = num.threads)$predictions
  # Average within each grid-point block
  colMeans(matrix(preds, nrow = n.sub, ncol = n.grid))
}

#' Doubly-robust AIPW ATE on one subset, with 95% CI and significance flag.
#' Returns NULL (and warns) if grf cannot estimate the subset.
#' @keywords internal
#' @noRd
.subgroup_ate_one <- function(c.forest, subset, label) {
  res <- tryCatch(
    grf::average_treatment_effect(c.forest, subset = subset),
    error = function(e) {
      warning("plot_pdp(subgroup): ", label, " skipped (",
              conditionMessage(e), ")", call. = FALSE)
      NULL
    }
  )
  if (is.null(res)) return(NULL)
  est <- res[["estimate"]]
  se  <- res[["std.err"]]
  data.frame(
    estimate = est,
    std.err  = se,
    lower    = est - 1.96 * se,
    upper    = est + 1.96 * se,
    signif   = (est - 1.96 * se) > 0 | (est + 1.96 * se) < 0
  )
}

#' Assemble per-level subgroup ATEs for a single covariate.
#' Levels are sort(unique(x)); failed levels are warned + dropped.
#' @keywords internal
#' @noRd
.subgroup_ate_1way_df <- function(c.forest, x_var) {
  x.col <- .col_vec(c.forest$X.orig, x_var)
  lv <- sort(unique(x.col[!is.na(x.col)]))
  rows <- lapply(lv, function(v) {
    s <- !is.na(x.col) & x.col == v
    one <- .subgroup_ate_one(c.forest, s, paste0(x_var, " = ", v))
    if (is.null(one)) return(NULL)
    one$level <- v
    one
  })
  df <- do.call(rbind, rows)
  if (is.null(df) || nrow(df) == 0L) {
    stop("plot_pdp(subgroup): no level of '", x_var,
         "' had enough units to estimate an ATE.", call. = FALSE)
  }
  df
}

#' Shared ggplot theme for the subgroup plots (matches the PDP panels).
#' @keywords internal
#' @noRd
.utopia_pdp_theme <- function() {
  ggplot2::theme(
    text = ggplot2::element_text(size = 12, family = "serif"),
    panel.background = ggplot2::element_rect(fill = "#e6e6e6"),
    plot.title = ggplot2::element_text(hjust = 0.5),
    legend.position = "bottom",
    panel.border = ggplot2::element_blank(),
    axis.text = ggplot2::element_text(),
    axis.title = ggplot2::element_text(size = ggplot2::rel(1.2)),
    strip.text = ggplot2::element_text(hjust = 0),
    strip.background = ggplot2::element_rect(fill = NA, color = NA),
    legend.key = ggplot2::element_blank(),
    complete = TRUE
  )
}

#' 1-way discrete subgroup ATE plot: point + 95% CI per level.
#' @keywords internal
#' @noRd
.plot_subgroup_1way <- function(c.forest, x_var, show_ate_region,
                                y.limits, xlab) {
  df <- .subgroup_ate_1way_df(c.forest, x_var)
  df$level <- factor(as.character(df$level),
                     levels = as.character(sort(unique(df$level))))
  x.label <- if (!is.null(xlab)) xlab else x_var

  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data[["level"]],
                                        y = .data[["estimate"]]))

  if (show_ate_region) {
    ate.result <- grf::average_treatment_effect(c.forest)
    ate <- ate.result[["estimate"]]
    hi  <- ate + 1.96 * ate.result[["std.err"]]
    lo  <- ate - 1.96 * ate.result[["std.err"]]
    p <- p +
      ggplot2::geom_hline(yintercept = ate, color = "black", linewidth = 0.5) +
      ggplot2::geom_hline(yintercept = hi, linetype = "dotted") +
      ggplot2::geom_hline(yintercept = lo, linetype = "dotted")
  }

  p <- p +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = .data[["lower"]], ymax = .data[["upper"]]),
      width = 0.15, color = "#5ab0c0", linewidth = 0.8) +
    ggplot2::geom_point(color = "#ea794e", size = 2.5) +
    ggplot2::labs(x = x.label, y = "CATE") +
    ggplot2::scale_y_continuous(limits = y.limits) +
    .utopia_pdp_theme()

  .as_utopia_plot(p)
}

#' Assemble subgroup ATEs over the cross of two covariates' levels.
#' Empty and failed cells are skipped; significance => trailing "*".
#' @keywords internal
#' @noRd
.subgroup_ate_2way_df <- function(c.forest, x_var, y_var) {
  x.col <- .col_vec(c.forest$X.orig, x_var)
  y.col <- .col_vec(c.forest$X.orig, y_var)
  x.lv <- sort(unique(x.col[!is.na(x.col)]))
  y.lv <- sort(unique(y.col[!is.na(y.col)]))

  cells <- list()
  for (vx in x.lv) {
    for (vy in y.lv) {
      s <- !is.na(x.col) & !is.na(y.col) & x.col == vx & y.col == vy
      if (!any(s)) next
      one <- .subgroup_ate_one(
        c.forest, s, paste0(x_var, "=", vx, ", ", y_var, "=", vy))
      if (is.null(one)) next
      one$xv <- vx
      one$yv <- vy
      cells[[length(cells) + 1L]] <- one
    }
  }
  df <- do.call(rbind, cells)
  if (is.null(df) || nrow(df) == 0L) {
    stop("plot_pdp(subgroup): no (", x_var, ", ", y_var,
         ") cell had enough units to estimate an ATE.", call. = FALSE)
  }
  df$lab <- paste0(formatC(df$estimate, format = "f", digits = 2),
                   ifelse(df$signif, "*", ""))
  df
}

#' 2-way discrete subgroup ATE heatmap with significance asterisks.
#' @keywords internal
#' @noRd
.plot_subgroup_2way <- function(c.forest, x_var, y_var, xlab) {
  df <- .subgroup_ate_2way_df(c.forest, x_var, y_var)
  x.lv <- sort(unique(df$xv))
  y.lv <- sort(unique(df$yv))
  df$xv <- factor(as.character(df$xv), levels = as.character(x.lv))
  df$yv <- factor(as.character(df$yv), levels = as.character(y.lv))
  x.label <- if (!is.null(xlab)) xlab else x_var

  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data[["xv"]], y = .data[["yv"]])) +
    ggplot2::geom_tile(ggplot2::aes(fill = .data[["estimate"]]),
                       color = "white") +
    ggplot2::geom_text(ggplot2::aes(label = .data[["lab"]]),
                       family = "serif", size = 3.5) +
    ggplot2::scale_fill_gradient2(low = "#fd647c", mid = "#e6e6e6",
                                  high = "#3d900e", midpoint = 0) +
    ggplot2::labs(x = x.label, y = y_var, fill = "Subgroup ATE") +
    ggplot2::scale_x_discrete(expand = c(0, 0)) +
    ggplot2::scale_y_discrete(expand = c(0, 0)) +
    .utopia_pdp_theme()

  .as_utopia_plot(p)
}

#' Ray-casting point-in-polygon test (base R)
#' @param pts Two-column matrix of query points.
#' @param poly Two-column matrix of polygon vertices (closed: first == last).
#' @return Logical vector, TRUE if inside.
#' @keywords internal
#' @noRd
.point_in_polygon <- function(pts, poly) {
  n <- nrow(poly) - 1L
  px <- pts[, 1L]; py <- pts[, 2L]
  inside <- logical(length(px))
  for (i in seq_len(n)) {
    x1 <- poly[i, 1L]; y1 <- poly[i, 2L]
    x2 <- poly[i + 1L, 1L]; y2 <- poly[i + 1L, 2L]
    # Does edge cross the horizontal ray from (px, py) to the right?
    cond <- (y1 > py) != (y2 > py)
    x.int <- x1 + (py[cond] - y1) * (x2 - x1) / (y2 - y1)
    inside[cond] <- xor(inside[cond], px[cond] < x.int)
  }
  inside
}

#' @keywords internal
#' @noRd
.plot_pdp_1way <- function(c.forest, x_var, X.sub, grid_size,
                           show_ate_region, show_scatter,
                           x.limits, y.limits,
                           color.var = NULL, color.cat = NULL,
                           color.lab = NULL, xlab = NULL,
                           num.threads = NULL) {
  rng <- range(.col_vec(X.sub, x_var), na.rm = TRUE)
  x.vals <- seq(rng[1], rng[2], length.out = grid_size)
  grid <- data.frame(x.vals)
  names(grid) <- x_var

  x.label <- if (!is.null(xlab)) xlab else x_var

  # -- grouped PDP (early return) ---------------------------------------------
  if (!is.null(color.var)) {
    color.col <- .col_vec(X.sub, color.var)
    group.vals <- sort(unique(color.col))
    if (is.null(color.cat)) color.cat <- as.character(group.vals)

    pd.list <- lapply(seq_along(group.vals), function(i) {
      X.g <- X.sub[color.col == group.vals[i] & !is.na(color.col), , drop = FALSE]
      data.frame(x = x.vals,
                 y = .compute_pd(c.forest, x_var, grid, X.g, num.threads),
                 group = color.cat[i])
    })
    pd.df <- do.call(rbind, pd.list)
    pd.df$group <- factor(pd.df$group, levels = color.cat)

    # Scatter data with group labels
    orig.groups <- .col_vec(c.forest$X.orig, color.var)
    raw.df <- data.frame(
      x = .col_vec(c.forest$X.orig, x_var),
      y = as.numeric(c.forest$predictions),
      group = factor(color.cat[match(orig.groups, group.vals)],
                     levels = color.cat)
    )

    legend.label <- if (!is.null(color.lab)) color.lab else color.var

    p <- ggplot2::ggplot(raw.df,
           ggplot2::aes(x = .data[["x"]], y = .data[["y"]],
                        color = .data[["group"]]))

    if (show_scatter) {
      p <- p + ggplot2::geom_point(shape = 1, alpha = 0.4)
    } else {
      p <- p + ggplot2::geom_point(alpha = 0)
    }

    p <- p +
      ggplot2::geom_line(data = pd.df, linewidth = 1.5) +
      ggplot2::labs(x = x.label, y = "CATE", color = legend.label) +
      ggplot2::scale_x_continuous(expand = c(0, 0), limits = x.limits) +
      ggplot2::scale_y_continuous(limits = y.limits) +
      ggplot2::theme(
        text = ggplot2::element_text(size = 12, family = "serif"),
        panel.background = ggplot2::element_rect(fill = "#e6e6e6"),
        plot.title = ggplot2::element_text(hjust = 0.5),
        legend.position = "bottom",
        panel.border = ggplot2::element_blank(),
        axis.text = ggplot2::element_text(),
        axis.title = ggplot2::element_text(size = ggplot2::rel(1.2)),
        strip.text = ggplot2::element_text(hjust = 0),
        strip.background = ggplot2::element_rect(fill = NA, color = NA),
        legend.key = ggplot2::element_blank(),
        complete = TRUE
      )

    return(.wrap_marginal(p, type = "histogram",
                          fill = "#e6e6e6", color = "white"))
  }

  # -- standard 1-way PDP -----------------------------------------------------
  pd.vals <- .compute_pd(c.forest, x_var, grid, X.sub, num.threads)
  pd.df <- data.frame(x = x.vals, y = pd.vals)

  # Base plot on real X.orig so ggMarginal histograms reflect true distributions
  raw.df <- data.frame(
    x = .col_vec(c.forest$X.orig, x_var),
    y = as.numeric(c.forest$predictions)
  )

  p <- ggplot2::ggplot(raw.df, ggplot2::aes(x = .data[["x"]], y = .data[["y"]]))

  if (show_scatter) {
    p <- p + ggplot2::geom_point(shape = 1, color = "#ea794e", alpha = 0.4)
  } else {
    # Invisible points so ggMarginal can read the real distributions
    p <- p + ggplot2::geom_point(alpha = 0)
  }

  if (show_ate_region) {
    ate.result <- grf::average_treatment_effect(c.forest)
    ate   <- ate.result[["estimate"]]
    upper <- ate + 1.96 * ate.result[["std.err"]]
    lower <- ate - 1.96 * ate.result[["std.err"]]
    p <- p +
      ggplot2::geom_hline(ggplot2::aes(yintercept = ate), color = "black", linewidth = .5) +
      ggplot2::geom_hline(ggplot2::aes(yintercept = upper), linetype = "dotted") +
      ggplot2::geom_hline(ggplot2::aes(yintercept = lower), linetype = "dotted") +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper),
                           fill = "#5ab0c0", alpha = 0.2)
  }

  # PD line on top
  p <- p +
    ggplot2::geom_line(data = pd.df, color = "#5ab0c0", linewidth = 1.5) +
    ggplot2::labs(x = x.label, y = "CATE") +
    ggplot2::scale_x_continuous(expand = c(0, 0), limits = x.limits) +
    ggplot2::scale_y_continuous(limits = y.limits) +
    ggplot2::theme(
      text = ggplot2::element_text(size = 12, family = "serif"),
      panel.background = ggplot2::element_rect(fill = "#e6e6e6"),
      plot.title = ggplot2::element_text(hjust = 0.5),
      legend.position = "bottom",
      panel.border = ggplot2::element_blank(),
      axis.text = ggplot2::element_text(),
      axis.title = ggplot2::element_text(size = ggplot2::rel(1.2)),
      strip.text = ggplot2::element_text(hjust = 0),
      strip.background = ggplot2::element_rect(fill = NA, color = NA),
      legend.key = ggplot2::element_blank(),
      complete = TRUE
    )

  .wrap_marginal(p, type = "histogram", fill = "#e6e6e6", color = "white")
}

#' @keywords internal
#' @noRd
.plot_pdp_2way <- function(c.forest, x_var, y_var, X.sub, grid_size,
                           trim, x.limits, y.limits,
                           num.threads = NULL) {
  x.rng <- range(.col_vec(X.sub, x_var), na.rm = TRUE)
  y.rng <- range(.col_vec(X.sub, y_var), na.rm = TRUE)
  x.vals <- seq(x.rng[1], x.rng[2], length.out = grid_size)
  y.vals <- seq(y.rng[1], y.rng[2], length.out = grid_size)
  grid <- expand.grid(x.vals, y.vals)
  names(grid) <- c(x_var, y_var)

  # Pre-filter grid to convex hull before predict (main speedup)
  if (trim) {
    train.xy <- cbind(.col_vec(c.forest$X.orig, x_var),
                      .col_vec(c.forest$X.orig, y_var))
    train.xy <- train.xy[stats::complete.cases(train.xy), , drop = FALSE]
    hull.idx <- grDevices::chull(train.xy)
    hull.idx <- c(hull.idx, hull.idx[1L])
    hull.poly <- train.xy[hull.idx, ]
    grid.xy <- cbind(grid[[x_var]], grid[[y_var]])
    inside <- .point_in_polygon(grid.xy, hull.poly)

    n.trimmed <- sum(!inside)
    if (n.trimmed > 0L) {
      message(n.trimmed, " of ", nrow(grid),
              " grid points trimmed (outside convex hull)")
    }

    if (!any(inside)) {
      warning("All grid points fall outside the convex hull; returning all NA.")
      tile.df <- data.frame(x = grid[[x_var]], y = grid[[y_var]],
                            pd = NA_real_)
    } else {
      grid.trim <- grid[inside, , drop = FALSE]
      pd.vals <- .compute_pd(c.forest, c(x_var, y_var), grid.trim, X.sub,
                             num.threads)
      tile.df <- data.frame(x = grid.trim[[x_var]], y = grid.trim[[y_var]],
                            pd = pd.vals)
    }
  } else {
    pd.vals <- .compute_pd(c.forest, c(x_var, y_var), grid, X.sub,
                           num.threads)
    tile.df <- data.frame(x = grid[[x_var]], y = grid[[y_var]], pd = pd.vals)
  }

  # Base plot on real X.orig so ggMarginal histograms reflect true distributions
  raw.df <- data.frame(
    x = .col_vec(c.forest$X.orig, x_var),
    y = .col_vec(c.forest$X.orig, y_var)
  )

  p <- ggplot2::ggplot(raw.df, ggplot2::aes(x = .data[["x"]], y = .data[["y"]])) +
    ggplot2::geom_point(alpha = 0) +
    ggplot2::geom_tile(
      data = tile.df[!is.na(tile.df$pd), ],
      ggplot2::aes(fill = .data[["pd"]]),
      inherit.aes = TRUE
    ) +
    ggplot2::scale_fill_gradient2(
      low = "#fd647c", mid = "#e6e6e6", high = "#3d900e", midpoint = 0
    ) +
    ggplot2::labs(x = x_var, y = y_var, fill = "PD (CATE)") +
    ggplot2::scale_x_continuous(expand = c(0, 0), limits = x.limits) +
    ggplot2::scale_y_continuous(expand = c(0, 0), limits = y.limits) +
    ggplot2::theme(
      text = ggplot2::element_text(size = 12, family = "serif"),
      panel.background = ggplot2::element_rect(fill = "#e6e6e6"),
      plot.title = ggplot2::element_text(hjust = 0.5),
      legend.position = "bottom",
      panel.border = ggplot2::element_blank(),
      axis.text = ggplot2::element_text(),
      axis.title = ggplot2::element_text(size = ggplot2::rel(1.2)),
      strip.text = ggplot2::element_text(hjust = 0),
      strip.background = ggplot2::element_rect(fill = NA, color = NA),
      legend.key = ggplot2::element_blank(),
      complete = TRUE
    )

  .wrap_marginal(p, type = "histogram", fill = "#e6e6e6", color = "white")
}
