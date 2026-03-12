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
#'   averaging. If `nrow(X.orig) > n_max`, a random subsample is drawn
#'   (default 2000).
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
#'
#' @return A `ggExtra::ggMarginal` plot object (class `"ggExtraPlot"`).
#'
#' @details
#' **Computational cost.** Each grid point requires a full `predict()` call on
#' the (possibly subsampled) covariate matrix. A 2-way PDP evaluates
#' `grid_size^2` such calls, so keep `grid_size` and `n_max` moderate for
#' large forests.
#'
#' @references
#' Friedman, J. H. (2001). Greedy Function Approximation: A Gradient Boosting
#' Machine. *Annals of Statistics*, 29(5), 1189--1232.
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
plot_pdp <- function(c.forest, x_var, y_var = NULL,
                     grid_size = 50, n_max = 2000,
                     show_ate_region = TRUE, show_scatter = TRUE,
                     trim = TRUE,
                     x.limits = NULL, y.limits = NULL,
                     color.var = NULL, color.cat = NULL,
                     color.lab = NULL, xlab = NULL) {

  if (!inherits(c.forest, "causal_forest")) {
    stop("c.forest must be a causal_forest object from the grf package.")
  }
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required but not installed.")
  }
  if (!requireNamespace("ggExtra", quietly = TRUE)) {
    stop("Package 'ggExtra' is required but not installed.")
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

  # Subsample for speed
  set.seed(1995)
  n <- nrow(X.orig)
  idx <- if (n > n_max) sample.int(n, n_max) else seq_len(n)
  X.sub <- X.orig[idx, , drop = FALSE]

  if (is.null(y_var)) {
    .plot_pdp_1way(c.forest, x_var, X.sub, grid_size,
                   show_ate_region, show_scatter, x.limits, y.limits,
                   color.var, color.cat, color.lab, xlab)
  } else {
    .plot_pdp_2way(c.forest, x_var, y_var, X.sub, grid_size,
                   trim, x.limits, y.limits)
  }
}


# -- internal helpers (not exported) ------------------------------------------

#' @keywords internal
#' @noRd
.compute_pd <- function(c.forest, vars, grid, X.sub) {
  n.sub <- nrow(X.sub)
  n.grid <- nrow(grid)
  # Stack n.grid copies of X.sub, overwriting target columns with grid values
  X.big <- X.sub[rep(seq_len(n.sub), times = n.grid), , drop = FALSE]
  for (v in vars) {
    X.big[, v] <- rep(grid[[v]], each = n.sub)
  }
  preds <- stats::predict(c.forest, newdata = X.big)$predictions
  # Average within each grid-point block
  colMeans(matrix(preds, nrow = n.sub, ncol = n.grid))
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
                           color.lab = NULL, xlab = NULL) {
  x.vals <- seq(min(X.sub[, x_var]), max(X.sub[, x_var]),
                length.out = grid_size)
  grid <- data.frame(x.vals)
  names(grid) <- x_var

  x.label <- if (!is.null(xlab)) xlab else x_var

  # -- grouped PDP (early return) ---------------------------------------------
  if (!is.null(color.var)) {
    group.vals <- sort(unique(X.sub[, color.var]))
    if (is.null(color.cat)) color.cat <- as.character(group.vals)

    pd.list <- lapply(seq_along(group.vals), function(i) {
      X.g <- X.sub[X.sub[, color.var] == group.vals[i], , drop = FALSE]
      data.frame(x = x.vals,
                 y = .compute_pd(c.forest, x_var, grid, X.g),
                 group = color.cat[i])
    })
    pd.df <- do.call(rbind, pd.list)
    pd.df$group <- factor(pd.df$group, levels = color.cat)

    # Scatter data with group labels
    orig.groups <- c.forest$X.orig[, color.var]
    raw.df <- data.frame(
      x = c.forest$X.orig[, x_var],
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

    return(ggExtra::ggMarginal(p, type = "histogram",
                               fill = "#e6e6e6", color = "white"))
  }

  # -- standard 1-way PDP -----------------------------------------------------
  pd.vals <- .compute_pd(c.forest, x_var, grid, X.sub)
  pd.df <- data.frame(x = x.vals, y = pd.vals)

  # Base plot on real X.orig so ggMarginal histograms reflect true distributions
  raw.df <- data.frame(
    x = c.forest$X.orig[, x_var],
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

  ggExtra::ggMarginal(p, type = "histogram", fill = "#e6e6e6", color = "white")
}

#' @keywords internal
#' @noRd
.plot_pdp_2way <- function(c.forest, x_var, y_var, X.sub, grid_size,
                           trim, x.limits, y.limits) {
  x.vals <- seq(min(X.sub[, x_var]), max(X.sub[, x_var]),
                length.out = grid_size)
  y.vals <- seq(min(X.sub[, y_var]), max(X.sub[, y_var]),
                length.out = grid_size)
  grid <- expand.grid(x.vals, y.vals)
  names(grid) <- c(x_var, y_var)

  pd.vals <- .compute_pd(c.forest, c(x_var, y_var), grid, X.sub)

  # Mask grid points outside the convex hull of the training data
  if (trim) {
    train.xy <- cbind(c.forest$X.orig[, x_var], c.forest$X.orig[, y_var])
    hull.idx <- grDevices::chull(train.xy)
    hull.idx <- c(hull.idx, hull.idx[1L])
    hull.poly <- train.xy[hull.idx, ]
    grid.xy <- cbind(grid[[x_var]], grid[[y_var]])
    inside <- .point_in_polygon(grid.xy, hull.poly)
    pd.vals[!inside] <- NA
  }

  tile.df <- data.frame(x = grid[[x_var]], y = grid[[y_var]], pd = pd.vals)

  # Base plot on real X.orig so ggMarginal histograms reflect true distributions
  raw.df <- data.frame(
    x = c.forest$X.orig[, x_var],
    y = c.forest$X.orig[, y_var]
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

  ggExtra::ggMarginal(p, type = "histogram", fill = "#e6e6e6", color = "white")
}
