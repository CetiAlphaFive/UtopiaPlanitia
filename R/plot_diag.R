#' Model Diagnostics for Causal Forest
#'
#' Generates a multi-panel diagnostics figure for a causal forest model,
#' covering outcome model fit, treatment balance, and CATE distribution.
#'
#' @param c.forest A fitted causal forest object from the \code{grf} package.
#' @return A list containing the following elements:
#' \item{out_check}{A ggplot2 object showing the density plot comparing real Y and predicted Y.}
#' \item{bal_perm_plot}{A ggplot2 object showing the treatment propensity plot.}
#' \item{cate_distro_plot}{A ggplot2 object showing the density plot of the distribution of CATEs.}
#' \item{rmse_out}{The Root Mean Squared Error (RMSE) for the outcome model
#'   predictions. Also printed to the console.}
#'
#' @details
#' The figure contains three panels:
#'
#' 1. **Outcome check** (top-left): Overlays the density of the observed
#'    outcome `Y` against the forest's outcome model predictions
#'    `Y.hat`. Good overlap indicates a well-specified outcome model.
#'    The RMSE is printed to the console.
#'
#' 2. **Balance permutation test** (top-right): Uses
#'    [MLbalance::random_check()] to test whether the treatment assignment
#'    `W` is predictable from the covariates `X`. A non-significant result
#'    (large p-value) is consistent with random or as-if-random assignment.
#'    See Gagnon-Bartsch and Shem-Tov (2019).
#'
#' 3. **CATE distribution** (bottom): Histogram with interval summary of
#'    the out-of-bag CATE estimates. A tight distribution centered at zero
#'    suggests little heterogeneity; a spread distribution suggests
#'    meaningful variation in treatment effects across units.
#'
#' @references
#' Gagnon-Bartsch, J. and Shem-Tov, Y. (2019). The Classification
#' Permutation Test: A Flexible Approach to Testing for Covariate
#' Imbalance in Observational and Experimental Studies.
#' \doi{10.48550/arXiv.1903.07841}
#'
#' @seealso [summary.causal_forest()] for a text-based summary,
#'   [plot.causal_forest()] to call this via `plot(cf, type = "diag")`.
#'
#' @importFrom rlang .data
#' @export
#' @examplesIf rlang::is_installed(c("ggplot2", "ggdist", "gridExtra", "MLbalance"))
#' library(grf)
#' set.seed(1995)
#' n <- 200; p <- 5
#' X <- matrix(rnorm(n * p), n, p)
#' W <- rbinom(n, 1, 0.5)
#' Y <- X[, 1] * W + rnorm(n)
#' cf <- causal_forest(X, Y, W, num.trees = 100)
#' plot_diag(cf)
plot_diag <- function(c.forest) {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required but not installed.")
  }
  if (!requireNamespace("gridExtra", quietly = TRUE)) {
    stop("Package 'gridExtra' is required but not installed.")
  }
  if (!requireNamespace("ggdist", quietly = TRUE)) {
    stop("Package 'ggdist' is required but not installed.")
  }
  has_mlbalance <- requireNamespace("MLbalance", quietly = TRUE)
  if (!has_mlbalance) {
    message("plot_diag(): 'MLbalance' not installed; skipping balance ",
            "permutation panel. Install from ",
            "https://github.com/CetiAlphaFive/MLbalance to enable it.")
  }

  # mlbalance theme to match
  g_theme <- function() {
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 14, face = "bold", hjust = 0.5),
      panel.background = ggplot2::element_rect(fill = "white", colour = "white",
                                                linewidth = 0.5, linetype = "solid"),
      axis.line = ggplot2::element_line(linewidth = .5, color = "black"),
      axis.title.x = ggplot2::element_text(size = 12, face = "bold"),
      axis.title.y = ggplot2::element_text(size = 12, face = "bold"),
      axis.text.x = ggplot2::element_text(color = "grey30", size = 10),
      axis.text.y = ggplot2::element_text(color = "grey30", size = 10),
      panel.grid.major.x = ggplot2::element_line(colour = "grey80"),
      plot.caption = ggplot2::element_text(hjust = 0),
      text = ggplot2::element_text(size = 12, family = "serif"),
      legend.position.inside = c(.1, .75),
      axis.ticks = ggplot2::element_blank(),
      legend.background = ggplot2::element_blank(),
      panel.spacing = grid::unit(2, "lines")
    )
  }

  # Outcome check: compare real Y and Y.hat
  plot.df1 <- data.frame(Y = c(c.forest$Y.orig, c.forest$Y.hat),
                         Type = c(rep("Y", length(c.forest$Y.orig)),
                                  rep("Y_hat", length(c.forest$Y.hat))))

  p1 <- plot.df1 |>
    ggplot2::ggplot(ggplot2::aes(x = .data[["Y"]], fill = .data[["Type"]])) +
    ggplot2::geom_density(alpha = 0.5) +
    ggplot2::labs(title = expression("Outcome Check: Y vs." ~ hat(Y)),
                  y = "Density", x = "Y") +
    g_theme() +
    ggplot2::scale_fill_manual(values = c("dodgerblue1", "darkorange1"))

  # Calculate RMSE
  rmse <- function(y, y_hat) sqrt(mean((y - y_hat)^2))
  rmse_out <- rmse(c.forest$Y.orig, c.forest$Y.hat)

  cat("RMSE for outcome model:", rmse_out, "\n")

  # Distribution of CATEs
  plot.df2 <- data.frame(cates = c.forest$predictions,
                         Type = "Regression Forest (Honest)")

  p2 <- plot.df2 |>
    ggplot2::ggplot(ggplot2::aes(x = .data[["cates"]], fill = .data[["Type"]])) +
    ggdist::stat_histinterval(alpha = 0.9, fill = "darkorange1",
                              outline_bars = TRUE, slab_color = "black") +
    ggplot2::labs(title = expression(hat(CATE) ~ "Distribution"),
                  y = "Density",
                  x = expression(hat(CATE) ~ "Estimates")) +
    g_theme()

  # Treatment propensity check using MLbalance (optional)
  p3 <- if (has_mlbalance) {
    MLbalance::random_check(W_real = c.forest$W.orig,
                            X = c.forest$X.orig)$plot +
      ggplot2::ggtitle("Balance Permutation Test")
  } else {
    NULL
  }

  # Arrange plots in a multi-panel figure
  panels <- if (is.null(p3)) list(p1, p2) else list(p1, p3, p2)
  gridExtra::grid.arrange(grobs = panels, ncol = 2)

  list(
    outcome_check_plot = p1,
    treatment_propensity_plot = p3,
    cates_distribution_plot = p2,
    rmse_out = rmse_out
  )
}
