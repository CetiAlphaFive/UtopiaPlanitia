#' Plot a Causal Forest
#'
#' Dispatches to one of the package's plot functions based on `type`.
#'
#' @param x A fitted causal forest object from the \code{grf} package.
#' @param type Character string specifying the plot type. One of:
#'   - `"diag"`: multi-panel diagnostics ([plot_diag()])
#'   - `"pdp"`: partial dependence plot ([plot_pdp()])
#'   - `"scatter"`: individual CATE scatter ([plot_scatter()])
#'   - `"rank"`: ranked CATEs with CIs ([plot_rank()])
#' @param ... Additional arguments passed to the underlying plot function
#'   (e.g., `x_var` for `"pdp"` and `"scatter"`).
#' @return A plot object (the return value of the dispatched function).
#'
#' @details
#' This is a convenience dispatcher. Each type targets a different diagnostic
#' question:
#' \describe{
#'   \item{`"diag"`}{Are the nuisance models (outcome, propensity) well
#'     calibrated? Is there treatment effect heterogeneity?}
#'   \item{`"pdp"`}{How does the CATE change as a function of one or two
#'     covariates, marginalizing over the rest?}
#'   \item{`"scatter"`}{What do the individual OOB CATE predictions look
#'     like against a single covariate?}
#'   \item{`"rank"`}{What is the distribution of individual CATEs, sorted
#'     by magnitude, with confidence intervals?}
#' }
#'
#' @seealso [plot_diag()], [plot_pdp()], [plot_scatter()], [plot_rank()]
#'
#' @method plot causal_forest
#' @export
#' @examplesIf rlang::is_installed(c("ggplot2", "ggdist"))
#' library(grf)
#' set.seed(1995)
#' n <- 200; p <- 5
#' X <- matrix(rnorm(n * p), n, p)
#' colnames(X) <- paste0("X", seq_len(p))
#' W <- rbinom(n, 1, 0.5)
#' Y <- X[, 1] * W + rnorm(n)
#' cf <- causal_forest(X, Y, W, num.trees = 100)
#' plot(cf, type = "rank")
plot.causal_forest <- function(x, type = c("diag", "pdp", "scatter", "rank"), ...) {
  type <- match.arg(type)
  out <- switch(type,
    diag    = plot_diag(x, ...),
    pdp     = plot_pdp(x, ...),
    scatter = plot_scatter(x, ...),
    rank    = plot_rank(x, ...)
  )
  invisible(out)
}
