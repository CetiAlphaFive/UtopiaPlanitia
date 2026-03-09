#' Plot a Causal Forest
#'
#' Dispatches to one of the package's plot functions based on `type`.
#'
#' @param x A fitted causal forest object from the \code{grf} package.
#' @param type Character string specifying the plot type. One of
#'   `"diag"`, `"pdp"`, `"rank"`, or `"inter"`.
#' @param ... Additional arguments passed to the underlying plot function
#'   (e.g., `x_var` for `"pdp"` and `"inter"`, `y_var` for `"inter"`).
#' @return A plot object (the return value of the dispatched function).
#' @method plot causal_forest
#' @export
#' @examples
#' \donttest{
#' library(grf)
#' set.seed(1995)
#' n <- 200; p <- 5
#' X <- matrix(rnorm(n * p), n, p)
#' colnames(X) <- paste0("X", seq_len(p))
#' W <- rbinom(n, 1, 0.5)
#' Y <- X[, 1] * W + rnorm(n)
#' cf <- causal_forest(X, Y, W, num.trees = 100)
#' plot(cf, type = "rank")
#' }
plot.causal_forest <- function(x, type = c("diag", "pdp", "rank", "inter"), ...) {
  type <- match.arg(type)
  switch(type,
    diag  = plot_diag(x, ...),
    pdp   = plot_pdp(x, ...),
    rank  = rank_plot(x, ...),
    inter = plot_inter(x, ...)
  )
}
