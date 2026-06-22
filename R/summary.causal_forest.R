#' Summarize a Causal Forest
#'
#' Produces a one-stop summary of a fitted causal forest: the average
#' treatment effect (ATE), split-frequency variable importance, and a
#' battery of heterogeneity tests.
#'
#' @param object A fitted causal forest object from the \code{grf} package.
#' @param seed Integer seed for reproducibility. Default is `1995`.
#'   Passed to [omni_hetero()] for the sequential RATE fold assignment.
#' @param ... Additional arguments (currently unused).
#' @return An object of class `"summary.causal_forest"` (returned invisibly)
#'   with components:
#'   \describe{
#'     \item{ate}{Named numeric vector with `estimate` and `std.err` from
#'       [grf::average_treatment_effect()].}
#'     \item{vimp}{Named numeric vector of split-frequency variable
#'       importance scores from [grf::variable_importance()]. These reflect
#'       how often each variable is used in tree splits — a fast heuristic,
#'       not the debiased LOCO scores from [cf_loco()].}
#'     \item{heterogeneity}{Data frame from [omni_hetero()] with five
#'       heterogeneity tests.}
#'   }
#'
#' @details
#' This method assembles three diagnostics:
#'
#' 1. **ATE**: The doubly robust average treatment effect estimate from grf.
#'
#' 2. **Variable importance**: grf's native split-frequency scores, which
#'    measure how often each covariate is selected for splitting. These are
#'    fast to compute but do not have a formal statistical interpretation.
#'    For debiased LOCO importance with proper methodology, use [cf_loco()].
#'
#' 3. **Heterogeneity tests**: The full [omni_hetero()] battery, including
#'    the calibration test, high/low CATE comparison, sequential RATE, and
#'    OOB RATE heuristics. See [omni_hetero()] for details on each test.
#'
#' @references
#' Athey, S., Tibshirani, J., and Wager, S. (2019). Generalized Random
#' Forests. *Annals of Statistics*, 47(2), 1148--1178.
#' \doi{10.1214/18-AOS1709}
#'
#' @seealso [print.summary.causal_forest()] for formatted output,
#'   [omni_hetero()] for the heterogeneity tests, [cf_loco()] for debiased
#'   variable importance, [plot.causal_forest()] for visual diagnostics.
#'
#' @method summary causal_forest
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
#' summary(cf)
#' }
summary.causal_forest <- function(object, seed = 1995, ...) {

  ate.result <- grf::average_treatment_effect(object)
  ate <- c(estimate = ate.result[["estimate"]],
           std.err = ate.result[["std.err"]])

  vi <- grf::variable_importance(object)
  vi.scores <- as.numeric(vi)
  names(vi.scores) <- colnames(object$X.orig)

  hetero <- omni_hetero(object, seed = seed, ...)


  out <- list(
    ate = ate,
    vimp = vi.scores,
    heterogeneity = hetero
  )
  class(out) <- "summary.causal_forest"
  out
}

#' Print Summary of a Causal Forest
#'
#' Formats and prints the output of [summary.causal_forest()], showing the
#' ATE, variable importance scores (sorted descending), and the
#' heterogeneity test table.
#'
#' @param x An object of class `"summary.causal_forest"`.
#' @param ... Additional arguments (currently unused).
#' @return The input object `x` (invisibly).
#'
#' @seealso [summary.causal_forest()]
#'
#' @method print summary.causal_forest
#' @export
print.summary.causal_forest <- function(x, ...) {

  cat("Average Treatment Effect\n")
  cat("  Estimate:", round(x$ate[["estimate"]], 4),
      " SE:", round(x$ate[["std.err"]], 4), "\n\n")

  cat("Variable Importance (grf native)\n")
  vi.sorted <- sort(x$vimp, decreasing = TRUE)
  vi.df <- data.frame(variable = names(vi.sorted),
                      importance = round(vi.sorted, 4))
  rownames(vi.df) <- NULL
  print(vi.df, row.names = FALSE)
  cat("\n")

  cat("Heterogeneity Tests\n")
  print(x$heterogeneity, row.names = FALSE)

  invisible(x)
}
