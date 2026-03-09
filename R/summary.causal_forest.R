#' Summarize a Causal Forest
#'
#' Produces a summary of heterogeneity tests, variable importance scores,
#' and the average treatment effect for a fitted causal forest.
#'
#' @param object A fitted causal forest object from the \code{grf} package.
#' @param seed Integer seed for reproducibility. Default is `1995`.
#' @param ... Additional arguments (currently unused).
#' @return An object of class `"summary.causal_forest"` (returned invisibly)
#'   with components:
#'   \describe{
#'     \item{heterogeneity}{Data frame from [omni_hetero()].}
#'     \item{vimp}{Named numeric vector of variable importance scores.}
#'     \item{ate}{Named numeric vector with `estimate` and `std.err`.}
#'   }
#' @method summary causal_forest
#' @export
summary.causal_forest <- function(object, seed = 1995, ...) {

  ate.result <- grf::average_treatment_effect(object)
  ate <- c(estimate = ate.result[["estimate"]],
           std.err = ate.result[["std.err"]])

  vi <- grf::variable_importance(object)
  vi.scores <- as.numeric(vi)
  names(vi.scores) <- colnames(object$X.orig)

  hetero <- omni_hetero(object, seed = seed)


  out <- list(
    ate = ate,
    vimp = vi.scores,
    heterogeneity = hetero
  )
  class(out) <- "summary.causal_forest"
  invisible(out)
}

#' Print Summary of a Causal Forest
#'
#' @param x An object of class `"summary.causal_forest"`.
#' @param ... Additional arguments (currently unused).
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
