#' Compute Variable Importance (Internal)
#'
#' Computes a single variable importance score from forest weights and
#' centered data. This is an internal helper called by [cf_loco()] for each
#' dropped-variable refit; most users should call `cf_loco()` directly.
#'
#' @param alpha Sparse weight matrix from [grf::get_forest_weights()].
#'   Each row `i` gives the weights that forest observations receive when
#'   predicting unit `i`'s treatment effect.
#' @param Y.centered Numeric vector. Outcome residuals `Y - Y.hat`, where
#'   `Y.hat` is the forest's outcome model (marginal mean of Y given X).
#' @param W.centered Numeric vector. Treatment residuals `W - W.hat`, where
#'   `W.hat` is the estimated propensity score.
#' @param tau.hat Numeric vector. Original out-of-bag CATE predictions from
#'   the full (non-dropped) causal forest.
#' @param stabilize Numeric floor for the per-observation denominator
#'   `Var_alpha(W.centered)`. Prevents division-by-zero when forest weights
#'   concentrate on units with near-identical propensity scores.
#'   Default `1e-6`. Set to `0` to disable.
#'
#' @details
#' For each observation, the function re-estimates the CATE using the
#' weight-based regression:
#'
#' \deqn{\hat{\tau}_i^{new} = \frac{Cov_\alpha(W_i, Y_i) - \hat{\tau}_i \cdot Var_\alpha(W_i)}{Var_\alpha(W_i)}}
#'
#' where \eqn{Cov_\alpha} and \eqn{Var_\alpha} are weighted covariances and
#' variances using the forest weights \eqn{\alpha}. The importance score is
#' then the normalized squared difference between the original and
#' re-estimated CATEs, divided by the variance of the original predictions.
#'
#' When `Var_alpha(W.centered)` is near zero for some observations (e.g.,
#' because the forest weights concentrate on units with similar propensity
#' scores), the `stabilize` parameter clamps the denominator to prevent
#' numerical instability.
#'
#' @return A single numeric value: the importance score for this refit.
#'   Higher values indicate greater importance.
#'
#' @references
#' Benard, C. and Josse, J. (2023). Variable Importance for Causal Forests:
#' Breaking Down the Heterogeneity of Treatment Effects.
#' \doi{10.48550/arXiv.2308.03369}
#'
#' @seealso [cf_loco()] which calls this function internally.
#'
#' @keywords internal
#' @noRd
compute_vimp <- function(alpha, Y.centered, W.centered, tau.hat,
                         stabilize = 1e-6) {
  W.bar <- alpha %*% W.centered
  W2.bar <- alpha %*% (W.centered^2)
  Y.bar <- alpha %*% Y.centered
  tau.bar <- alpha %*% tau.hat
  denom <- as.numeric(W2.bar - W.bar^2)
  n.clamped <- sum(abs(denom) < stabilize)
  if (n.clamped > 0L) {
    warning(n.clamped, " of ", length(denom),
            " observations had near-zero local treatment variation ",
            "(Var_alpha(W.c) < ", stabilize, ") and were stabilized.",
            call. = FALSE)
  }
  denom <- ifelse(abs(denom) < stabilize, sign(denom) * stabilize, denom)
  numer <- as.numeric(
    alpha %*% (W.centered * Y.centered - tau.hat * (W.centered^2)) -
      (W.bar * Y.bar - tau.bar * W2.bar)
  )
  tau.hat.new <- numer / denom
  vimp <- as.numeric(sum((tau.hat - tau.hat.new)^2) /
                       (length(Y.centered) * stats::var(tau.hat)))
  return(vimp)
}
