#' Compute Variable Importance
#'
#' Computes variable importance from coefficients alpha of the retrained forest, centered outcome and treatment assignment, and original oob predictions.
#'
#' @param alpha Coefficients alpha of the retrained forest.
#' @param Y.centered Centered outcome.
#' @param W.centered Centered treatment assignment.
#' @param tau.hat Original out-of-bag predictions.
#' @return A numeric value representing variable importance.
#' @export
compute_vimp <- function(alpha, Y.centered, W.centered, tau.hat){
  W.bar <- alpha %*% W.centered
  W2.bar <- alpha %*% (W.centered^2)
  Y.bar <- alpha %*% Y.centered
  tau.bar <- alpha %*% tau.hat
  tau.hat.new <- (alpha %*% (W.centered * Y.centered - tau.hat * (W.centered^2)) - (W.bar * Y.bar - tau.bar * W2.bar)) / (W2.bar - W.bar^2)
  vimp <- as.numeric(sum((tau.hat - tau.hat.new)^2) / (length(Y.centered) * var(tau.hat)))
  return(vimp)
}
