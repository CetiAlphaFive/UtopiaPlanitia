#' Omnibus Tests of Heterogeneity
#'
#' Performs various heterogeneity tests on a fitted causal forest model.
#'
#' @param c.forest A fitted causal forest object from the \code{grf} package.
#' @return A data frame summarizing the results of the heterogeneity tests, including the test names, estimates, p-values, and whether heterogeneity is detected.
#' @import grf
#' @export
#' @examples
#' \dontrun{
#' cf_model <- causal_forest(X, Y, W)
#' summary_results <- omni_hetero(cf_model)
#' print(summary_results)
#' }
omni_hetero <- function(c.forest) {
  # Chernozhukov's omnibus test
  calibration_test <- test_calibration(c.forest)

  # Naive high/low test
  tau.hat <- c.forest$predictions
  high.effect <- tau.hat > stats::median(tau.hat)
  ate.high <- average_treatment_effect(c.forest, subset = high.effect)
  ate.low <- average_treatment_effect(c.forest, subset = !high.effect)
  naive_high_low_diff <- ate.high[["estimate"]] - ate.low[["estimate"]]
  naive_high_low_se <- sqrt(ate.high[["std.err"]]^2 + ate.low[["std.err"]]^2)
  naive_high_low_ci <- naive_high_low_diff + c(-1, 1) * stats::qnorm(0.975) * naive_high_low_se
  naive_high_low_p_value <- 2 * stats::pnorm(-abs(naive_high_low_diff / naive_high_low_se))

  # Wager's sequential RATE test
  rate_sequential <- function(X, Y, W, num.folds = 5) {
    fold.id <- sample(rep(1:num.folds, length = nrow(X)))
    samples.by.fold <- split(seq_along(fold.id), fold.id)

    t.statistics <- c()

    # Form AIPW scores for estimating RATE
    nuisance.forest <- causal_forest(X, Y, W)
    DR.scores <- get_scores(nuisance.forest)

    for (k in 2:num.folds) {
      train <- unlist(samples.by.fold[1:(k - 1)])
      test <- samples.by.fold[[k]]

      cate.forest <- causal_forest(X[train, ], Y[train], W[train])

      cate.hat.test <- stats::predict(cate.forest, X[test, ])$predictions

      rate.fold <- rank_average_treatment_effect.fit(DR.scores[test], cate.hat.test)
      t.statistics <- c(t.statistics, rate.fold$estimate / rate.fold$std.err)
    }

    p.value <- 2 * stats::pnorm(-abs(sum(t.statistics) / sqrt(num.folds - 1)))

    p.value
  }

  X <- c.forest$X.orig
  Y <- c.forest$Y.orig
  W <- c.forest$W.orig

  sequential_rate_test_pvalue <- rate_sequential(X, Y, W)

  # Wager's heuristic test
  tau.hat.oob <- c.forest$predictions
  rate.oob <- rank_average_treatment_effect(c.forest, tau.hat.oob)

  t.stat.oob <- rate.oob$estimate / rate.oob$std.err
  # Compute a two-sided p-value Pr(>|t|)
  p.val <- 2 * stats::pnorm(-abs(t.stat.oob))
  # Compute a one-sided p-value Pr(>t)
  p.val.onesided <- stats::pnorm(t.stat.oob, lower.tail = FALSE)

  heuristic_test <- list(
    reject_two_sided = p.val <= 0.05,
    reject_one_sided = p.val.onesided <= 0.05
  )

  # Combine results into a summary table
  summary_table <- data.frame(
    heterogeneity_test = c(
      "Best Linear Fit Test (Chernozhukov et. al, 2024)",
      "High vs. Low Test (Athey et. al, 2017)",
      "Sequential RATE Test (Wager, 2024)",
      "RATE OOB Test (Two-Sided, Wager, 2024)",
      "RATE OOB Test (One-Sided, Wager, 2024)"
    ),
    estimate = c(
      calibration_test[2],
      naive_high_low_diff,
      "Not Applicable",
      rate.oob$estimate,
      rate.oob$estimate
    ),
    p.val = c(
      calibration_test[8],
      naive_high_low_p_value,
      sequential_rate_test_pvalue,
      p.val,
      p.val.onesided
    ),
    hetero_detect = c(
      calibration_test[8] <= 0.05,
      naive_high_low_p_value <= .05,
      sequential_rate_test_pvalue <= 0.05,
      heuristic_test$reject_two_sided,
      heuristic_test$reject_one_sided
    )
  )

  return(summary_table)
}
