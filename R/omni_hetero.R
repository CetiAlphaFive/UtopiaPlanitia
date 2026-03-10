#' Omnibus Tests of Heterogeneity
#'
#' Performs various heterogeneity tests on a fitted causal forest model.
#' Combines the calibration test of Chernozhukov et al. (2018), a naive
#' high/low CATE split (Athey and Wager, 2019), the sequential RATE test
#' (Wager, 2024), and OOB RATE heuristics into a single summary table.
#'
#' @param c.forest A fitted causal forest object from the \code{grf} package.
#' @param seed An integer seed for reproducibility. Default is `1995`.
#' @return A data frame summarizing the results of the heterogeneity tests,
#'   including the test names, estimates, p-values, and whether heterogeneity
#'   is detected at the 0.05 level.
#'
#' @details
#' The **OOB RATE two-sided test** is known to be anti-conservative (roughly
#' 30\% rejection rate under the null at nominal 5\%; see the grf RATE
#' vignette). The one-sided version is approximately valid when the direction
#' is pre-specified. The sequential RATE test has correct size.
#'
#' @references
#' Chernozhukov, V., Demirer, M., Duflo, E., and Fernandez-Val, I. (2018).
#' Generic Machine Learning Inference on Heterogeneous Treatment Effects in
#' Randomized Experiments. NBER Working Paper 24678.
#'
#' Athey, S. and Wager, S. (2019). Estimating Treatment Effects with Causal
#' Forests: An Application. *Observational Studies*, 5, 37--51.
#'
#' Wager, S. (2024). Sequential Validation of Treatment Heterogeneity.
#' arXiv:2405.05534.
#'
#' @import grf
#' @export
#' @examples
#' \donttest{
#' library(grf)
#' set.seed(1995)
#' n <- 200; p <- 5
#' X <- matrix(rnorm(n * p), n, p)
#' W <- rbinom(n, 1, 0.5)
#' Y <- X[, 1] * W + rnorm(n)
#' cf <- causal_forest(X, Y, W, num.trees = 100)
#' omni_hetero(cf)
#' }
omni_hetero <- function(c.forest, seed = 1995) {

  if (!inherits(c.forest, "causal_forest")) {
    stop("c.forest must be a grf causal forest.")
  }

  set.seed(seed)

  # pull out clusters
  cls <- if (length(c.forest$clusters) == 0) NULL else c.forest$clusters

  # extract nuisance plug-ins from original forest
  Y.hat <- c.forest$Y.hat
  W.hat <- c.forest$W.hat

  # extract tuning parameters to match original forest specification
  tp <- c.forest$tunable.params
  n.trees <- c.forest[["_num_trees"]]
  sw <- c.forest$sample.weights
  ecw <- c.forest$equalize.cluster.weights
  cigs <- c.forest$ci.group.size

  # Chernozhukov's calibration test
  calibration_test <- test_calibration(c.forest)
  cal.est  <- calibration_test["differential.forest.prediction", "Estimate"]
  cal.pval <- calibration_test["differential.forest.prediction", "Pr(>t)"]

  # Naive high/low test (Athey and Wager, 2019)
  tau.hat <- c.forest$predictions
  high.effect <- tau.hat > stats::median(tau.hat)
  ate.high <- average_treatment_effect(c.forest, subset = high.effect)
  ate.low  <- average_treatment_effect(c.forest, subset = !high.effect)
  naive_high_low_diff <- ate.high[["estimate"]] - ate.low[["estimate"]]
  naive_high_low_se   <- sqrt(ate.high[["std.err"]]^2 + ate.low[["std.err"]]^2)
  naive_high_low_p_value <- 2 * stats::pnorm(-abs(naive_high_low_diff / naive_high_low_se))

  # Wager's sequential RATE test
  rate_sequential <- function(X, Y, W, num.folds = 5) {

    set.seed(seed)
    fold.id <- sample(rep(1:num.folds, length = nrow(X)))
    samples.by.fold <- split(seq_along(fold.id), fold.id)

    t.statistics <- c()

    # Form AIPW scores for estimating RATE (full-sample, per Wager 2024)
    nuisance.forest <- causal_forest(X, Y, W,
                                     Y.hat = Y.hat, W.hat = W.hat,
                                     num.trees = n.trees,
                                     sample.weights = sw,
                                     clusters = cls,
                                     equalize.cluster.weights = ecw,
                                     sample.fraction = tp$sample.fraction,
                                     mtry = tp$mtry,
                                     min.node.size = tp$min.node.size,
                                     honesty.fraction = tp$honesty.fraction,
                                     honesty.prune.leaves = tp$honesty.prune.leaves,
                                     alpha = tp$alpha,
                                     imbalance.penalty = tp$imbalance.penalty,
                                     ci.group.size = cigs,
                                     seed = seed)
    DR.scores <- get_scores(nuisance.forest)

    for (k in 2:num.folds) {
      train <- unlist(samples.by.fold[1:(k - 1)])
      test  <- samples.by.fold[[k]]

      cate.forest <- causal_forest(X[train, ], Y[train], W[train],
                                   Y.hat = Y.hat[train], W.hat = W.hat[train],
                                   num.trees = n.trees,
                                   sample.weights = sw[train],
                                   clusters = cls[train],
                                   equalize.cluster.weights = ecw,
                                   sample.fraction = tp$sample.fraction,
                                   mtry = tp$mtry,
                                   min.node.size = tp$min.node.size,
                                   honesty.fraction = tp$honesty.fraction,
                                   honesty.prune.leaves = tp$honesty.prune.leaves,
                                   alpha = tp$alpha,
                                   imbalance.penalty = tp$imbalance.penalty,
                                   ci.group.size = cigs,
                                   seed = seed)

      cate.hat.test <- stats::predict(cate.forest, X[test, ])$predictions

      rate.fold <- rank_average_treatment_effect.fit(DR.scores[test], cate.hat.test)
      t.statistics <- c(t.statistics, rate.fold$estimate / rate.fold$std.err)
    }

    2 * stats::pnorm(-abs(sum(t.statistics) / sqrt(num.folds - 1)))
  }

  X <- c.forest$X.orig
  Y <- c.forest$Y.orig
  W <- c.forest$W.orig

  sequential_rate_test_pvalue <- rate_sequential(X, Y, W)

  # Wager's heuristic OOB RATE test
  tau.hat.oob <- c.forest$predictions
  rate.oob <- rank_average_treatment_effect(c.forest, tau.hat.oob)

  t.stat.oob <- rate.oob$estimate / rate.oob$std.err
  p.val          <- 2 * stats::pnorm(-abs(t.stat.oob))
  p.val.onesided <- stats::pnorm(t.stat.oob, lower.tail = FALSE)

  # Combine results
  summary_table <- data.frame(
    heterogeneity_test = c(
      "Calibration Test (Chernozhukov et al., 2018)",
      "High vs. Low CATE (Athey and Wager, 2019)",
      "Sequential RATE (Wager, 2024)",
      "OOB RATE, two-sided (heuristic, anti-conservative)",
      "OOB RATE, one-sided (heuristic)"
    ),
    estimate = c(
      cal.est,
      naive_high_low_diff,
      NA_real_,
      rate.oob$estimate,
      rate.oob$estimate
    ),
    p_value = c(
      cal.pval,
      naive_high_low_p_value,
      sequential_rate_test_pvalue,
      p.val,
      p.val.onesided
    ),
    hetero_detected = c(
      cal.pval <= 0.05,
      naive_high_low_p_value <= 0.05,
      sequential_rate_test_pvalue <= 0.05,
      p.val <= 0.05,
      p.val.onesided <= 0.05
    )
  )

  return(summary_table)
}
