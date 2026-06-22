#' Omnibus Tests of Heterogeneity
#'
#' Performs various heterogeneity tests on a fitted causal forest model.
#' Combines the calibration test of Chernozhukov et al. (2018), a cross-fit
#' high/low CATE split (Athey and Wager, 2019; cf. grf PR #1502), the
#' sequential RATE test (Wager, 2024), and OOB RATE heuristics into a single
#' summary table.
#'
#' @param c.forest A fitted causal forest object from the \code{grf} package.
#' @param seed An integer seed for reproducibility. Default is `1995`.
#'   Controls the fold assignment in the sequential RATE test and the
#'   half-sample split in the cross-fit High vs. Low test.
#' @param min_fold_n Minimum per-fold training sample size below which the
#'   Sequential RATE test is considered unstable. When `n < 400` or
#'   `n / num.folds < min_fold_n`, a warning is emitted at the start of the
#'   Sequential RATE computation and users are advised to prefer the
#'   Calibration test (Chernozhukov et al., 2018) or OOB RATE heuristics
#'   for small samples. Within the fold loop, any fold whose test-set CATE
#'   predictions are (near-)constant, whose RATE standard error is zero,
#'   or whose t-statistic is `NaN` is dropped from aggregation rather than
#'   allowed to propagate a silent `NaN` into the final p-value. If ANY
#'   fold is dropped (i.e. fewer than `num.folds - 1` t-statistics
#'   accumulate), the Sequential RATE p-value is returned as `NA_real_`
#'   with an explanatory `reason` attribute, because the
#'   `sqrt(num.folds - 1)` aggregation denominator would otherwise
#'   under-normalize and deflate the t-statistic. Default is `100`.
#' @param num.folds Integer number of folds K for the sequential RATE
#'   cross-validation (Wager, 2024). Must be a single integer `>= 3`
#'   (`K = 2` cannot yield the two usable folds the test requires) and no
#'   greater than the number of observations. Default is `5`; `5` or more
#'   is recommended. Larger K gives each per-fold CATE forest more training
#'   data, but more folds at small n raises the risk of degenerate folds
#'   (see `min_fold_n`).
#' @return An object of class `"omni_hetero"` (a data frame) with one row
#'   per test and the following columns:
#'   \describe{
#'     \item{category}{Character. `"Preferred"` for tests with valid Type I
#'       error control, `"Heuristic"` for screening tests.}
#'     \item{heterogeneity_test}{Character. Name and citation of the test.}
#'     \item{estimate}{Numeric. The test statistic or effect estimate.
#'       `NA` for the sequential RATE test (which only produces a p-value).}
#'     \item{p_value}{Numeric. Two-sided p-value (or one-sided for the final
#'       row). Small values indicate evidence of treatment effect
#'       heterogeneity.}
#'     \item{hetero_detected}{Logical. `TRUE` if `p_value <= 0.05`, `NA`
#'       when `p_value` is `NA`.}
#'   }
#'
#' @details
#' The function combines five tests of treatment effect heterogeneity,
#' grouped into **Preferred** (valid size) and **Heuristic** categories:
#'
#' **Preferred tests** (valid Type I error control):
#'
#' 1. **Sequential RATE** (Wager, 2024): A k-fold cross-validated test with
#'    correct size (valid Type I error). This is the most trustworthy test
#'    in the battery and should be preferred for formal inference.
#'
#' 2. **Calibration test** (Chernozhukov et al., 2018): Regresses doubly
#'    robust scores on the forest's CATE predictions. The "differential
#'    forest prediction" coefficient tests whether the forest captures
#'    meaningful heterogeneity beyond the ATE.
#'
#' **Heuristic tests** (useful for screening, not formal inference):
#'
#' 3. **High vs. low CATE, cross-fit** (Athey and Wager, 2019): Splits the
#'    sample in half, predicts CATEs on each half using a forest trained
#'    on the other half, then median-splits and compares \eqn{ATE_{high}} vs.
#'    \eqn{ATE_{low}} within each held-out half. Cross-fitting avoids the
#'    winner's curse that contaminates the naive same-data version (see
#'    grf PR #1502).
#'
#' 4. **OOB RATE, two-sided** (heuristic): Uses out-of-bag CATE predictions
#'    directly. Known to be anti-conservative (~30\% rejection rate under the
#'    null at nominal 5\%; see the grf RATE vignette). Useful as a quick
#'    screening tool, not for formal inference.
#'
#' 5. **OOB RATE, one-sided** (heuristic): One-sided version of the above.
#'    Approximately valid when the direction of heterogeneity is pre-specified.
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
#' \doi{10.48550/arXiv.2405.05534}
#'
#' @seealso [summary.causal_forest()] which calls this function as part of
#'   its output, [cf_loco()] for variable-level importance rather than an
#'   omnibus heterogeneity test.
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
omni_hetero <- function(c.forest, seed = 1995, min_fold_n = 100,
                        num.folds = 5) {

  if (!inherits(c.forest, "causal_forest")) {
    stop("c.forest must be a grf causal forest.")
  }

  # Validate num.folds: a single integer K >= 3 (5 or more recommended).
  # K = 2 always yields < 2 usable folds (the loop runs k in 2:K), so the
  # Sequential RATE p-value would always be NA -- reject it outright.
  if (!is.numeric(num.folds) || length(num.folds) != 1L ||
        !is.finite(num.folds) || num.folds != round(num.folds) ||
        num.folds < 3 || num.folds > .Machine$integer.max) {
    stop("num.folds must be a single integer >= 3 (5 or more recommended).",
         call. = FALSE)
  }
  num.folds <- as.integer(num.folds)
  # More folds than observations leaves high fold-ids unassigned in
  # sample(rep(1:num.folds, length = n)), so samples.by.fold[[k]] is missing
  # and the fold loop errors. Guard against it.
  if (num.folds > nrow(c.forest$X.orig)) {
    stop("num.folds (", num.folds, ") cannot exceed the number of ",
         "observations (", nrow(c.forest$X.orig), ").", call. = FALSE)
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
  cal.se   <- calibration_test["differential.forest.prediction", "Std. Error"]
  cal.mean.est <- calibration_test["mean.forest.prediction", "Estimate"]

  # BLP plot payload: cache the inputs so plot.omni_hetero() can reproduce
  # the Chernozhukov (2018) partial-residual regression without touching the
  # forest a second time. All quantities below mirror the regressors grf
  # constructs inside test_calibration():
  #   response = AIPW doubly-robust scores
  #   x1       = mean forest prediction (a constant equal to mean OOB CATE)
  #   x2       = differential forest prediction (OOB CATE - mean OOB CATE)
  blp_dr      <- grf::get_scores(c.forest)
  blp_tau.oob <- c.forest$predictions
  blp_mean    <- mean(blp_tau.oob)
  blp_diff    <- blp_tau.oob - blp_mean
  blp_payload <- list(
    dr        = as.numeric(blp_dr),
    tau_oob   = as.numeric(blp_tau.oob),
    centered  = as.numeric(blp_diff),
    mean_pred = blp_mean,
    beta_mean = cal.mean.est,
    beta_diff = cal.est,
    se_diff   = cal.se,
    p_diff    = cal.pval
  )

  # ------------------------------------------------------------------
  # H2 (audit-20260511): Cross-fit High vs. Low CATE test
  #
  # The naive median-split (which uses the same data to predict CATEs,
  # split at the median, AND estimate ATEs on each half) is severely
  # anti-conservative: Monte Carlo under the null DGP at n=200 measured a
  # rejection rate of 0.45 [0.26, 0.66] at nominal alpha = 0.05 -- a 9x
  # inflation due to winner's curse. See grf PR #1502 for the canonical
  # honest construction.
  #
  # Replacement: single-direction sample-split cross-fit (matches the
  # audit-20260511 reference implementation that achieved nominal 0.05
  # Type I).
  #   1. Random half-sample split: idx_A vs idx_B.
  #   2. Fit cf_A on idx_A; predict tau on idx_B units. This separates
  #      "splitting at the median" from "fitting the forest".
  #   3. Fit cf_B on idx_B independently; use cf_B to estimate the ATEs
  #      on the high / low subsets of idx_B (selected by tau predictions
  #      from cf_A). The forest doing the ATE is independent of the
  #      forest that decided the split -- no winner's curse.
  #   4. Joint SE: sqrt(se_high^2 + se_low^2).
  #
  # Variable names retain the "naive_high_low_*" prefix purely to avoid
  # rippling renames into downstream code (summary_table assembly below).
  # The test is now cross-fit; the name is historical.
  # ------------------------------------------------------------------
  X <- c.forest$X.orig
  Y <- c.forest$Y.orig
  W <- c.forest$W.orig
  n <- nrow(X)

  idx_A <- sample(seq_len(n), floor(n / 2))
  idx_B <- setdiff(seq_len(n), idx_A)

  cf_A <- grf::causal_forest(X[idx_A, , drop = FALSE], Y[idx_A], W[idx_A],
                             num.trees = n.trees,
                             sample.weights = sw[idx_A],
                             clusters = cls[idx_A],
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
  cf_B <- grf::causal_forest(X[idx_B, , drop = FALSE], Y[idx_B], W[idx_B],
                             num.trees = n.trees,
                             sample.weights = sw[idx_B],
                             clusters = cls[idx_B],
                             equalize.cluster.weights = ecw,
                             sample.fraction = tp$sample.fraction,
                             mtry = tp$mtry,
                             min.node.size = tp$min.node.size,
                             honesty.fraction = tp$honesty.fraction,
                             honesty.prune.leaves = tp$honesty.prune.leaves,
                             alpha = tp$alpha,
                             imbalance.penalty = tp$imbalance.penalty,
                             ci.group.size = cigs,
                             seed = seed + 1L)

  # Predict tau on idx_B using cf_A (forest that has NOT seen idx_B);
  # median-split the predictions; ATE estimation on idx_B is then done
  # by cf_B, which has NOT been used to choose the split.
  tau_B      <- stats::predict(cf_A, X[idx_B, , drop = FALSE])$predictions
  med_B      <- stats::median(tau_B)
  high_B_pos <- which(tau_B >  med_B)
  low_B_pos  <- which(tau_B <= med_B)

  # Guard against degenerate half-samples (too few units on one side).
  if (length(high_B_pos) < 2L || length(low_B_pos) < 2L) {
    warning("High/Low cross-fit: half-sample has <2 units on at least ",
            "one side of the median; returning NA.",
            call. = FALSE)
    naive_high_low_diff    <- NA_real_
    naive_high_low_se      <- NA_real_
    naive_high_low_p_value <- NA_real_
  } else {
    ate_high <- grf::average_treatment_effect(cf_B, subset = high_B_pos)
    ate_low  <- grf::average_treatment_effect(cf_B, subset = low_B_pos)

    naive_high_low_diff    <- ate_high[["estimate"]] - ate_low[["estimate"]]
    naive_high_low_se      <- sqrt(ate_high[["std.err"]]^2 +
                                   ate_low[["std.err"]]^2)
    naive_high_low_p_value <- 2 * stats::pnorm(-abs(naive_high_low_diff /
                                                    naive_high_low_se))
  }

  # Wager's sequential RATE test
  #
  # Robustness additions (v0.3.2):
  #   - Upfront size check: warn if n < 400 or n/num.folds < min_fold_n.
  #   - Per-fold degeneracy guard: drop folds where the test-set CATE forest
  #     predicts (near-)constant, where the fold RATE std.err is zero, or
  #     where the resulting t-statistic is NaN / non-finite. These otherwise
  #     propagate silently into the aggregated p-value.
  #   - If ANY fold is dropped (audit-20260511 H3), return NA_real_ with a
  #     reason rather than aggregating a deflated t-statistic against the
  #     sqrt(num.folds - 1) denominator.
  #   - The sum(t)/sqrt(num.folds - 1) aggregation is PRESERVED (any change
  #     to that formula is a separate, higher-tier concern about fold
  #     independence -- see omni_hetero audit).
  rate_sequential <- function(X, Y, W, num.folds = 5) {

    set.seed(seed)
    fold.id <- sample(rep(1:num.folds, length = nrow(X)))
    samples.by.fold <- split(seq_along(fold.id), fold.id)

    t.statistics  <- c()
    dropped.folds <- integer(0)
    drop.reasons  <- character(0)
    # per-fold accumulator for plot.omni_hetero(); one row per attempted
    # fold (including dropped ones, flagged via `dropped`).
    folds.list <- vector("list", length = num.folds - 1L)

    # Form AIPW scores for estimating RATE (full-sample, per Wager 2024).
    # The full-sample nuisance forest legitimately uses the full-sample
    # plug-in Y.hat / W.hat: it only computes DR scores, which are not
    # used to fit the per-fold CATE forests below.
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

    # small-sample tolerance for near-constant CATE predictions
    cate.tol <- 1e-8

    for (k in 2:num.folds) {
      train <- unlist(samples.by.fold[1:(k - 1)])
      test  <- samples.by.fold[[k]]

      # C1 (audit-20260511): Wager (2024) Sec. 2 martingale requirement.
      # The per-fold CATE forest must fit nuisance internally on the
      # training subsample. Do NOT pass Y.hat[train] / W.hat[train]
      # here -- those are slices of a full-sample fit that uses the
      # test fold for plug-in estimation, breaking the sequential
      # martingale property. The reference grf vignette
      # (`rate_cv.Rmd` on master) fits cleanly per fold.
      cate.forest <- causal_forest(X[train, ], Y[train], W[train],
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
      n.test <- length(test)

      # guard 1: degenerate training fit -- CATE forest predicts (near-)constant
      if (!is.finite(stats::sd(cate.hat.test)) ||
          stats::sd(cate.hat.test) < cate.tol) {
        dropped.folds <- c(dropped.folds, k)
        drop.reasons  <- c(drop.reasons,
                           "near-constant CATE predictions on test fold")
        folds.list[[k - 1L]] <- data.frame(
          fold = k, n_test = n.test, estimate = NA_real_,
          std.err = NA_real_, t_stat = NA_real_,
          dropped = TRUE,
          drop_reason = "near-constant CATE predictions on test fold"
        )
        next
      }

      rate.fold <- rank_average_treatment_effect.fit(DR.scores[test], cate.hat.test)

      # guard 2: RATE std.err is zero / non-finite (t = 0/0 = NaN)
      if (!is.finite(rate.fold$std.err) || rate.fold$std.err == 0) {
        dropped.folds <- c(dropped.folds, k)
        drop.reasons  <- c(drop.reasons,
                           "zero or non-finite RATE std.err")
        folds.list[[k - 1L]] <- data.frame(
          fold = k, n_test = n.test, estimate = rate.fold$estimate,
          std.err = rate.fold$std.err, t_stat = NA_real_,
          dropped = TRUE,
          drop_reason = "zero or non-finite RATE std.err"
        )
        next
      }

      t.stat <- rate.fold$estimate / rate.fold$std.err

      # guard 3: defensive -- any remaining non-finite t (e.g. Inf / NaN)
      if (!is.finite(t.stat)) {
        dropped.folds <- c(dropped.folds, k)
        drop.reasons  <- c(drop.reasons, "non-finite t-statistic")
        folds.list[[k - 1L]] <- data.frame(
          fold = k, n_test = n.test, estimate = rate.fold$estimate,
          std.err = rate.fold$std.err, t_stat = NA_real_,
          dropped = TRUE, drop_reason = "non-finite t-statistic"
        )
        next
      }

      t.statistics <- c(t.statistics, t.stat)
      folds.list[[k - 1L]] <- data.frame(
        fold = k, n_test = n.test, estimate = rate.fold$estimate,
        std.err = rate.fold$std.err, t_stat = t.stat,
        dropped = FALSE, drop_reason = NA_character_
      )
    }

    folds.df <- do.call(rbind, folds.list[!vapply(folds.list, is.null, logical(1))])
    rownames(folds.df) <- NULL

    n.usable <- length(t.statistics)

    if (length(dropped.folds) > 0L) {
      warning("Sequential RATE: dropped ", length(dropped.folds),
              " of ", num.folds - 1, " folds due to degenerate fits (",
              paste(unique(drop.reasons), collapse = "; "), ").",
              call. = FALSE)
    }

    if (n.usable < 2L) {
      reason <- paste0(
        "Fewer than 2 usable folds after degeneracy filtering (",
        n.usable, " usable, ", length(dropped.folds), " dropped). ",
        "Likely cause: sample size too small for k-fold sequential RATE. ",
        "Prefer the Calibration test for formal inference at this n."
      )
      return(list(p = NA_real_, reason = reason,
                  folds_df = folds.df,
                  dropped_folds = dropped.folds,
                  drop_reasons = drop.reasons,
                  sequential_t = NA_real_,
                  num_folds = num.folds))
    }

    # H3 (audit-20260511): when one or more folds were dropped, the
    # sqrt(num.folds - 1) denominator under-normalizes (denom > n.usable),
    # deflating the aggregated t-statistic. Rather than emit a deflated
    # (and weakly anti-conservative-by-omission) p-value, return NA with
    # a reason. Users get clean NA + message instead of a silently biased
    # p-value.
    if (length(t.statistics) < (num.folds - 1)) {
      reason <- paste0(
        "Sequential RATE: ", length(t.statistics), " of ",
        num.folds - 1, " folds usable after degeneracy filtering. ",
        "Aggregation denominator sqrt(num.folds - 1) would deflate the ",
        "t-statistic relative to the number of contributing folds. ",
        "Returning NA p-value. Increase sample size or use the Calibration ",
        "test for formal inference at this n."
      )
      return(list(p = NA_real_, reason = reason,
                  folds_df = folds.df,
                  dropped_folds = dropped.folds,
                  drop_reasons = drop.reasons,
                  sequential_t = NA_real_,
                  num_folds = num.folds))
    }

    # Aggregation formula preserved per Wager (2024).
    # Uses sqrt(num.folds - 1) = sqrt(K-1), matching the original fold-count
    # normalization; this is intentionally not rescaled by n.usable here.
    t_seq <- sum(t.statistics) / sqrt(num.folds - 1)
    p_seq <- 2 * stats::pnorm(-abs(t_seq))
    list(p = p_seq, reason = NA_character_,
         folds_df = folds.df,
         dropped_folds = dropped.folds,
         drop_reasons = drop.reasons,
         sequential_t = t_seq,
         num_folds = num.folds)
  }

  # Upfront size check (Task A requirement): warn when Sequential RATE
  # likely unstable. Scales with the user-supplied num.folds.
  .n <- nrow(X)
  if (.n < 400L || (.n / num.folds) < min_fold_n) {
    warning(
      "Sequential RATE may be unstable at this sample size (n = ", .n,
      ", n/num.folds = ", round(.n / num.folds, 1),
      "; min_fold_n = ", min_fold_n, "). Training folds may be too small ",
      "for the per-fold CATE forest to detect heterogeneity, which can ",
      "produce degenerate RATE statistics. Consider the Calibration test ",
      "(Chernozhukov et al., 2018) or the OOB RATE heuristics instead.",
      call. = FALSE
    )
  }

  rate_result <- rate_sequential(X, Y, W, num.folds = num.folds)
  sequential_rate_test_pvalue <- rate_result$p
  if (is.na(sequential_rate_test_pvalue) && !is.na(rate_result$reason)) {
    message("Sequential RATE: ", rate_result$reason)
  }
  rate_payload <- list(
    folds_df      = rate_result$folds_df,
    sequential_t  = rate_result$sequential_t,
    p_value       = rate_result$p,
    num_folds     = rate_result$num_folds,
    dropped_folds = rate_result$dropped_folds,
    drop_reasons  = rate_result$drop_reasons,
    reason        = rate_result$reason,
    # OOB TOC curve + summary (the classic grf RATE viz; heuristic, not
    # the sequential test). Populated below once rate.oob is computed.
    toc_df           = NULL,
    oob_rate_target  = NA_character_,
    oob_rate_est     = NA_real_,
    oob_rate_se      = NA_real_
  )

  # Wager's heuristic OOB RATE test
  tau.hat.oob <- c.forest$predictions
  rate.oob <- rank_average_treatment_effect(c.forest, tau.hat.oob)

  t.stat.oob <- rate.oob$estimate / rate.oob$std.err
  p.val          <- 2 * stats::pnorm(-abs(t.stat.oob))
  p.val.onesided <- stats::pnorm(t.stat.oob, lower.tail = FALSE)

  # Stash TOC + OOB RATE summary so plot.omni_hetero() can render the
  # classic grf TOC curve alongside the per-fold forest plot.
  rate_payload$toc_df          <- as.data.frame(rate.oob$TOC)
  rate_payload$oob_rate_target <- as.character(rate.oob$target)
  rate_payload$oob_rate_est    <- rate.oob$estimate
  rate_payload$oob_rate_se     <- rate.oob$std.err

  # Combine results (Preferred tests first, then Heuristic)
  summary_table <- data.frame(
    category = c(
      "Preferred",
      "Preferred",
      "Heuristic",
      "Heuristic",
      "Heuristic"
    ),
    heterogeneity_test = c(
      "Sequential RATE (Wager, 2024)",
      "Calibration Test (Chernozhukov et al., 2018)",
      "High vs. Low CATE, cross-fit (Athey and Wager, 2019)",
      "OOB RATE, two-sided (heuristic, anti-conservative)",
      "OOB RATE, one-sided (heuristic)"
    ),
    estimate = c(
      NA_real_,
      cal.est,
      naive_high_low_diff,
      rate.oob$estimate,
      rate.oob$estimate
    ),
    p_value = c(
      sequential_rate_test_pvalue,
      cal.pval,
      naive_high_low_p_value,
      p.val,
      p.val.onesided
    ),
    hetero_detected = c(
      # C2a (audit-20260511): explicit NA propagation when p-value is NA.
      # Using ifelse(is.na(p), NA, p <= 0.05) gives a true logical NA in
      # the column (rather than `isTRUE()` which silently collapses NA to
      # FALSE and would print as "No heterogeneity"). The print method
      # then renders "--" for NA, signalling "test did not run" rather
      # than the wrong-and-confident "No".
      ifelse(is.na(sequential_rate_test_pvalue),
             NA, sequential_rate_test_pvalue <= 0.05),
      cal.pval <= 0.05,
      ifelse(is.na(naive_high_low_p_value),
             NA, naive_high_low_p_value <= 0.05),
      p.val <= 0.05,
      p.val.onesided <= 0.05
    )
  )

  class(summary_table) <- c("omni_hetero", "data.frame")
  attr(summary_table, "blp")  <- blp_payload
  attr(summary_table, "rate") <- rate_payload
  summary_table
}

#' Print Omnibus Heterogeneity Tests
#'
#' Formats the [omni_hetero()] output as a clean table suitable for an
#' appendix, grouped by Preferred and Heuristic categories.
#'
#' @param x An object of class `"omni_hetero"`.
#' @param latex Logical. If `TRUE`, prints a self-contained LaTeX
#'   `tabular` environment ready to copy-paste into a manuscript.
#'   Default is `FALSE`.
#' @param ... Additional arguments (currently unused).
#' @return The input object `x` (invisibly).
#'
#' @seealso [omni_hetero()]
#'
#' @method print omni_hetero
#' @export
print.omni_hetero <- function(x, latex = FALSE, ...) {

  if (latex) {
    print_omni_latex(x)
    return(invisible(x))
  }

  fmt <- as.data.frame(x)
  fmt$estimate <- ifelse(is.na(x$estimate), "--",
                         formatC(x$estimate, format = "f", digits = 4))
  fmt$p_value <- ifelse(is.na(x$p_value), "--",
                        formatC(x$p_value, format = "f", digits = 4))
  # C2b (audit-20260511): NA-safe formatter -- print em-dash when the
  # underlying p-value (and hence hetero_detected) is NA, instead of
  # propagating <NA> into the printed table.
  fmt$hetero_detected <- ifelse(is.na(x$hetero_detected), "--",
                                ifelse(x$hetero_detected, "Yes", "No"))

  preferred <- fmt[fmt$category == "Preferred", -1, drop = FALSE]
  heuristic <- fmt[fmt$category == "Heuristic", -1, drop = FALSE]

  cat("Omnibus Heterogeneity Tests\n")
  cat(strrep("-", 40), "\n")
  cat("\nPreferred (valid size)\n\n")
  print.data.frame(preferred, row.names = FALSE, right = FALSE)
  cat("\nHeuristic (screening only)\n\n")
  print.data.frame(heuristic, row.names = FALSE, right = FALSE)

  invisible(x)
}

#' Format p-value for LaTeX output
#' @noRd
fmt_pval_latex <- function(p) {
  ifelse(is.na(p) | is.nan(p), "---",
  ifelse(p < 0.001, "$<$0.001",
         formatC(p, format = "f", digits = 3)))
}

#' Print omni_hetero as LaTeX tabular
#' @noRd
print_omni_latex <- function(x) {

  # Short test names (no citations -- those go in table notes)
  short_names <- c(
    "Sequential RATE (Wager, 2024)"                              = "Sequential RATE",
    "Calibration Test (Chernozhukov et al., 2018)"                = "Calibration test",
    "High vs. Low CATE, cross-fit (Athey and Wager, 2019)"        = "High vs.\\ low CATE (cross-fit)",
    "OOB RATE, two-sided (heuristic, anti-conservative)"          = "OOB RATE (two-sided)",
    "OOB RATE, one-sided (heuristic)"                             = "OOB RATE (one-sided)"
  )

  est <- ifelse(is.na(x$estimate), "---",
                formatC(x$estimate, format = "f", digits = 4))
  pval <- fmt_pval_latex(x$p_value)
  sig <- ifelse(!is.na(x$hetero_detected) & x$hetero_detected, "Yes", "No")
  test <- short_names[x$heterogeneity_test]

  cat("\\begin{table}[ht]\n")
  cat("\\centering\n")
  cat("\\caption{Omnibus Tests of Treatment Effect Heterogeneity}\n")
  cat("\\label{tab:hetero}\n")
  cat("\\begin{tabular}{llccc}\n")
  cat("\\hline\n")
  cat(" & Test & Estimate & $p$-value & Detected \\\\\n")
  cat("\\hline\n")

  # Preferred
  pref_idx <- which(x$category == "Preferred")
  cat("\\textit{Preferred} & & & & \\\\\n")
  for (i in pref_idx) {
    cat(sprintf("  & %s & %s & %s & %s \\\\\n", test[i], est[i], pval[i], sig[i]))
  }

  # Heuristic
  heur_idx <- which(x$category == "Heuristic")
  cat("\\textit{Heuristic} & & & & \\\\\n")
  for (i in heur_idx) {
    cat(sprintf("  & %s & %s & %s & %s \\\\\n", test[i], est[i], pval[i], sig[i]))
  }

  cat("\\hline\n")
  cat("\\end{tabular}\n")
  cat("\\end{table}\n")
}
