#' Omnibus Tests of Heterogeneity
#'
#' Performs various heterogeneity tests on a fitted causal forest model.
#' Combines the calibration test of Chernozhukov et al. (2018), a naive
#' high/low CATE split (Athey and Wager, 2019), the sequential RATE test
#' (Wager, 2024), and OOB RATE heuristics into a single summary table.
#'
#' @param c.forest A fitted causal forest object from the \code{grf} package.
#' @param seed An integer seed for reproducibility. Default is `1995`.
#'   Controls the fold assignment in the sequential RATE test.
#' @param min_fold_n Minimum per-fold training sample size below which the
#'   Sequential RATE test is considered unstable. When `n < 400` or
#'   `n / num.folds < min_fold_n`, a warning is emitted at the start of the
#'   Sequential RATE computation and users are advised to prefer the
#'   Calibration test (Chernozhukov et al., 2018) or OOB RATE heuristics
#'   for small samples. Within the fold loop, any fold whose test-set CATE
#'   predictions are (near-)constant, whose RATE standard error is zero,
#'   or whose t-statistic is `NaN` is dropped from aggregation rather than
#'   allowed to propagate a silent `NaN` into the final p-value. If fewer
#'   than 2 usable folds remain, the Sequential RATE p-value is returned
#'   as `NA_real_` with an explanatory `reason` attribute. Default is
#'   `100`.
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
#'     \item{hetero_detected}{Logical. `TRUE` if `p_value <= 0.05`.}
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
#' 3. **High vs. low CATE** (Athey and Wager, 2019): Splits units at the
#'    median predicted CATE and compares the ATE in each half. A significant
#'    difference suggests the forest detects real variation.
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
omni_hetero <- function(c.forest, seed = 1995, min_fold_n = 100) {

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
  #
  # Robustness additions (v0.3.2):
  #   - Upfront size check: warn if n < 400 or n/num.folds < min_fold_n.
  #   - Per-fold degeneracy guard: drop folds where the test-set CATE forest
  #     predicts (near-)constant, where the fold RATE std.err is zero, or
  #     where the resulting t-statistic is NaN / non-finite. These otherwise
  #     propagate silently into the aggregated p-value.
  #   - If fewer than 2 usable folds remain, return NA_real_ with a `reason`
  #     attribute instead of a silent NaN.
  #   - The sum(t)/sqrt(num.folds - 1) aggregation is PRESERVED (any change
  #     to that formula is a separate, higher-tier concern about fold
  #     independence — see omni_hetero audit).
  rate_sequential <- function(X, Y, W, num.folds = 5) {

    set.seed(seed)
    fold.id <- sample(rep(1:num.folds, length = nrow(X)))
    samples.by.fold <- split(seq_along(fold.id), fold.id)

    t.statistics  <- c()
    dropped.folds <- integer(0)
    drop.reasons  <- character(0)

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

    # small-sample tolerance for near-constant CATE predictions
    cate.tol <- 1e-8

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

      # guard 1: degenerate training fit — CATE forest predicts (near-)constant
      if (!is.finite(stats::sd(cate.hat.test)) ||
          stats::sd(cate.hat.test) < cate.tol) {
        dropped.folds <- c(dropped.folds, k)
        drop.reasons  <- c(drop.reasons,
                           "near-constant CATE predictions on test fold")
        next
      }

      rate.fold <- rank_average_treatment_effect.fit(DR.scores[test], cate.hat.test)

      # guard 2: RATE std.err is zero / non-finite (t = 0/0 = NaN)
      if (!is.finite(rate.fold$std.err) || rate.fold$std.err == 0) {
        dropped.folds <- c(dropped.folds, k)
        drop.reasons  <- c(drop.reasons,
                           "zero or non-finite RATE std.err")
        next
      }

      t.stat <- rate.fold$estimate / rate.fold$std.err

      # guard 3: defensive — any remaining non-finite t (e.g. Inf / NaN)
      if (!is.finite(t.stat)) {
        dropped.folds <- c(dropped.folds, k)
        drop.reasons  <- c(drop.reasons, "non-finite t-statistic")
        next
      }

      t.statistics <- c(t.statistics, t.stat)
    }

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
      pval <- NA_real_
      attr(pval, "reason") <- reason
      return(pval)
    }

    # Aggregation formula preserved per Wager (2024).
    # Uses sqrt(num.folds - 1) = sqrt(K-1), matching the original fold-count
    # normalization; this is intentionally not rescaled by n.usable here.
    2 * stats::pnorm(-abs(sum(t.statistics) / sqrt(num.folds - 1)))
  }

  X <- c.forest$X.orig
  Y <- c.forest$Y.orig
  W <- c.forest$W.orig

  # Upfront size check (Task A requirement): warn when Sequential RATE
  # likely unstable. Uses default num.folds = 5 from rate_sequential().
  .seq.num.folds <- 5L
  .n <- nrow(X)
  if (.n < 400L || (.n / .seq.num.folds) < min_fold_n) {
    warning(
      "Sequential RATE may be unstable at this sample size (n = ", .n,
      ", n/num.folds = ", round(.n / .seq.num.folds, 1),
      "; min_fold_n = ", min_fold_n, "). Training folds may be too small ",
      "for the per-fold CATE forest to detect heterogeneity, which can ",
      "produce degenerate RATE statistics. Consider the Calibration test ",
      "(Chernozhukov et al., 2018) or the OOB RATE heuristics instead.",
      call. = FALSE
    )
  }

  sequential_rate_test_pvalue <- rate_sequential(X, Y, W)
  if (is.na(sequential_rate_test_pvalue) &&
      !is.null(attr(sequential_rate_test_pvalue, "reason"))) {
    message("Sequential RATE: ", attr(sequential_rate_test_pvalue, "reason"))
    # strip attribute so it doesn't leak into the data.frame column
    sequential_rate_test_pvalue <- NA_real_
  }

  # Wager's heuristic OOB RATE test
  tau.hat.oob <- c.forest$predictions
  rate.oob <- rank_average_treatment_effect(c.forest, tau.hat.oob)

  t.stat.oob <- rate.oob$estimate / rate.oob$std.err
  p.val          <- 2 * stats::pnorm(-abs(t.stat.oob))
  p.val.onesided <- stats::pnorm(t.stat.oob, lower.tail = FALSE)

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
      "High vs. Low CATE (Athey and Wager, 2019)",
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
      sequential_rate_test_pvalue <= 0.05,
      cal.pval <= 0.05,
      naive_high_low_p_value <= 0.05,
      p.val <= 0.05,
      p.val.onesided <= 0.05
    )
  )

  class(summary_table) <- c("omni_hetero", "data.frame")
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
  fmt$estimate <- ifelse(is.na(x$estimate), "\u2014",
                         formatC(x$estimate, format = "f", digits = 4))
  fmt$p_value <- formatC(x$p_value, format = "f", digits = 4)
  fmt$hetero_detected <- ifelse(x$hetero_detected, "Yes", "No")

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

  # Short test names (no citations — those go in table notes)
  short_names <- c(
    "Sequential RATE (Wager, 2024)"                      = "Sequential RATE",
    "Calibration Test (Chernozhukov et al., 2018)"        = "Calibration test",
    "High vs. Low CATE (Athey and Wager, 2019)"           = "High vs.\\ low CATE",
    "OOB RATE, two-sided (heuristic, anti-conservative)"  = "OOB RATE (two-sided)",
    "OOB RATE, one-sided (heuristic)"                     = "OOB RATE (one-sided)"
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
