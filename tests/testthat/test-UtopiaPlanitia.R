library(testthat)
library(grf)
library(UtopiaPlanitia)

make_cf <- function(n = 200, p = 5, num.trees = 100) {
  set.seed(1995)
  X <- matrix(rnorm(n * p), n, p)
  colnames(X) <- paste0("X", seq_len(p))
  W <- rbinom(n, 1, .5)
  Y <- X[, 1] * W + rnorm(n)
  grf::causal_forest(X, Y, W, num.trees = num.trees)
}

test_that("cf_loco returns cf_loco class with correct structure", {
  cf <- make_cf()
  result <- cf_loco(cf, normalize = TRUE)
  expect_s3_class(result, "cf_loco")
  expect_named(result, c("vimp", "normalized", "n", "p"))
  expect_true(result$normalized)
  expect_equal(sum(result$vimp$Importance), 1, tolerance = 1e-8)
})

test_that("print.cf_loco runs without error", {
  cf <- make_cf()
  result <- cf_loco(cf)
  expect_output(print(result), "LOCO Variable Importance")
})

test_that("summary.cf_loco runs without error", {
  cf <- make_cf()
  result <- cf_loco(cf)
  expect_output(summary(result), "LOCO Variable Importance")
})

test_that("plot.cf_loco returns a ggplot", {
  skip_if_not_installed("ggplot2")
  cf <- make_cf()
  result <- cf_loco(cf)
  p <- plot(result)
  expect_s3_class(p, "gg")
})

test_that("summary.causal_forest returns correct class and structure", {
  cf <- make_cf()
  s <- suppressWarnings(suppressMessages(summary(cf)))
  expect_s3_class(s, "summary.causal_forest")
  expect_named(s, c("ate", "vimp", "heterogeneity"))
  expect_length(s$ate, 2)
  expect_named(s$ate, c("estimate", "std.err"))
  expect_length(s$vimp, 5)
  expect_s3_class(s$heterogeneity, "data.frame")
})

test_that("print.summary.causal_forest runs without error", {
  cf <- make_cf()
  s <- suppressWarnings(suppressMessages(summary(cf)))
  expect_output(print(s), "Average Treatment Effect")
})

test_that("plot.causal_forest validates type argument", {
  cf <- make_cf()
  expect_error(plot(cf, type = "invalid"), "should be one of")
})

test_that("plot_rank returns a ggplot", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("ggdist")
  cf <- make_cf()
  p <- plot_rank(cf)
  expect_s3_class(p, "gg")
})

test_that("rank_plot is deprecated", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("ggdist")
  cf <- make_cf()
  expect_warning(rank_plot(cf), "deprecated")
})

test_that("plot_scatter returns a ggExtraPlot", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("ggExtra")
  cf <- make_cf()
  p <- plot_scatter(cf, x_var = "X1")
  expect_s3_class(p, "ggExtraPlot")
})

test_that("plot_pdp 1-way returns a ggExtraPlot", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("ggExtra")
  cf <- make_cf()
  p <- plot_pdp(cf, x_var = "X1", grid_size = 3)
  expect_s3_class(p, "ggExtraPlot")
})

test_that("plot_pdp 2-way returns a ggExtraPlot", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("ggExtra")
  cf <- make_cf()
  p <- plot_pdp(cf, x_var = "X1", y_var = "X2", grid_size = 3)
  expect_s3_class(p, "ggExtraPlot")
})

test_that("plot_pdp 2-way trim = FALSE skips convex hull masking", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("ggExtra")
  cf <- make_cf()
  p <- plot_pdp(cf, x_var = "X1", y_var = "X2", grid_size = 3, trim = FALSE)
  expect_s3_class(p, "ggExtraPlot")
})

test_that("plot_pdp rejects bad variable names", {
  cf <- make_cf()
  expect_error(plot_pdp(cf, x_var = "not_a_var"), "not in the covariate matrix")
  expect_error(plot_pdp(cf, x_var = "X1", y_var = "not_a_var"), "not in the covariate matrix")
})

test_that("plot_inter emits deprecation warning", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("ggExtra")
  skip_if_not_installed("hexbin")
  cf <- make_cf()
  expect_warning(plot_inter(cf, x_var = "X1", y_var = "X2"), "deprecated")
})

test_that("plot.causal_forest dispatches scatter type", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("ggExtra")
  cf <- make_cf()
  p <- plot(cf, type = "scatter", x_var = "X1")
  expect_s3_class(p, "ggExtraPlot")
})

test_that("plot.causal_forest inter type emits deprecation warning", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("ggExtra")
  skip_if_not_installed("hexbin")
  cf <- make_cf()
  expect_warning(plot(cf, type = "inter", x_var = "X1", y_var = "X2"), "deprecated")
})

test_that("cf_loco stabilize warns on near-zero denominator", {
  cf <- make_cf()
  # Set an absurdly high threshold so every observation triggers the warning.
  # Multiple warnings fire (one per compute_vimp call); suppress extras.
  suppressWarnings(
    expect_warning(cf_loco(cf, stabilize = 1e6), "near-zero local treatment variation")
  )
})

test_that("cf_loco stabilize = 0 disables clamping", {
  cf <- make_cf()
  # stabilize = 0 should not warn (threshold is zero, nothing clamped)
  expect_no_warning(cf_loco(cf, stabilize = 0))
})

test_that("plot_pdp num.threads passes without error", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("ggExtra")
  cf <- make_cf()
  p <- plot_pdp(cf, x_var = "X1", grid_size = 3, num.threads = 1)
  expect_s3_class(p, "ggExtraPlot")
})

test_that("plot_pdp 2-way respects explicit n_max", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("ggExtra")
  cf <- make_cf()
  # Explicit n_max = 100 should be used (not the 500 default for 2-way)
  p <- plot_pdp(cf, x_var = "X1", y_var = "X2", grid_size = 3, n_max = 100)
  expect_s3_class(p, "ggExtraPlot")
})

test_that("plot_pdp 2-way hull trim emits message", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("ggExtra")
  cf <- make_cf()
  expect_message(
    plot_pdp(cf, x_var = "X1", y_var = "X2", grid_size = 5),
    "trimmed"
  )
})

test_that("cf_loco screen = TRUE returns correct structure and messages", {
  cf <- make_cf()
  expect_message(result <- cf_loco(cf, screen = TRUE), "Screening")
  expect_s3_class(result, "cf_loco")
  expect_equal(nrow(result$vimp), 5)
  # screened-out variables should have importance = 0
  expect_true(any(result$vimp$Importance == 0))
})

test_that("cf_loco screen = integer keeps exactly k nonzero", {
  cf <- make_cf()
  expect_message(result <- cf_loco(cf, screen = 3), "top 3 of 5")
  nonzero <- sum(result$vimp$Importance != 0)
  expect_equal(nonzero, 3)
})

test_that("cf_loco screen = FALSE with small p does not prompt", {
  cf <- make_cf()
  # small p, low cost — should run without prompting or screening
  result <- cf_loco(cf, screen = FALSE)
  expect_s3_class(result, "cf_loco")
  expect_equal(nrow(result$vimp), 5)
})

# -- omni_hetero tests --------------------------------------------------------

test_that("omni_hetero returns correct structure", {
  cf <- make_cf()
  result <- suppressWarnings(suppressMessages(omni_hetero(cf)))
  expect_s3_class(result, "omni_hetero")
  expect_s3_class(result, "data.frame")
  expect_named(result, c("category", "heterogeneity_test", "estimate", "p_value", "hetero_detected"))
  expect_equal(nrow(result), 5)
  # Preferred tests first, then Heuristic
  expect_equal(result$category, c("Preferred", "Preferred", "Heuristic", "Heuristic", "Heuristic"))
  expect_equal(result$heterogeneity_test[1], "Sequential RATE (Wager, 2024)")
  expect_equal(result$heterogeneity_test[2], "Calibration Test (Chernozhukov et al., 2018)")
  # p-values should be between 0 and 1 (NA allowed for estimate-only rows)
  pvals <- result$p_value[!is.na(result$p_value)]
  expect_true(all(pvals >= 0 & pvals <= 1))
  # hetero_detected should be logical
  expect_type(result$hetero_detected, "logical")
})

test_that("print.omni_hetero runs without error", {
  cf <- make_cf()
  result <- suppressWarnings(suppressMessages(omni_hetero(cf)))
  expect_output(print(result), "Preferred")
  expect_output(print(result), "Heuristic")
})

test_that("print.omni_hetero latex output produces valid LaTeX", {
  cf <- make_cf()
  result <- suppressWarnings(suppressMessages(omni_hetero(cf)))
  out <- capture.output(print(result, latex = TRUE))
  expect_true(any(grepl("\\\\begin\\{table\\}", out)))
  expect_true(any(grepl("\\\\end\\{table\\}", out)))
  expect_true(any(grepl("\\\\textit\\{Preferred\\}", out)))
  expect_true(any(grepl("\\\\textit\\{Heuristic\\}", out)))
  expect_true(any(grepl("Sequential RATE", out)))
})

test_that("omni_hetero rejects non-causal_forest input", {
  expect_error(omni_hetero("not a forest"), "must be a grf causal forest")
})

test_that("omni_hetero seed is reproducible", {
  cf <- make_cf()
  r1 <- suppressWarnings(suppressMessages(omni_hetero(cf, seed = 42)))
  r2 <- suppressWarnings(suppressMessages(omni_hetero(cf, seed = 42)))
  expect_equal(r1, r2)
})

# -- loco tests ---------------------------------------------------------------

test_that("loco OOB mode returns correct structure", {
  skip_if_not_installed("ranger")
  set.seed(1995)
  dat <- data.frame(y = rnorm(100), x1 = rnorm(100), x2 = rnorm(100), x3 = rnorm(100))
  mod <- ranger::ranger(y ~ ., data = dat, num.trees = 50)
  result <- loco(mod, split = FALSE)
  expect_s3_class(result, "data.frame")
  expect_named(result, c("variable", "importance"))
  expect_equal(nrow(result), 3)
})

test_that("loco rejects non-ranger input", {
  expect_error(loco(list()), "ranger")
})

test_that("loco OOB mode importance values are numeric", {
  skip_if_not_installed("ranger")
  set.seed(1995)
  dat <- data.frame(y = rnorm(100), x1 = rnorm(100), x2 = rnorm(100))
  mod <- ranger::ranger(y ~ ., data = dat, num.trees = 50)
  result <- loco(mod, split = FALSE)
  expect_type(result$importance, "double")
  expect_true(all(!is.na(result$importance)))
})

# -- plot_diag tests -----------------------------------------------------------

test_that("plot_diag returns correct structure", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("ggdist")
  skip_if_not_installed("gridExtra")
  skip_if_not_installed("MLbalance")
  cf <- make_cf()
  result <- expect_output(plot_diag(cf), "RMSE")
  expect_type(result, "list")
  expect_named(result, c("outcome_check_plot", "treatment_propensity_plot",
                          "cates_distribution_plot", "rmse_out"))
  expect_s3_class(result$outcome_check_plot, "gg")
  expect_s3_class(result$cates_distribution_plot, "gg")
  expect_type(result$rmse_out, "double")
  expect_true(result$rmse_out > 0)
})
