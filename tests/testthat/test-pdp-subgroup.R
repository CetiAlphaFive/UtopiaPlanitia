make_cf <- function(n = 400, seed = 1995) {
  set.seed(seed)
  p <- 4
  X <- matrix(stats::rnorm(n * p), n, p)
  colnames(X) <- paste0("X", seq_len(p))
  X[, "X1"] <- rbinom(n, 1, 0.5)            # binary covariate
  X[, "X2"] <- sample(0:2, n, replace = TRUE) # 3-level integer covariate
  W <- rbinom(n, 1, 0.5)
  Y <- X[, "X1"] * W + stats::rnorm(n)
  grf::causal_forest(X, Y, W, num.trees = 200)
}

test_that(".subgroup_ate_1way_df returns one row per level with CI + signif", {
  skip_on_cran()
  cf <- make_cf()
  df <- UtopiaPlanitia:::.subgroup_ate_1way_df(cf, "X1")

  expect_s3_class(df, "data.frame")
  expect_setequal(df$level, c(0, 1))
  expect_true(all(c("estimate", "std.err", "lower", "upper", "signif", "level")
                  %in% names(df)))
  expect_type(df$signif, "logical")
  # CI consistency
  expect_equal(df$lower, df$estimate - 1.96 * df$std.err)
  expect_equal(df$upper, df$estimate + 1.96 * df$std.err)
})

test_that("plot_pdp(subgroup = TRUE) 1-way returns a utopia_plot", {
  skip_on_cran()
  skip_if_not_installed("ggplot2")
  cf <- make_cf()
  p <- plot_pdp(cf, x_var = "X1", subgroup = TRUE)
  expect_s3_class(p, "utopia_plot")
})

test_that("plot_pdp(subgroup = FALSE) is unchanged (still utopia_plot)", {
  skip_on_cran()
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("ggExtra")
  cf <- make_cf()
  p <- plot_pdp(cf, x_var = "X1")
  expect_s3_class(p, "utopia_plot")
})

test_that("a level with too few units is warned and dropped", {
  skip_on_cran()
  cf <- make_cf(n = 200)
  # Inject a rare extra level (single unit) into X2 of the stored matrix
  cf$X.orig[1, "X2"] <- 99
  expect_warning(
    df <- UtopiaPlanitia:::.subgroup_ate_1way_df(cf, "X2"),
    "skipped"
  )
  expect_false(99 %in% df$level)   # the singleton level was dropped
})

test_that("all-fail subsets raise an informative error", {
  skip_on_cran()
  cf <- make_cf(n = 120)
  # X3 is continuous => every value unique => every subset has 1 unit => all fail
  expect_error(
    suppressWarnings(UtopiaPlanitia:::.subgroup_ate_1way_df(cf, "X3")),
    "no level of 'X3'"
  )
})

test_that(".subgroup_ate_2way_df returns one row per non-empty cell with a label", {
  skip_on_cran()
  cf <- make_cf()
  df <- UtopiaPlanitia:::.subgroup_ate_2way_df(cf, "X1", "X2")
  expect_s3_class(df, "data.frame")
  expect_true(all(c("xv", "yv", "estimate", "signif", "lab") %in% names(df)))
  # X1 has 2 levels, X2 has 3 => up to 6 cells
  expect_lte(nrow(df), 6)
  expect_gt(nrow(df), 0)
  # significant cells carry a trailing asterisk
  expect_true(all(grepl("\\*$", df$lab[df$signif])))
  expect_false(any(grepl("\\*$", df$lab[!df$signif])))
})

test_that("plot_pdp 2-way subgroup returns a utopia_plot", {
  skip_on_cran()
  skip_if_not_installed("ggplot2")
  cf <- make_cf()
  p <- plot_pdp(cf, x_var = "X1", y_var = "X2", subgroup = TRUE)
  expect_s3_class(p, "utopia_plot")
})
