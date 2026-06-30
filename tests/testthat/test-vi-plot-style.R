# Structural tests for the restyled VI plots (plot.cf_loco, plot.cf_perm).
# No vdiffr / pixel snapshots (fragile across ggplot2 versions): assert the
# house style is wired up via layer classes instead.

# Helper: the ggproto geom class names present in a built ggplot's layers.
.geom_classes <- function(g) {
  vapply(g$layers, function(l) class(l$geom)[1], character(1))
}

test_that("plot.cf_loco returns the house VI bar style", {
  skip_if_not_installed("ggplot2")
  cf <- make_test_cf()
  vi <- cf_loco(cf, verbose = FALSE)

  g <- plot(vi)
  expect_s3_class(g, "ggplot")

  geoms <- .geom_classes(g)
  expect_true("GeomCol" %in% geoms)
  expect_true("GeomPoint" %in% geoms)
  expect_true("GeomVline" %in% geoms)
  # Horizontal orientation: importance on x, variable on y (no coord_flip).
  expect_identical(rlang::as_label(g$mapping$x), "Importance")
  expect_identical(rlang::as_label(g$mapping$y), "Variable")
  expect_s3_class(g$coordinates, "CoordCartesian")
})

test_that("plot.cf_loco honors the fill argument and normalized title", {
  skip_if_not_installed("ggplot2")
  cf <- make_test_cf()

  vi <- cf_loco(cf, verbose = FALSE)
  g  <- plot(vi, fill = "#33a02c")
  col_layer <- g$layers[[which(.geom_classes(g) == "GeomCol")[1]]]
  expect_identical(col_layer$aes_params$fill, "#33a02c")
  expect_identical(g$labels$title, "LOCO Variable Importance")

  vin <- suppressWarnings(cf_loco(cf, normalize = TRUE, verbose = FALSE))
  gn  <- plot(vin)
  expect_identical(gn$labels$title, "LOCO Variable Importance (normalized)")
})

test_that("plot.cf_perm returns the house VI bar style with a CI whisker", {
  skip_if_not_installed("ggplot2")
  cf  <- make_test_cf()
  res <- cf_perm(cf, n.perm = 15, seed = 1, verbose = FALSE)

  g <- plot(res)
  expect_s3_class(g, "ggplot")

  geoms <- .geom_classes(g)
  expect_true("GeomCol" %in% geoms)
  expect_true("GeomPoint" %in% geoms)
  expect_true("GeomVline" %in% geoms)
  # One-sided lower-CI whisker present when CIs are finite.
  expect_true("GeomSegment" %in% geoms)
  expect_s3_class(g$coordinates, "CoordCartesian")
})

test_that("plot.cf_perm shades a recognizable significant case (signal DGP)", {
  skip_if_not_installed("ggplot2")
  cf  <- make_test_cf()
  res <- cf_perm(cf, n.perm = 15, seed = 1, verbose = FALSE)

  # X1 drives the CATE in make_test_cf(); at least one covariate should clear
  # the one-sided lower bound and thus take the significant fill.
  sig <- !is.na(res$vimp$CI.lower) & res$vimp$CI.lower > 0
  expect_true(any(sig))

  g <- plot(res)
  # The fill aesthetic is mapped to Significant (so fills split by significance).
  fill_layer <- g$layers[[which(.geom_classes(g) == "GeomCol")[1]]]
  expect_identical(rlang::as_label(fill_layer$mapping$fill), "Significant")
})

test_that("plot.cf_perm drops the whisker and de-shades when normalized", {
  skip_if_not_installed("ggplot2")
  set.seed(11)
  n <- 250; p <- 3
  X <- matrix(stats::rnorm(n * p), n, p); colnames(X) <- paste0("X", 1:p)
  W <- stats::rbinom(n, 1, 0.5)
  Y <- stats::rnorm(n)  # no HTE -> normalize warns, CI.lower all NA
  cf <- grf::causal_forest(X, Y, W, num.trees = 250, seed = 11)
  res <- suppressWarnings(
    cf_perm(cf, n.perm = 10, normalize = TRUE, seed = 1, verbose = FALSE))

  # Normalized objects carry NA confidence bounds, so nothing is significant.
  expect_true(all(is.na(res$vimp$CI.lower)))

  g <- plot(res)
  geoms <- .geom_classes(g)
  expect_true("GeomCol" %in% geoms)
  expect_false("GeomSegment" %in% geoms)        # no finite CI -> no whisker
  expect_identical(g$labels$title,
                   "PermuCATE Variable Importance (normalized)")
})
