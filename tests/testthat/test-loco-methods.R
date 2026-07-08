# Tests for loco()'s S3 methods (print.loco_vimp, summary.loco_vimp,
# plot.loco_vimp) and the classed "loco_vimp" return object. Self-sufficient:
# does not rely on helpers defined in other test files (test files may be
# run standalone via testthat::test_file()).

skip_if_no_ranger <- function() {
  testthat::skip_if_not_installed("ranger")
}

# Helper: the ggproto geom class names present in a built ggplot's layers.
# (Local copy of the convention used in test-vi-plot-style.R.)
.geom_classes <- function(g) {
  vapply(g$layers, function(l) class(l$geom)[1], character(1))
}

make_signal_model <- function(n = 150, seed = 1, trees = 100) {
  set.seed(seed)
  dat <- data.frame(
    y  = rnorm(n),
    x1 = rnorm(n),
    x2 = rnorm(n),
    x3 = rnorm(n)
  )
  dat$y <- 2 * dat$x1 + rnorm(n, sd = 0.5)
  list(dat = dat, mod = ranger::ranger(y ~ ., data = dat, num.trees = trees))
}

# ---- 2.1 / 2.2: split mode (z, wilcox) -----------------------------------

test_that("loco() split mode, method='z' returns a loco object with working methods", {
  skip_if_no_ranger()
  b <- make_signal_model()
  vi <- suppressWarnings(loco(b$mod, data = b$dat, split = TRUE, method = "z", seed = 1))

  expect_s3_class(vi, "loco_vimp")
  expect_true(is.list(vi))
  expect_false(is.data.frame(vi))
  expect_named(vi$vimp, c("Variable", "Importance", "CI.lower", "CI.upper", "p.value"))
  expect_identical(vi$method, "z")
  expect_identical(vi$split, TRUE)
  expect_true(all(vi$vimp$CI.lower <= vi$vimp$CI.upper))
  expect_equal(vi$vimp$Importance, (vi$vimp$CI.lower + vi$vimp$CI.upper) / 2)

  out <- capture.output(print(vi))
  expect_true(length(out) > 0)
  expect_true(any(grepl("LOCO Variable Importance", out, fixed = TRUE)))
  expect_identical(print(vi), vi)
  expect_identical(capture.output(summary(vi)), capture.output(print(vi)))

  testthat::skip_if_not_installed("ggplot2")
  g <- plot(vi)
  expect_s3_class(g, "ggplot")
  geoms <- .geom_classes(g)
  expect_true("GeomCol" %in% geoms)
  expect_true("GeomPoint" %in% geoms)
  expect_true("GeomVline" %in% geoms)
  expect_true("GeomSegment" %in% geoms)
})

test_that("loco() split mode, method='wilcox' returns a loco object with working methods", {
  skip_if_no_ranger()
  b <- make_signal_model()
  vi <- suppressWarnings(loco(b$mod, data = b$dat, split = TRUE, method = "wilcox", seed = 1))

  expect_identical(vi$method, "wilcox")
  expect_named(vi$vimp, c("Variable", "Importance", "CI.lower", "CI.upper", "p.value"))
  expect_true(all(vi$vimp$CI.lower <= vi$vimp$CI.upper, na.rm = TRUE))

  expect_no_error(print(vi))
  expect_no_error(summary(vi))

  testthat::skip_if_not_installed("ggplot2")
  g <- plot(vi)
  expect_s3_class(g, "ggplot")
  geoms <- .geom_classes(g)
  expect_true("GeomSegment" %in% geoms)
})

# ---- 2.3: OOB mode (no inference) ----------------------------------------

test_that("loco() OOB mode degrades cleanly (no CI/p.value, no whisker)", {
  skip_if_no_ranger()
  b <- make_signal_model()
  vi <- loco(b$mod, data = b$dat, split = FALSE, seed = 1)

  expect_identical(vi$split, FALSE)
  expect_identical(vi$method, "oob")
  expect_true(all(is.na(vi$vimp$CI.lower)))
  expect_true(all(is.na(vi$vimp$CI.upper)))
  expect_true(all(is.na(vi$vimp$p.value)))

  expect_no_error(print(vi))
  expect_no_error(summary(vi))
  expect_identical(print(vi), vi)
  expect_identical(summary(vi), vi)

  testthat::skip_if_not_installed("ggplot2")
  g <- plot(vi)
  expect_s3_class(g, "ggplot")
  geoms <- .geom_classes(g)
  expect_true("GeomCol" %in% geoms)
  expect_true("GeomPoint" %in% geoms)
  expect_true("GeomVline" %in% geoms)
  expect_false("GeomSegment" %in% geoms)
  expect_identical(g$labels$title, "LOCO Variable Importance (OOB)")
})

# ---- 2.4: group mode ------------------------------------------------------

test_that("loco() group mode (OOB): Members list-column and readable print", {
  skip_if_no_ranger()
  b <- make_signal_model()
  vi <- loco(b$mod, data = b$dat, split = FALSE, seed = 1,
             groups = list(gA = c("x1", "x2"), gB = "x3"))

  expect_true(vi$group)
  expect_true("Members" %in% names(vi$vimp))
  expect_setequal(vi$vimp$Variable, c("gA", "gB"))
  expect_setequal(vi$vimp$Members[[which(vi$vimp$Variable == "gA")]], c("x1", "x2"))

  out <- capture.output(print(vi))
  expect_true(any(grepl("x1", out, fixed = TRUE)))
  expect_true(any(grepl("x2", out, fixed = TRUE)))

  testthat::skip_if_not_installed("ggplot2")
  g <- plot(vi)
  expect_s3_class(g, "ggplot")
  expect_setequal(levels(g$data$Variable), c("gA", "gB"))
})

test_that("loco() group mode + split mode: whisker renders with group labels", {
  skip_if_no_ranger()
  b <- make_signal_model()
  vi <- suppressWarnings(loco(b$mod, data = b$dat, split = TRUE, method = "z", seed = 1,
                              groups = list(gA = c("x1", "x2"), gB = "x3")))

  expect_s3_class(vi, "loco_vimp")
  expect_true(vi$group)
  expect_named(vi$vimp, c("Variable", "Importance", "CI.lower", "CI.upper",
                          "p.value", "Members"))
  expect_true(all(vi$vimp$CI.lower <= vi$vimp$CI.upper))

  testthat::skip_if_not_installed("ggplot2")
  g <- plot(vi)
  expect_s3_class(g, "ggplot")
  geoms <- .geom_classes(g)
  expect_true("GeomSegment" %in% geoms)
})

# ---- 2.5: fill.sig argument ------------------------------------------------

test_that("plot.loco honors the fill.sig argument", {
  skip_if_no_ranger()
  testthat::skip_if_not_installed("ggplot2")
  b <- make_signal_model()
  vi <- suppressWarnings(loco(b$mod, data = b$dat, split = TRUE, method = "z", seed = 1))

  g <- plot(vi, fill.sig = "#33a02c")
  scales <- g$scales$scales
  fill_idx <- which(vapply(scales, function(s) identical(s$aesthetics, "fill"),
                           logical(1)))
  expect_equal(length(fill_idx), 1L)
  pal <- scales[[fill_idx]]$palette(TRUE)
  expect_identical(unname(pal[["TRUE"]]), "#33a02c")
})

# ---- 3. Edge cases ---------------------------------------------------------

test_that("loco() single-group bare character vector groups arg works with print/plot", {
  skip_if_no_ranger()
  b <- make_signal_model()
  vi <- loco(b$mod, data = b$dat, split = FALSE, seed = 1, groups = c("x1", "x2"))

  expect_equal(nrow(vi$vimp), 1L)
  expect_identical(vi$vimp$Variable, "group1")
  expect_true(vi$group)

  expect_no_error(print(vi))
  testthat::skip_if_not_installed("ggplot2")
  expect_no_error(plot(vi))
})
