# Tests for audit-20260511 fixes (C1, C2, H2, H3, H5)
# Independent test pass — reads test-spec.md, does NOT read diff.

library(testthat)
library(grf)
library(UtopiaPlanitia)

# Resolve the R source path robustly — load_all path vs installed package path.
.uto_R_dir <- function() {
  # Prefer the dev source if we are running under devtools::test()
  cand <- c(
    file.path("..", "..", "R"),                                          # tests/testthat -> pkg/
    "/run/media/jack/storage/Dropbox/UtopiaPlanitia/R"                   # absolute fallback
  )
  for (d in cand) if (dir.exists(d)) return(normalizePath(d))
  stop("Cannot locate UtopiaPlanitia R/ source directory.")
}

# ------------------------------------------------------------------
# T-C1: Sequential RATE per-fold forest does NOT receive full-sample
# Y.hat / W.hat. Outer nuisance forest DOES.
# ------------------------------------------------------------------
test_that("C1: inner per-fold causal_forest in rate_sequential drops Y.hat[train]/W.hat[train]", {
  src_path <- file.path(.uto_R_dir(), "omni_hetero.R")
  expect_true(file.exists(src_path))
  src <- readLines(src_path)
  joined <- paste(src, collapse = "\n")

  # The inner cate.forest fit should not have Y.hat[train] or W.hat[train]
  # anywhere in its argument list. We locate the cate.forest binding then
  # take the source up to the matching closing paren.

  cate_start <- grep("cate\\.forest\\s*<-\\s*causal_forest\\(", src)
  expect_length(cate_start, 1)

  # Walk paren depth from that line to find the closing call
  depth <- 0
  end_line <- NA_integer_
  for (i in cate_start:length(src)) {
    depth <- depth + lengths(regmatches(src[i], gregexpr("\\(", src[i])))
    depth <- depth - lengths(regmatches(src[i], gregexpr("\\)", src[i])))
    if (depth <= 0) { end_line <- i; break }
  }
  expect_false(is.na(end_line))
  inner_block <- paste(src[cate_start:end_line], collapse = "\n")

  expect_false(grepl("Y.hat[train]", inner_block, fixed = TRUE),
               info = "inner cate.forest must not receive Y.hat[train]")
  expect_false(grepl("W.hat[train]", inner_block, fixed = TRUE),
               info = "inner cate.forest must not receive W.hat[train]")

  # Now: the OUTER nuisance.forest call SHOULD still pass Y.hat = Y.hat and
  # W.hat = W.hat (full-sample). Locate it.
  nuis_start <- grep("nuisance\\.forest\\s*<-\\s*causal_forest\\(", src)
  expect_length(nuis_start, 1)
  depth <- 0
  end_line <- NA_integer_
  for (i in nuis_start:length(src)) {
    depth <- depth + lengths(regmatches(src[i], gregexpr("\\(", src[i])))
    depth <- depth - lengths(regmatches(src[i], gregexpr("\\)", src[i])))
    if (depth <= 0) { end_line <- i; break }
  }
  outer_block <- paste(src[nuis_start:end_line], collapse = "\n")
  expect_true(grepl("Y.hat\\s*=\\s*Y.hat\\b", outer_block),
              info = "outer nuisance.forest must still pass full-sample Y.hat")
  expect_true(grepl("W.hat\\s*=\\s*W.hat\\b", outer_block),
              info = "outer nuisance.forest must still pass full-sample W.hat")
})

# ------------------------------------------------------------------
# T-C2: NA-safe hetero_detected + clean print
# ------------------------------------------------------------------
make_small_null_cf_t <- function(n = 200, p = 5, num.trees = 100, seed = 1995) {
  set.seed(seed)
  X <- matrix(rnorm(n * p), n, p); colnames(X) <- paste0("X", seq_len(p))
  W <- rbinom(n, 1, 0.5)
  Y <- rnorm(n)
  grf::causal_forest(X, Y, W, num.trees = num.trees)
}

test_that("C2: hetero_detected is logical NA (not NaN, not FALSE) when seq p-value is NA", {
  cf <- make_small_null_cf_t()
  out <- suppressWarnings(suppressMessages(omni_hetero(cf)))
  expect_s3_class(out, "omni_hetero")
  expect_type(out$hetero_detected, "logical")
  seq_row <- grep("Sequential RATE", out$heterogeneity_test)
  expect_length(seq_row, 1)
  # Expect NA in the hetero_detected column for the Sequential RATE row.
  # (At n=200 the H3 fix will set p-value to NA because every rep drops
  # 1 fold, so hetero_detected propagates NA.)
  expect_true(is.na(out$hetero_detected[seq_row]))
  expect_false(is.nan(out$hetero_detected[seq_row]))
})

test_that("C2: print() output contains no <NA> and no NaN tokens", {
  cf <- make_small_null_cf_t()
  out <- suppressWarnings(suppressMessages(omni_hetero(cf)))
  txt <- capture.output(print(out))
  joined <- paste(txt, collapse = "\n")
  expect_false(grepl("<NA>", joined, fixed = TRUE))
  expect_false(grepl("NaN",  joined, fixed = TRUE))
})

# ------------------------------------------------------------------
# T-H2.2: Cross-fit High/Low row exists, label says cross-fit
# ------------------------------------------------------------------
test_that("H2: row label contains 'cross-fit', shape preserved", {
  cf <- make_small_null_cf_t(n = 200, num.trees = 100)
  out <- suppressWarnings(suppressMessages(omni_hetero(cf)))
  expect_equal(nrow(out), 5L)
  expect_true(any(grepl("cross.?fit", out$heterogeneity_test, ignore.case = TRUE)))
  hl_row <- grep("High vs", out$heterogeneity_test)
  expect_length(hl_row, 1)
  pv <- out$p_value[hl_row]
  expect_true(is.na(pv) || (pv >= 0 && pv <= 1))
})

# ------------------------------------------------------------------
# T-H3: Sequential RATE returns NA when any fold drops
# ------------------------------------------------------------------
test_that("H3: at n=200 (every rep drops 1 fold), Sequential RATE p-value is NA (not NaN)", {
  cf <- make_small_null_cf_t(n = 200, num.trees = 100)
  out <- suppressWarnings(suppressMessages(omni_hetero(cf)))
  seq_pval <- out$p_value[grep("Sequential RATE", out$heterogeneity_test)]
  expect_true(is.na(seq_pval))
  expect_false(is.nan(seq_pval))
})

# ------------------------------------------------------------------
# T-API: pre-existing tests + public API stable
# ------------------------------------------------------------------
test_that("T-API: omni_hetero formals unchanged", {
  fn <- names(formals(omni_hetero))
  expect_setequal(fn, c("c.forest", "seed", "min_fold_n"))
})

test_that("T-API: cf_loco formals unchanged", {
  fn <- names(formals(cf_loco))
  expect_setequal(fn,
                  c("c.forest", "variable.groups", "group.by.corr",
                    "corr.threshold", "normalize", "screen",
                    "stabilize", "seed"))
})
