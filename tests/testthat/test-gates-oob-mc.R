library(testthat)
library(grf)
library(UtopiaPlanitia)

run_slow <- nzchar(Sys.getenv("UTOPIA_RUN_SLOW_TESTS"))

test_that("GATES MC: OOB null rejection not wildly anti-conservative", {
  skip_on_cran()
  skip_on_ci()
  if (!run_slow) skip("slow; set UTOPIA_RUN_SLOW_TESTS=1 to run")

  mc_file <- normalizePath(
    file.path(testthat::test_path(), "..", "..", "tools", "gates_oob_mc.R"),
    mustWork = TRUE
  )
  sys.source(mc_file, envir = environment())
  res <- run_gates_mc(REPS = 30L, N = 250L, NTREES = 120L, ALPHA = 0.10)

  expect_lt(res$null_reject_oob, 0.25)
  expect_lt(res$null_reject_cf, 0.25)
})
