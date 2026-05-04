# Tests for setup_tabpfn_token() and its file-write helper.
# These tests must NEVER touch the real ~/.Renviron — every test that
# could write to disk uses withr::local_tempfile() and overrides
# `renviron_path`. Tests that exercise the prompt branches are skipped
# unless we can drive them without an interactive console; we test the
# extracted helper .write_token_to_renviron() directly instead.

test_that("token = arg + persist = FALSE sets env var without prompting", {
  skip_if_not_installed("withr")
  withr::with_envvar(c(TABPFN_TOKEN = ""), {
    tmp <- withr::local_tempfile()
    res <- setup_tabpfn_token(token = "abcdef1234567890",
                              persist = FALSE,
                              renviron_path = tmp)
    expect_true(res)
    expect_identical(Sys.getenv("TABPFN_TOKEN"), "abcdef1234567890")
    # persist = FALSE -> no file should be created
    expect_false(file.exists(tmp))
  })
})

test_that("already-set token short-circuits with invisible(TRUE)", {
  skip_if_not_installed("withr")
  withr::with_envvar(c(TABPFN_TOKEN = "preexistingtokenXXX"), {
    res <- expect_invisible(setup_tabpfn_token())
    expect_true(res)
    # env var unchanged
    expect_identical(Sys.getenv("TABPFN_TOKEN"), "preexistingtokenXXX")
  })
})

test_that("non-interactive + missing token errors", {
  skip_if_not_installed("withr")
  # interactive() is FALSE under R CMD check / testthat by default,
  # so we just need TABPFN_TOKEN cleared and no `token` argument.
  withr::with_envvar(c(TABPFN_TOKEN = ""), {
    expect_error(
      setup_tabpfn_token(),
      regexp = "TABPFN_TOKEN"
    )
  })
})

test_that("empty / whitespace / short tokens are rejected via token = arg", {
  skip_if_not_installed("withr")
  withr::with_envvar(c(TABPFN_TOKEN = ""), {
    tmp <- withr::local_tempfile()

    # empty
    expect_false(setup_tabpfn_token(token = "",
                                    persist = FALSE,
                                    renviron_path = tmp))
    expect_identical(Sys.getenv("TABPFN_TOKEN"), "")

    # contains whitespace
    expect_false(setup_tabpfn_token(token = "abcdef 1234567890",
                                    persist = FALSE,
                                    renviron_path = tmp))
    expect_identical(Sys.getenv("TABPFN_TOKEN"), "")

    # too short (<=10 chars)
    expect_false(setup_tabpfn_token(token = "short",
                                    persist = FALSE,
                                    renviron_path = tmp))
    expect_identical(Sys.getenv("TABPFN_TOKEN"), "")
  })
})

test_that("persist = TRUE writes token to renviron_path", {
  skip_if_not_installed("withr")
  withr::with_envvar(c(TABPFN_TOKEN = ""), {
    tmp <- withr::local_tempfile()
    res <- setup_tabpfn_token(token = "abcdef1234567890",
                              persist = TRUE,
                              renviron_path = tmp)
    expect_true(res)
    expect_true(file.exists(tmp))
    lines <- readLines(tmp)
    expect_true(any(grepl("^TABPFN_TOKEN=abcdef1234567890$", lines)))
  })
})

test_that(".write_token_to_renviron creates file when missing", {
  skip_if_not_installed("withr")
  tmp <- withr::local_tempfile()
  expect_false(file.exists(tmp))
  ok <- UtopiaPlanitia:::.write_token_to_renviron(
    path = tmp,
    token = "abcdef1234567890",
    overwrite = FALSE
  )
  expect_true(ok)
  expect_true(file.exists(tmp))
  expect_identical(readLines(tmp), "TABPFN_TOKEN=abcdef1234567890")
})

test_that(".write_token_to_renviron appends when no existing line", {
  skip_if_not_installed("withr")
  tmp <- withr::local_tempfile()
  writeLines(c("FOO=bar", "BAZ=qux"), tmp)
  ok <- UtopiaPlanitia:::.write_token_to_renviron(
    path = tmp,
    token = "abcdef1234567890",
    overwrite = FALSE
  )
  expect_true(ok)
  lines <- readLines(tmp)
  expect_identical(lines,
                   c("FOO=bar", "BAZ=qux", "TABPFN_TOKEN=abcdef1234567890"))
})

test_that(".write_token_to_renviron refuses to overwrite when overwrite = FALSE", {
  skip_if_not_installed("withr")
  tmp <- withr::local_tempfile()
  writeLines(c("TABPFN_TOKEN=oldtokenoldtoken"), tmp)
  ok <- UtopiaPlanitia:::.write_token_to_renviron(
    path = tmp,
    token = "abcdef1234567890",
    overwrite = FALSE
  )
  expect_false(ok)
  # file unchanged
  expect_identical(readLines(tmp), "TABPFN_TOKEN=oldtokenoldtoken")
})

test_that(".write_token_to_renviron replaces line when overwrite = TRUE", {
  skip_if_not_installed("withr")
  tmp <- withr::local_tempfile()
  writeLines(c("FOO=bar",
               "TABPFN_TOKEN=oldtokenoldtoken",
               "BAZ=qux"), tmp)
  ok <- UtopiaPlanitia:::.write_token_to_renviron(
    path = tmp,
    token = "abcdef1234567890",
    overwrite = TRUE
  )
  expect_true(ok)
  lines <- readLines(tmp)
  expect_identical(lines,
                   c("FOO=bar",
                     "TABPFN_TOKEN=abcdef1234567890",
                     "BAZ=qux"))
})

test_that(".write_token_to_renviron rejects invalid tokens", {
  skip_if_not_installed("withr")
  tmp <- withr::local_tempfile()
  expect_false(UtopiaPlanitia:::.write_token_to_renviron(tmp, "", FALSE))
  expect_false(UtopiaPlanitia:::.write_token_to_renviron(tmp, "short", FALSE))
  expect_false(UtopiaPlanitia:::.write_token_to_renviron(
    tmp, "has whitespace here too", FALSE))
  expect_false(file.exists(tmp))
})
