#' Set up the TABPFN_TOKEN environment variable
#'
#' Interactive helper that checks for `TABPFN_TOKEN`, walks the user
#' through obtaining one from <https://ux.priorlabs.ai/account>, and
#' optionally persists it to `~/.Renviron` for future R sessions.
#'
#' Behaviour:
#' \itemize{
#'   \item If `TABPFN_TOKEN` is already set and `overwrite = FALSE`,
#'     the function reports the length of the existing token and
#'     returns `invisible(TRUE)` without prompting.
#'   \item If the session is non-interactive and the token is missing,
#'     the function `stop()`s with a pointer to
#'     <https://ux.priorlabs.ai/account>.
#'   \item Otherwise the function prints short instructions, optionally
#'     opens a browser, prompts for the token (using
#'     `askpass::askpass()` if available), validates it, sets the
#'     environment variable for the current session, and optionally
#'     appends it to `~/.Renviron`.
#' }
#'
#' Token validation rejects empty tokens, tokens containing whitespace,
#' and tokens shorter than 11 characters. A rejected token returns
#' `invisible(FALSE)` and does not change `Sys.getenv("TABPFN_TOKEN")`.
#'
#' @param token Optional character scalar. If supplied, skips the
#'   prompt and uses this value directly (still validated and still
#'   subject to `persist`).
#' @param persist Optional logical. If `TRUE`, append the token to
#'   `renviron_path` (creating the file if absent). If `FALSE`, set
#'   only for the current session. If `NULL` (default), the user is
#'   asked interactively.
#' @param overwrite Logical. If `FALSE` (default), an existing
#'   `TABPFN_TOKEN` short-circuits the function. If `TRUE`, the
#'   function proceeds even when a token is already set.
#' @param renviron_path Path to the `.Renviron` file used when
#'   `persist = TRUE`. Defaults to `path.expand("~/.Renviron")`. Exposed
#'   primarily for testing.
#'
#' @return Invisibly, `TRUE` on success and `FALSE` if the user
#'   provided an invalid token. May also `stop()` in non-interactive
#'   sessions when the token is missing.
#'
#' @export
#' @examples
#' \dontrun{
#' # Check first, prompt only if missing:
#' setup_tabpfn_token()
#'
#' # Force re-entry even if a token is already set:
#' setup_tabpfn_token(overwrite = TRUE)
#'
#' # Non-interactive use (e.g. in a script with a token from a vault):
#' setup_tabpfn_token(token = Sys.getenv("MY_VAULT_TOKEN"),
#'                    persist = FALSE)
#' }
setup_tabpfn_token <- function(token = NULL,
                               persist = NULL,
                               overwrite = FALSE,
                               renviron_path = path.expand("~/.Renviron")) {

  current <- Sys.getenv("TABPFN_TOKEN", unset = "")

  # 1. Already set and not overwriting -> short-circuit.
  if (nzchar(current) && !isTRUE(overwrite)) {
    n <- nchar(current)
    message("TABPFN_TOKEN already set (", n, " chars).")
    return(invisible(TRUE))
  }

  # 2. Missing and non-interactive (and no token supplied) -> error.
  if (is.null(token) && !interactive()) {
    stop(
      "TABPFN_TOKEN env var not set and session is non-interactive. ",
      "Register at https://ux.priorlabs.ai, accept the license, copy ",
      "your API key from https://ux.priorlabs.ai/account, then set ",
      "TABPFN_TOKEN in your environment (e.g. via ~/.Renviron) or call ",
      "setup_tabpfn_token() interactively.",
      call. = FALSE
    )
  }

  # 3a. If the caller supplied a token directly, skip prompts.
  if (!is.null(token)) {
    if (!.is_valid_tabpfn_token(token)) {
      message("Provided token is invalid (empty, contains whitespace, ",
              "or shorter than 11 chars). TABPFN_TOKEN was not changed.")
      return(invisible(FALSE))
    }
  } else {
    # 3b. Interactive flow: instructions + optional browser + prompt.
    message("Set up TABPFN_TOKEN:\n",
            "  - Register/login at https://ux.priorlabs.ai\n",
            "  - Accept the TabPFN license\n",
            "  - Copy your API key from https://ux.priorlabs.ai/account")

    open_ans <- tolower(trimws(readline(
      "Open browser to get token now? (y/n): "
    )))
    if (open_ans %in% c("y", "yes")) {
      try(utils::browseURL("https://ux.priorlabs.ai/account"),
          silent = TRUE)
    }

    token <- .prompt_tabpfn_token()

    if (!.is_valid_tabpfn_token(token)) {
      message("Token is invalid (empty, contains whitespace, or shorter ",
              "than 11 chars). TABPFN_TOKEN was not changed.")
      return(invisible(FALSE))
    }
  }

  # 4. Set for current session.
  Sys.setenv(TABPFN_TOKEN = token)

  # 5. Decide whether to persist.
  if (is.null(persist)) {
    persist_ans <- tolower(trimws(readline(
      "Persist to ~/.Renviron for future sessions? (y/n): "
    )))
    persist <- persist_ans %in% c("y", "yes")
  }

  if (isTRUE(persist)) {
    overwrite_existing <- NULL
    if (file.exists(renviron_path) &&
        .renviron_has_tabpfn_token(renviron_path)) {
      ans <- tolower(trimws(readline(
        "Existing TABPFN_TOKEN line found in .Renviron. Overwrite? (y/n): "
      )))
      overwrite_existing <- ans %in% c("y", "yes")
      if (!isTRUE(overwrite_existing)) {
        message("Kept existing line in ", renviron_path,
                ". TABPFN_TOKEN is set for this session only.")
        return(invisible(TRUE))
      }
    }

    ok <- .write_token_to_renviron(
      path = renviron_path,
      token = token,
      overwrite = isTRUE(overwrite_existing) || isTRUE(overwrite)
    )

    if (isTRUE(ok)) {
      message("Wrote TABPFN_TOKEN to ", renviron_path,
              ". Restart R to pick it up automatically in future sessions.")
    } else {
      message("Could not persist token to ", renviron_path,
              ". TABPFN_TOKEN is set for this session only.")
    }
  } else {
    message("Set TABPFN_TOKEN for this session only.")
  }

  invisible(TRUE)
}


# -- helpers -----------------------------------------------------------------

#' @keywords internal
#' @noRd
.is_valid_tabpfn_token <- function(token) {
  if (is.null(token) || !is.character(token) || length(token) != 1L) {
    return(FALSE)
  }
  if (is.na(token)) return(FALSE)
  if (!nzchar(token)) return(FALSE)
  if (grepl("\\s", token)) return(FALSE)
  if (nchar(token) <= 10L) return(FALSE)
  TRUE
}

#' @keywords internal
#' @noRd
.prompt_tabpfn_token <- function() {
  if (requireNamespace("askpass", quietly = TRUE)) {
    tok <- tryCatch(
      askpass::askpass("Paste TABPFN_TOKEN: "),
      error = function(e) NULL
    )
    if (is.null(tok)) tok <- ""
  } else {
    tok <- readline("Paste TABPFN_TOKEN: ")
  }
  trimws(as.character(tok))
}

#' @keywords internal
#' @noRd
.renviron_has_tabpfn_token <- function(path) {
  if (!file.exists(path)) return(FALSE)
  lines <- tryCatch(readLines(path, warn = FALSE),
                    error = function(e) character())
  any(grepl("^\\s*TABPFN_TOKEN\\s*=", lines))
}

#' @keywords internal
#' @noRd
#' @description
#' Append (or overwrite) the `TABPFN_TOKEN=...` line in an `.Renviron`
#' file at `path`. Returns `TRUE` on success and `FALSE` on any I/O
#' failure. When `overwrite = TRUE` and a line is already present, the
#' existing line is replaced; otherwise a new line is appended. When
#' `overwrite = FALSE` and a line exists, the file is left unchanged
#' and `FALSE` is returned to signal "did nothing".
.write_token_to_renviron <- function(path, token, overwrite = FALSE) {
  if (!.is_valid_tabpfn_token(token)) return(FALSE)

  new_line <- paste0("TABPFN_TOKEN=", token)

  if (!file.exists(path)) {
    ok <- tryCatch({
      dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
      con <- file(path, open = "w")
      on.exit(close(con), add = TRUE)
      writeLines(new_line, con = con)
      TRUE
    }, error = function(e) FALSE)
    return(ok)
  }

  lines <- tryCatch(readLines(path, warn = FALSE),
                    error = function(e) NULL)
  if (is.null(lines)) return(FALSE)

  has_line <- grepl("^\\s*TABPFN_TOKEN\\s*=", lines)
  if (any(has_line)) {
    if (!isTRUE(overwrite)) return(FALSE)
    lines[has_line] <- new_line
  } else {
    lines <- c(lines, new_line)
  }

  ok <- tryCatch({
    con <- file(path, open = "w")
    on.exit(close(con), add = TRUE)
    writeLines(lines, con = con)
    TRUE
  }, error = function(e) FALSE)

  ok
}
