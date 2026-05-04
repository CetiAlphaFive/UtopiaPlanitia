# Set up the TABPFN_TOKEN environment variable

Interactive helper that checks for `TABPFN_TOKEN`, walks the user
through obtaining one from <https://ux.priorlabs.ai/account>, and
optionally persists it to `~/.Renviron` for future R sessions.

## Usage

``` r
setup_tabpfn_token(
  token = NULL,
  persist = NULL,
  overwrite = FALSE,
  renviron_path = path.expand("~/.Renviron")
)
```

## Arguments

- token:

  Optional character scalar. If supplied, skips the prompt and uses this
  value directly (still validated and still subject to `persist`).

- persist:

  Optional logical. If `TRUE`, append the token to `renviron_path`
  (creating the file if absent). If `FALSE`, set only for the current
  session. If `NULL` (default), the user is asked interactively.

- overwrite:

  Logical. If `FALSE` (default), an existing `TABPFN_TOKEN`
  short-circuits the function. If `TRUE`, the function proceeds even
  when a token is already set.

- renviron_path:

  Path to the `.Renviron` file used when `persist = TRUE`. Defaults to
  `path.expand("~/.Renviron")`. Exposed primarily for testing.

## Value

Invisibly, `TRUE` on success and `FALSE` if the user provided an invalid
token. May also [`stop()`](https://rdrr.io/r/base/stop.html) in
non-interactive sessions when the token is missing.

## Details

Behaviour:

- If `TABPFN_TOKEN` is already set and `overwrite = FALSE`, the function
  reports the length of the existing token and returns `invisible(TRUE)`
  without prompting.

- If the session is non-interactive and the token is missing, the
  function [`stop()`](https://rdrr.io/r/base/stop.html)s with a pointer
  to <https://ux.priorlabs.ai/account>.

- Otherwise the function prints short instructions, optionally opens a
  browser, prompts for the token (using
  [`askpass::askpass()`](https://r-lib.r-universe.dev/askpass/reference/askpass.html)
  if available), validates it, sets the environment variable for the
  current session, and optionally appends it to `~/.Renviron`.

Token validation rejects empty tokens, tokens containing whitespace, and
tokens shorter than 11 characters. A rejected token returns
`invisible(FALSE)` and does not change `Sys.getenv("TABPFN_TOKEN")`.

## Examples

``` r
if (FALSE) { # \dontrun{
# Check first, prompt only if missing:
setup_tabpfn_token()

# Force re-entry even if a token is already set:
setup_tabpfn_token(overwrite = TRUE)

# Non-interactive use (e.g. in a script with a token from a vault):
setup_tabpfn_token(token = Sys.getenv("MY_VAULT_TOKEN"),
                   persist = FALSE)
} # }
```
