.onAttach <- function(libname, pkgname) {
  if (requireNamespace("tabpfn", quietly = TRUE) &&
      !nzchar(Sys.getenv("TABPFN_TOKEN", unset = ""))) {
    packageStartupMessage(
      "UtopiaPlanitia: tabpfn installed but TABPFN_TOKEN not set.\n",
      "Run setup_tabpfn_token() to enable tabcf()."
    )
  }
}
