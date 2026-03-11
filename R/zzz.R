.pkgenv <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  .pkgenv[["wcvpdata_available"]] <- requireNamespace("wcvpdata", quietly = TRUE)
}

.require_wcvpdata <- function() {
  ok <- requireNamespace("wcvpdata", quietly = TRUE)
  .pkgenv[["wcvpdata_available"]] <- ok
  if (ok) return(invisible(TRUE))

  cli::cli_abort(c(
    "x" = "No default WCVP backbone is available in this session.",
    "i" = "Pass a backbone explicitly with {.arg target_df}.",
    "i" = "Or install the optional companion package {.pkg wcvpdata} to enable the default backbone."
  ))
}
