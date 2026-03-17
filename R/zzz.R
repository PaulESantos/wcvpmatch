.wcvpmatch_cache <- new.env(parent = emptyenv())

.onAttach <- function(libname, pkgname) {
  if (!interactive()) {
    return(invisible())
  }

  if (isTRUE(getOption("wcvpmatch.quiet", FALSE))) {
    return(invisible())
  }

  packageStartupMessage(
    paste(.wcvpmatch_startup_message(), collapse = "\n")
  )
}

.require_wcvpdata <- function() {
  ok <- requireNamespace("wcvpdata", quietly = TRUE)
  if (ok) return(invisible(TRUE))

  cli::cli_abort(c(
    "x" = "No default WCVP backbone is available in this session.",
    "i" = "Install {.pkg wcvpdata} with {.code install.packages('wcvpdata', repos = c('https://paulesantos.r-universe.dev', 'https://cloud.r-project.org'))} to enable the default backbone.",
    "i" = "Or pass a backbone explicitly with {.arg target_df}."
  ))
}
