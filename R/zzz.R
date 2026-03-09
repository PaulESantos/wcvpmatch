.pkgenv <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  .pkgenv[["wcvpdata_available"]] <- requireNamespace("wcvpdata", quietly = TRUE)
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(.wvcpmatch_attach_msg())
}

.wvcpmatch_attach_msg <- function() {
  header <- cli::rule(
    left = cli::style_bold("Attaching wcvpmatch ecosystem"),
    right = paste0("wcvpmatch ", as.character(utils::packageVersion("wcvpmatch")))
  )

  if (isTRUE(.pkgenv$wcvpdata_available)) {
    details <- c(
      paste0(
        cli::col_green(cli::symbol$tick), " ",
        cli::col_blue("wcvpdata "),
        as.character(utils::packageVersion("wcvpdata")),
        " ",
        cli::col_grey("(wcvp_checklist_names available)")
      )
    )
  } else {
    details <- c(
      paste0(cli::col_red(cli::symbol$cross), " ", cli::style_bold("wcvpdata"), " not installed."),
      paste0(cli::col_green(cli::symbol$tick), " Install: ",
             cli::col_blue("remotes::install_github('matildesrib/wcvpdata')")),
      paste0(cli::col_green(cli::symbol$tick), " Then run ", cli::col_blue("wcvp_matching()"),
             " with ", cli::col_blue("wcvpdata::wcvp_checklist_names"), ".")
    )
  }

  paste(c(header, details), collapse = "\n")
}

.require_wcvpdata <- function() {
  ok <- requireNamespace("wcvpdata", quietly = TRUE)
  .pkgenv[["wcvpdata_available"]] <- ok
  if (ok) return(invisible(TRUE))

  stop(
    paste(
      "The package 'wcvpdata' is required to use WCVP names.",
      "Install it with:",
      "remotes::install_github('matildesrib/wcvpdata')",
      sep = "\n"
    ),
    call. = FALSE
  )
}
