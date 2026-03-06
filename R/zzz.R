.pkgenv <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  .pkgenv[["rwcvpdata_available"]] <- requireNamespace("rWCVPdata", quietly = TRUE)
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(.wvcpmatch_attach_msg())
}

.wvcpmatch_attach_msg <- function() {
  header <- cli::rule(
    left = cli::style_bold("Attaching wvcpmatch ecosystem"),
    right = paste0("wvcpmatch ", as.character(utils::packageVersion("wvcpmatch")))
  )

  if (isTRUE(.pkgenv$rwcvpdata_available)) {
    details <- c(
      paste0(
        cli::col_green(cli::symbol$tick), " ",
        cli::col_blue("rWCVPdata "),
        as.character(utils::packageVersion("rWCVPdata")),
        " ",
        cli::col_grey("(wcvp_names available)")
      )
    )
  } else {
    details <- c(
      paste0(cli::col_red(cli::symbol$cross), " ", cli::style_bold("rWCVPdata"), " not installed."),
      paste0(cli::col_green(cli::symbol$tick), " Install: ",
             cli::col_blue("remotes::install_github('PaulESantos/rWCVPdata')")),
      paste0(cli::col_green(cli::symbol$tick), " Then run ", cli::col_blue("wcvp_matching()"),
             " with ", cli::col_blue("rWCVPdata::wcvp_names"), ".")
    )
  }

  paste(c(header, details), collapse = "\n")
}

.require_rwcvpdata <- function() {
  ok <- requireNamespace("rWCVPdata", quietly = TRUE)
  .pkgenv[["rwcvpdata_available"]] <- ok
  if (ok) return(invisible(TRUE))

  stop(
    paste(
      "The package 'rWCVPdata' is required to use WCVP names.",
      "Install it with:",
      "remotes::install_github('PaulESantos/rWCVPdata')",
      sep = "\n"
    ),
    call. = FALSE
  )
}
