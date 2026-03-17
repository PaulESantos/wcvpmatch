#' Check Default Backbone Setup
#'
#' Reports whether the optional companion package `wcvpdata` is available for
#' use as the default WCVP backbone and, if not, explains how to install it
#' from `r-universe`.
#'
#' @param inform Logical. If `TRUE` (default), print a short setup message.
#'
#' @return Invisibly returns a named list with setup status fields:
#'   `default_backbone_available`, `wcvpdata_installed`,
#'   `wcvpdata_has_backbone`, `wcvpdata_version`, `repository`, and
#'   `install_command`.
#'
#' @examples
#' library(wcvpmatch)
#' wcvp_setup_info()
#' @export
wcvp_setup_info <- function(inform = TRUE) {
  status <- .wcvpmatch_setup_status()

  if (isTRUE(inform)) {
    cli::cat_line(.wcvpmatch_setup_message(status, concise = FALSE))
  }

  invisible(status)
}

.wcvpmatch_setup_status <- function() {
  install_command <- paste0(
    "install.packages(",
    "'wcvpdata', repos = c(",
    "'https://paulesantos.r-universe.dev', ",
    "'https://cloud.r-project.org'))"
  )

  installed <- requireNamespace("wcvpdata", quietly = TRUE)
  version <- if (installed) .wcvpmatch_package_version("wcvpdata") else NA_character_
  has_backbone <- installed && .wcvpmatch_has_backbone_dataset("wcvpdata")

  list(
    default_backbone_available = installed && has_backbone,
    wcvpdata_installed = installed,
    wcvpdata_has_backbone = has_backbone,
    wcvpdata_version = version,
    repository = "https://paulesantos.r-universe.dev",
    install_command = install_command
  )
}

.wcvpmatch_has_backbone_dataset <- function(pkg) {
  items <- tryCatch(
    utils::data(package = pkg)$results[, "Item"],
    error = function(e) character()
  )
  "wcvp_checklist_names" %in% items
}

.wcvpmatch_setup_header <- function() {
  cli::rule(
    left = cli::style_bold("Default WCVP Backbone"),
    right = paste0("wcvpmatch ", utils::packageVersion("wcvpmatch"))
  )
}

.wcvpmatch_package_version <- function(pkg) {
  as.character(utils::packageVersion(pkg))
}

.wcvpmatch_setup_lines <- function(status) {
  label_width <- max(nchar(c("wcvpdata", "backbone", "repository")))
  version <- if (isTRUE(status$wcvpdata_installed)) status$wcvpdata_version else "not installed"

  package_line <- paste0(
    if (isTRUE(status$wcvpdata_installed)) cli::col_green(cli::symbol$tick) else cli::col_yellow("!"),
    " ",
    cli::col_blue(format("wcvpdata", width = label_width)),
    " ",
    cli::ansi_align(version, width = max(nchar(version), 13))
  )

  backbone_state <- if (isTRUE(status$default_backbone_available)) {
    cli::col_green("available")
  } else if (isTRUE(status$wcvpdata_installed)) {
    cli::col_yellow("installed but unavailable")
  } else {
    cli::col_yellow("not available")
  }

  backbone_line <- paste0(
    if (isTRUE(status$default_backbone_available)) cli::col_green(cli::symbol$tick) else cli::col_yellow("!"),
    " ",
    cli::col_blue(format("backbone", width = label_width)),
    " ",
    backbone_state
  )

  repository_line <- paste0(
    cli::col_blue("i"),
    " ",
    cli::col_blue(format("repository", width = label_width)),
    " ",
    status$repository
  )

  c(package_line, backbone_line, repository_line)
}

.wcvpmatch_inline <- function(text) {
  cli::format_inline(text)
}

.wcvpmatch_setup_footer <- function(status) {
  if (isTRUE(status$default_backbone_available)) {
    return(c(
      .wcvpmatch_inline("{.blue i} Functions can use the default backbone when {.code target_df = NULL}.")
    ))
  }

  if (isTRUE(status$wcvpdata_installed)) {
    return(c(
      .wcvpmatch_inline("{.yellow !} {.pkg wcvpdata} is installed, but {.val wcvp_checklist_names} was not found."),
      .wcvpmatch_inline(paste0("{.blue i} Reinstall {.pkg wcvpdata} from {.url ", status$repository, "}.")),
      .wcvpmatch_inline("{.blue i} Or pass a backbone explicitly with {.arg target_df}.")
    ))
  }

  c(
    .wcvpmatch_inline(paste0("{.blue i} Install {.pkg wcvpdata} with {.code ", status$install_command, "}.")),
    .wcvpmatch_inline("{.blue i} Or pass a backbone explicitly with {.arg target_df}.")
  )
}

.wcvpmatch_startup_footer <- function(status) {
  .wcvpmatch_inline("{.blue i} Run {.code wcvp_setup_info()} to check the default WCVP backbone.")
}

.wcvpmatch_setup_message <- function(status, concise = FALSE) {
  footer <- if (isTRUE(concise)) .wcvpmatch_startup_footer(status) else .wcvpmatch_setup_footer(status)
  c(
    .wcvpmatch_setup_header(),
    .wcvpmatch_setup_lines(status),
    footer
  )
}

.wcvpmatch_startup_message <- function() {
  c(
    cli::rule(
      left = cli::style_bold("Loading wcvpmatch"),
      right = paste0("wcvpmatch ", utils::packageVersion("wcvpmatch"))
    ),
    .wcvpmatch_startup_footer(NULL)
  )
}
