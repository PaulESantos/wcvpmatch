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
#'   `wcvpdata_has_backbone`, `repository`, and `install_command`.
#'
#' @examples
#' library(wcvpmatch)
#' wcvp_setup_info()
#' @export
wcvp_setup_info <- function(inform = TRUE) {
  install_command <- paste0(
    "install.packages(",
    "'wcvpdata', repos = c(",
    "'https://paulesantos.r-universe.dev', ",
    "'https://cloud.r-project.org'))"
  )

  installed <- requireNamespace("wcvpdata", quietly = TRUE)
  has_backbone <- installed && exists(
    "wcvp_checklist_names",
    envir = asNamespace("wcvpdata"),
    inherits = FALSE
  )

  status <- list(
    default_backbone_available = installed && has_backbone,
    wcvpdata_installed = installed,
    wcvpdata_has_backbone = has_backbone,
    repository = "https://paulesantos.r-universe.dev",
    install_command = install_command
  )

  if (isTRUE(inform)) {
    if (isTRUE(status$default_backbone_available)) {
      cli::cli_inform(c(
        "v" = "The default WCVP backbone is available through {.pkg wcvpdata}.",
        "i" = "Functions can use the default backbone when {.arg target_df = NULL}."
      ))
    } else if (isTRUE(status$wcvpdata_installed)) {
      cli::cli_warn(c(
        "!" = "{.pkg wcvpdata} is installed, but the expected object {.val wcvp_checklist_names} was not found.",
        "i" = "Reinstall {.pkg wcvpdata} from {.url https://paulesantos.r-universe.dev}.",
        "i" = "Or pass a backbone explicitly with {.arg target_df}."
      ))
    } else {
      cli::cli_inform(c(
        "i" = "The default WCVP backbone is not available in this session.",
        "i" = paste0("Install {.pkg wcvpdata} with {.code ", install_command, "}."),
        "i" = "Or pass a backbone explicitly with {.arg target_df}."
      ))
    }
  }

  invisible(status)
}
