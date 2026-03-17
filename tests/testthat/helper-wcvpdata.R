skip_if_no_default_backbone <- function() {
  testthat::skip_if_not_installed("wcvpdata")

  has_object <- .wcvpmatch_has_backbone_dataset("wcvpdata")

  if (!has_object) {
    testthat::skip("wcvpdata is installed but does not export wcvp_checklist_names.")
  }
}
