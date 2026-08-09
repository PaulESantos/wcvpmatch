skip_if_no_default_backbone <- function() {
  testthat::skip_if_not_installed("wcvpdata")

  has_accessor <- .wcvpmatch_has_backbone_dataset("wcvpdata")

  if (!has_accessor) {
    testthat::skip("wcvpdata is installed but does not export wcvp_matching_names().")
  }
}
