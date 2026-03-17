test_that("wcvp_setup_info returns a stable status object", {
  expect_no_error(
    status <- wcvp_setup_info(inform = FALSE)
  )

  expect_true(is.list(status))
  expect_true(all(c(
    "default_backbone_available",
    "wcvpdata_installed",
    "wcvpdata_has_backbone",
    "wcvpdata_version",
    "repository",
    "install_command"
  ) %in% names(status)))
  expect_true(is.logical(status$default_backbone_available))
  expect_true(is.logical(status$wcvpdata_installed))
  expect_true(is.logical(status$wcvpdata_has_backbone))
  expect_true(is.character(status$wcvpdata_version) || is.na(status$wcvpdata_version))
  expect_true(is.character(status$repository))
  expect_true(is.character(status$install_command))
  expect_match(status$repository, "paulesantos[.]r-universe[.]dev")
  expect_match(status$install_command, "install[.]packages")
})
