test_that("selective loading agrees with the complete default backbone", {
  skip_if_no_default_backbone()
  dataset <- .default_selective_dataset()
  skip_if(is.null(dataset), "The installed backbone lacks matching schema v1")
  full <- get_db()
  acer <- full[full$genus == "Acer", , drop = FALSE]
  cross <- acer[!is.na(acer$accepted_plant_name_id) &
    !acer$accepted_plant_name_id %in% acer$plant_name_id &
    .is_species_record(acer), , drop = FALSE]
  expect_gt(nrow(cross), 0)
  exemplar <- cross[1, ]
  input <- tibble::tibble(Genus = c(as.character(exemplar$genus), "Acer", "Fakexyz"),
    Species = c(as.character(exemplar$species), "rubrun", "fictitia"))
  expected <- suppressWarnings(wcvp_matching(input, target_df = full, output = "full"))
  actual <- suppressWarnings(wcvp_matching(input, output = "full"))
  expect_identical(actual, expected)
  expect_false(is.na(actual$accepted_taxon_name[1]))
  selective <- .default_selective_target(check_df_format(input), dataset, 1, "osa")
  expect_lt(nrow(selective), nrow(full) / 100)
  expect_false(identical(attr(selective, "wcvpmatch_source"), "default"))
})

test_that("unknown schema falls back to the complete backbone", {
  skip_if_no_default_backbone()
  dataset <- .default_selective_dataset()
  skip_if(is.null(dataset), "The installed backbone lacks matching schema v1")
  local_mocked_bindings(.default_selective_dataset = function() NULL)
  input <- tibble::tibble(Genus = "Acer", Species = "rubrum")
  out <- wcvp_matching(input, profile = TRUE)
  expect_true("get_db" %in% attr(out, "timings")$stage)
  expect_false("selective_backbone" %in% attr(out, "timings")$stage)
})
