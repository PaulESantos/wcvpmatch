make_distribution_names <- function() {
  tibble::tibble(
    plant_name_id = c(1, 2, 3, 4, 5, 6),
    accepted_plant_name_id = c(NA, 3, NA, NA, 1, NA),
    taxon_rank = c("Species", "Species", "Species", "Species", "Species", "Species"),
    taxon_status = c("Accepted", "Synonym", "Accepted", "Accepted", "Synonym", "Accepted"),
    family = c("Cactaceae", "Cactaceae", "Cactaceae", "Fagaceae", "Cactaceae", "Cactaceae"),
    genus = c("Opuntia", "Nopalea", "Opuntia", "Quercus", "Opuntia", "Mammillaria"),
    species = c("ficus-indica", "cochenillifera", "cochenillifera", "robur", "tuna", "elongata"),
    taxon_name = c(
      "Opuntia ficus-indica",
      "Nopalea cochenillifera",
      "Opuntia cochenillifera",
      "Quercus robur",
      "Opuntia tuna",
      "Mammillaria elongata"
    )
  )
}

make_distribution_records <- function() {
  tibble::tibble(
    plant_locality_id = 1:7,
    plant_name_id = c(1, 2, 3, 3, 4, 5, 6),
    continent_code_l1 = c("8", "8", "8", "4", "1", "8", "8"),
    continent = c(
      "SOUTHERN AMERICA",
      "SOUTHERN AMERICA",
      "SOUTHERN AMERICA",
      "NORTHERN AMERICA",
      "EUROPE",
      "SOUTHERN AMERICA",
      "SOUTHERN AMERICA"
    ),
    region_code_l2 = c("83", "83", "83", "41", "10", "85", "83"),
    region = c(
      "Western South America",
      "Western South America",
      "Western South America",
      "Mexico",
      "Europe",
      "Southern South America",
      "Western South America"
    ),
    area_code_l3 = c("MEX", "PER", "COL", "MEX", "ESP", "GAL", "MEX"),
    area = c("Mexico", "Peru", "Colombia", "Mexico", "Spain", "Galapagos", "Mexico"),
    introduced = c(0, 0, 0, 1, 0, 0, 0),
    extinct = c(0, 0, 0, 0, 0, 0, 0),
    location_doubtful = c(0, 0, 0, 0, 0, 0, 0)
  )
}

test_that("wcvp_distribution uses the species matching backend and resolves accepted ids", {
  out <- wcvp_distribution(
    c("Nopalea cochenilliferaa", "Taxon inexistente"),
    taxon_rank = "species",
    wcvp_names = make_distribution_names(),
    wcvp_distributions = make_distribution_records()
  )

  expect_true(all(c("submited_name", "accepted_taxon_name") %in% names(out)))

  matched_out <- dplyr::filter(out, submited_name == "Nopalea cochenilliferaa")
  unmatched_out <- dplyr::filter(out, submited_name == "Taxon inexistente")

  expect_true(all(matched_out$distribution_status == "distribution_found"))
  expect_equal(unique(matched_out$matched), TRUE)
  expect_equal(unique(matched_out$accepted_taxon_name), "Opuntia cochenillifera")
  expect_setequal(matched_out$area_code_l3, c("COL", "MEX"))
  expect_false("PER" %in% matched_out$area_code_l3)

  expect_equal(nrow(unmatched_out), 1)
  expect_equal(unmatched_out$distribution_status, "no_match")
  expect_false(unmatched_out$matched)
  expect_true(is.na(unmatched_out$area_code_l3))
})

test_that("wcvp_distribution aggregates accepted names at genus level", {
  out <- wcvp_distribution(
    "Opuntia",
    taxon_rank = "genus",
    introduced = FALSE,
    wcvp_names = make_distribution_names(),
    wcvp_distributions = make_distribution_records()
  )

  expect_equal(unique(out$matched_taxon), "Opuntia")
  expect_setequal(out$area_code_l3, c("COL", "MEX"))
  expect_false("GAL" %in% out$area_code_l3)
  expect_true(all(out$distribution_status == "distribution_found"))
  expect_true(all(out$introduced == FALSE))
})

test_that("wcvp_distribution supports fuzzy family matching with fozziejoin", {
  out <- wcvp_distribution(
    "Cactacee",
    taxon_rank = "family",
    max_dist = 1,
    wcvp_names = make_distribution_names(),
    wcvp_distributions = make_distribution_records()
  )

  expect_equal(unique(out$matched_taxon), "Cactaceae")
  expect_equal(unique(out$match_distance), 1)
  expect_setequal(out$area_code_l3, c("COL", "MEX"))
})

test_that("wcvp_distribution keeps matched taxa without distribution rows", {
  out <- wcvp_distribution(
    "Quercus robur",
    taxon_rank = "species",
    introduced = FALSE,
    wcvp_names = make_distribution_names(),
    wcvp_distributions = dplyr::filter(make_distribution_records(), plant_name_id != 4)
  )

  expect_equal(nrow(out), 1)
  expect_equal(out$distribution_status, "no_distribution")
  expect_true(out$matched)
  expect_true(is.na(out$area_code_l3))
})

test_that("wcvp_distribution can summarise output to one row per input", {
  out <- wcvp_distribution(
    c("Nopalea cochenilliferaa", "Taxon inexistente"),
    taxon_rank = "species",
    summarise_by_input = TRUE,
    wcvp_names = make_distribution_names(),
    wcvp_distributions = make_distribution_records()
  )

  expect_equal(nrow(out), 2)
  expect_true(all(c("distribution", "areas", "area_codes", "n_areas") %in% names(out)))
  expect_true(is.character(out$areas))
  expect_true(is.character(out$area_codes))

  matched_out <- dplyr::filter(out, submited_name == "Nopalea cochenilliferaa")
  unmatched_out <- dplyr::filter(out, submited_name == "Taxon inexistente")

  expect_equal(matched_out$distribution_status, "distribution_found")
  expect_equal(matched_out$n_areas, 2)
  expect_equal(matched_out$distribution, "Colombia - Mexico")
  expect_equal(matched_out$areas, "Colombia - Mexico")
  expect_equal(matched_out$area_codes, "COL - MEX")

  expect_equal(unmatched_out$distribution_status, "no_match")
  expect_true(is.na(unmatched_out$distribution))
  expect_equal(unmatched_out$n_areas, 0)
  expect_true(is.na(unmatched_out$areas))
})

test_that("wcvp_distribution can fall back to genus distribution without breaking", {
  out <- wcvp_distribution(
    "Opuntia especieinventada",
    taxon_rank = "species",
    wcvp_names = make_distribution_names(),
    wcvp_distributions = make_distribution_records()
  )

  expect_true(all(out$distribution_status == "genus_distribution_fallback"))
  expect_true(all(out$matched))
  expect_true(all(out$taxon_rank == "species"))
  expect_equal(unique(out$matched_taxon), "Opuntia")
  expect_setequal(out$area_code_l3, c("COL", "MEX"))
})

test_that("wcvp_distribution does not abort on incomplete species inputs", {
  expect_no_error(
    out <- wcvp_distribution(
      c("Opuntia", "Taxon inexistente"),
      taxon_rank = "species",
      summarise_by_input = TRUE,
      wcvp_names = make_distribution_names(),
      wcvp_distributions = make_distribution_records()
    )
  )

  expect_equal(nrow(out), 2)
  expect_true(all(out$distribution_status %in% c("genus_distribution_fallback", "no_match")))
})

test_that("wcvp_distribution errors when distributions schema is incomplete", {
  bad_dist <- dplyr::select(make_distribution_records(), -area_code_l3)

  expect_error(
    wcvp_distribution(
      "Opuntia",
      taxon_rank = "genus",
      wcvp_names = make_distribution_names(),
      wcvp_distributions = bad_dist
    ),
    "area_code_l3"
  )
})
