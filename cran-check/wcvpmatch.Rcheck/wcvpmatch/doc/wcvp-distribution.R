## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  message = FALSE,
  warning = FALSE
)

if (file.exists("../DESCRIPTION") && requireNamespace("pkgload", quietly = TRUE)) {
  pkgload::load_all("..", export_all = FALSE, helpers = FALSE, quiet = TRUE)
} else {
  library(wcvpmatch)
}

library(tibble)
library(dplyr)

## -----------------------------------------------------------------------------
make_distribution_names <- function() {
  tibble(
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
  tibble(
    plant_locality_id = 1:7,
    plant_name_id = c(1, 2, 3, 3, 4, 5, 6),
    continent_code_l1 = c("8", "8", "8", "4", "1", "8", "8"),
    continent = c(
      "SOUTHERN AMERICA", "SOUTHERN AMERICA", "SOUTHERN AMERICA",
      "NORTHERN AMERICA", "EUROPE", "SOUTHERN AMERICA", "SOUTHERN AMERICA"
    ),
    region_code_l2 = c("83", "83", "83", "41", "10", "85", "83"),
    region = c(
      "Western South America", "Western South America", "Western South America",
      "Mexico", "Europe", "Southern South America", "Western South America"
    ),
    area_code_l3 = c("MEX", "PER", "COL", "MEX", "ESP", "GAL", "MEX"),
    area = c("Mexico", "Peru", "Colombia", "Mexico", "Spain", "Galapagos", "Mexico"),
    introduced = c(0, 0, 0, 1, 0, 0, 0),
    extinct = c(0, 0, 0, 0, 0, 0, 0),
    location_doubtful = c(0, 0, 0, 0, 0, 0, 0)
  )
}

distribution_names <- make_distribution_names()
distribution_records <- make_distribution_records()

distribution_names
distribution_records

## -----------------------------------------------------------------------------
species_out <- wcvp_distribution(
  c("Nopalea cochenilliferaa", "Taxon inexistente"),
  taxon_rank = "species",
  wcvp_names = distribution_names,
  wcvp_distributions = distribution_records
)

species_out |>
  select(
    submited_name,
    matched_taxon,
    accepted_taxon_name,
    area_code_l3,
    area,
    distribution_status
  )

## -----------------------------------------------------------------------------
species_summary <- wcvp_distribution(
  c("Nopalea cochenilliferaa", "Taxon inexistente"),
  taxon_rank = "species",
  summarise_by_input = TRUE,
  wcvp_names = distribution_names,
  wcvp_distributions = distribution_records
)

species_summary |>
  select(
    submited_name,
    accepted_taxon_name,
    distribution_status,
    area_codes,
    distribution,
    n_areas
  )

## -----------------------------------------------------------------------------
spatial_out <- wcvp_distribution(
  "Nopalea cochenillifera",
  taxon_rank = "species",
  output = "spatial",
  wcvp_names = distribution_names,
  wcvp_distributions = distribution_records
)

spatial_out

## -----------------------------------------------------------------------------
genus_out <- wcvp_distribution(
  "Opuntia",
  taxon_rank = "genus",
  introduced = FALSE,
  wcvp_names = distribution_names,
  wcvp_distributions = distribution_records
)

genus_out |>
  select(matched_taxon, area_code_l3, area, occurrence_type, distribution_status)

## -----------------------------------------------------------------------------
family_out <- wcvp_distribution(
  "Cactacee",
  taxon_rank = "family",
  max_dist = 1,
  wcvp_names = distribution_names,
  wcvp_distributions = distribution_records
)

family_out |>
  select(matched_taxon, match_distance, area_code_l3, area) |>
  distinct()

## -----------------------------------------------------------------------------
fallback_out <- wcvp_distribution(
  "Opuntia especieinventada",
  taxon_rank = "species",
  wcvp_names = distribution_names,
  wcvp_distributions = distribution_records
)

fallback_out |>
  select(submited_name, matched_taxon, area_code_l3, area, distribution_status)

