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
make_matching_backbone <- function() {
  tibble(
    genus = c("Aniba", "Jaltomata", "Veronica", "Veronica"),
    species = c("heterotepala", "sagastegui", "vulcanica", "spathulata"),
    infraspecific_rank = NA_character_,
    infraspecies = NA_character_,
    plant_name_id = c(1, 2, 10, 200),
    taxon_name = c(
      "Aniba heterotepala",
      "Jaltomata sagastegui",
      "Veronica vulcanica",
      "Veronica spathulata"
    ),
    taxon_authors = c("A.Author", "B.Author", "C.Author", "D.Author"),
    taxon_status = c("Accepted", "Accepted", "Synonym", "Accepted"),
    accepted_plant_name_id = c(1, 2, 200, 200)
  )
}

matching_backbone <- make_matching_backbone()
matching_backbone

## -----------------------------------------------------------------------------
parsed_names <- classify_spnames(
  c("Aniba heterotepala", "Jaltometa sagasteguii", "Veronica vulcanica")
)

parsed_names |>
  select(Input.Name, Orig.Genus, Orig.Species, Rank)

## -----------------------------------------------------------------------------
matched <- wcvp_matching(
  parsed_names,
  target_df = matching_backbone,
  allow_duplicates = TRUE,
  max_dist = 2,
  method = "osa",
  add_name_distance = TRUE,
  output_name_style = "snake_case",
  output = "full"
)

matched |>
  select(
    input_name,
    matched_taxon_name,
    accepted_taxon_name,
    taxon_status,
    matched_dist
  )

## -----------------------------------------------------------------------------
matched |>
  select(
    input_name,
    direct_match,
    genus_match,
    fuzzy_match_genus,
    direct_match_species_within_genus,
    suffix_match_species_within_genus,
    fuzzy_match_species_within_genus,
    matched
  )

## -----------------------------------------------------------------------------
matched |>
  filter(input_name == "Veronica vulcanica") |>
  select(
    input_name,
    matched_taxon_name,
    accepted_taxon_name,
    matched_taxon_authors,
    accepted_taxon_authors,
    taxon_status,
    is_accepted_name
  )

## -----------------------------------------------------------------------------
duplicate_input <- tibble(
  Genus = c("Aniba", "Aniba"),
  Species = c("heterotepala", "heterotepala"),
  Input.Name = c("Aniba heterotepala", "Aniba heterotepala")
)

wcvp_matching(
  duplicate_input,
  target_df = matching_backbone,
  allow_duplicates = TRUE,
  output_name_style = "snake_case"
) |>
  select(input_index, input_name, matched_taxon_name, accepted_taxon_name)

