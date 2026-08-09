make_synonym_names <- function() {
  tibble::tibble(
    plant_name_id = c(1, 2, 3, 4),
    accepted_plant_name_id = c(1, 1, 1, 4),
    taxon_rank = "Species",
    taxon_status = c("Accepted", "Synonym", "Synonym", "Accepted"),
    family = "Cactaceae",
    genus = c("Opuntia", "Nopalea", "Cactus", "Opuntia"),
    species = c("cochenillifera", "cochenillifera", "cochenillifera", "ficus-indica"),
    taxon_name = c(
      "Opuntia cochenillifera",
      "Nopalea cochenillifera",
      "Cactus cochenillifera",
      "Opuntia ficus-indica"
    ),
    taxon_authors = c("(L.) Salm-Dyck", "(L.) Salm-Dyck", "L.", "(L.) Mill."),
    homotypic_synonym = c(NA, TRUE, FALSE, NA),
    basionym_plant_name_id = c(NA, "3", "3", NA)
  )
}

test_that("wcvp_synonyms resolves an accepted name and returns its synonyms", {
  out <- wcvp_synonyms(
    "Opuntia cochenillifera",
    target_df = make_synonym_names()
  )

  expect_equal(unique(out$accepted_taxon_name), "Opuntia cochenillifera")
  expect_setequal(out$synonym_name, c("Nopalea cochenillifera", "Cactus cochenillifera"))
  expect_named(
    out,
    c(
      "submitted_name", "accepted_taxon_name", "accepted_taxon_authors",
      "name_role", "synonym_name", "synonym_authors", "homotypic_synonym"
    )
  )
})

test_that("wcvp_synonyms resolves a synonym through the matching core", {
  out <- wcvp_synonyms(
    "Nopalea cochenilliferaa",
    target_df = make_synonym_names()
  )

  expect_equal(unique(out$accepted_taxon_name), "Opuntia cochenillifera")
  expect_setequal(out$synonym_name, c("Nopalea cochenillifera", "Cactus cochenillifera"))
})

test_that("wcvp_synonyms keeps diagnostic fields in full output", {
  out <- wcvp_synonyms(
    "Opuntia cochenillifera",
    target_df = make_synonym_names(),
    output = "full"
  )

  expect_true(all(c("matched", "match_distance", "accepted_plant_name_id", "basionym_plant_name_id", "synonym_status") %in% names(out)))
})

test_that("wcvp_synonyms can retain the accepted name and unmatched inputs", {
  out <- wcvp_synonyms(
    c("Opuntia cochenillifera", "Unknown species"),
    target_df = make_synonym_names(),
    include_accepted = TRUE,
    output = "full"
  )

  expect_true(any(out$name_role == "accepted"))
  expect_true(any(out$synonym_status == "no_match"))
})

test_that("wcvp_synonyms returns one accepted row when no synonyms are recorded", {
  out <- wcvp_synonyms(
    "Opuntia ficus-indica",
    target_df = make_synonym_names(),
    include_accepted = TRUE,
    output = "full"
  )

  expect_equal(nrow(out), 1)
  expect_equal(out$name_role, "accepted")
  expect_equal(out$synonym_status, "accepted_name")
  expect_true(is.na(out$synonym_name))
})
