test_that("all species within genus names matched in test", {
  skip_if_no_default_backbone()
  df <- get_testset(mutation = 2) %>%
    wcvpmatch:::wcvp_direct_match() %>%
    wcvpmatch:::wcvp_genus_match() %>%
    wcvpmatch:::wcvp_fuzzy_match_genus() %>%
    wcvpmatch:::wcvp_direct_match_species_within_genus()

  matched_spp <- stats::na.omit(df$Matched.Species)
  expect_true(all(matched_spp %in% df$Orig.Species))
})

test_that("direct species match is constrained to matched genus", {
  target_df <- tibble::tibble(
    genus = c("Jaltomata", "Othergenus"),
    species = c("sagastegui", "sagasteguii"),
    infraspecific_rank = NA_character_,
    infraspecies = NA_character_
  )

  input <- tibble::tibble(
    Orig.Genus = "Jaltometa",
    Orig.Species = "sagasteguii",
    Matched.Genus = "Jaltomata",
    Rank = 2
  )

  out <- wcvpmatch:::wcvp_direct_match_species_within_genus(input, target_df = target_df)

  expect_false(out$direct_match_species_within_genus[1])
})
