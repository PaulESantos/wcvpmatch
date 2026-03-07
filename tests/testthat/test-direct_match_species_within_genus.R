test_that("all species within genus names matched in test", {
 df <- get_testset(mutation = 2) %>%
   direct_match() %>%
   genus_match() %>%
   fuzzy_match_genus() %>%
   direct_match_species_within_genus()

 expect_true(all(df$Matched.Species %in% df$Orig.Species))
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

  out <- direct_match_species_within_genus(input, target_df = target_df)

  expect_false(out$direct_match_species_within_genus[1])
})
