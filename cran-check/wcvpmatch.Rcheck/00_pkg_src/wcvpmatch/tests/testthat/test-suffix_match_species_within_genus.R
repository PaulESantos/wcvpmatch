test_that("all suffix ending matches", {
  skip_if_no_default_backbone()

  test_dat <- tibble::tibble(Orig.Genus = rep("Abarema", 2),
                             Orig.Species = c("angulatum", "abbottiae"))
  df <- test_dat |>
    wcvpmatch:::wcvp_direct_match() |>
    wcvpmatch:::wcvp_genus_match() |>
    wcvpmatch:::wcvp_direct_match_species_within_genus() |>
    wcvpmatch:::wcvp_suffix_match_species_within_genus()
  expect_true(all(df$suffix_match_species_within_genus))
})

test_that("suffix matching is constrained to matched genus candidates", {
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

  out <- wcvpmatch:::wcvp_suffix_match_species_within_genus(input, target_df = target_df)

  expect_false(out$suffix_match_species_within_genus[1])
})

test_that("ambiguous suffix roots continue to fuzzy matching", {
  target <- tibble::tibble(
    genus = c("Abies", "Abies"),
    species = c("alba", "album"),
    infraspecific_rank = NA_character_,
    infraspecies = NA_character_
  )
  input <- tibble::tibble(
    Orig.Genus = "Abies",
    Orig.Species = "albus",
    Matched.Genus = "Abies"
  )

  out <- wcvpmatch:::wcvp_suffix_match_species_within_genus(input, target)

  expect_false(out$suffix_match_species_within_genus)
  expect_equal(nrow(attr(out, "ambiguous_suffix")), 2)
})
