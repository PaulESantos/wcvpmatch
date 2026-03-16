test_that("correct one character fuzzy match", {
  skip_if_no_default_backbone()
  df <- get_testset(mutation = 1) |>
    wcvp_genus_match() |>
    wcvp_fuzzy_match_species_within_genus()

  expect_false(all(df$Matched.Species %in% get_testset(mutation = 0)$Orig.Species))
  expect_true(all(df$fuzzy_species_dist == 1, na.rm = TRUE))
})

test_that("transposition of adjacent characters: expect distance one based on optimal string alignment distance (osa): see stringdist", {
  skip_if_no_default_backbone()
  # set.seed(100)
  # df <- Trees.Full %>% dplyr::select(Genus, Species) %>% dplyr::rename(Orig.Genus = Genus, Orig.Species = Species) %>% dplyr::sample_n(10)
  genera <- c("Deguelia", "Maerua", "Dortmanna", "Photinia", "Dolicholus", "Mimosa", "Freycinetia", "Photinia")
  species <- c("chrysophylla", "schinzii", "fervens", "chingshuiensis", "pringlei", "weddelliana", "impavida", "zhijiangensis")
  df <- tibble::tibble("Orig.Genus" = genera,
                       "Orig.Species" = species)

  ## check if these species still present in database:
  ## else test examples has to be changed
  assertthat::assert_that(nrow(df) == nrow(df |>
    dplyr::semi_join(get_db(),
      by = c(
        "Orig.Genus" = "Genus",
        "Orig.Species" = "Species"
      )
    )))

  ## introduce transposition errors
  transposed_species <- c(
    "chrysohpylla", "schinizi", "fevrens",
    "chingshiuensis", "rpinglei", "weddleliana", "ipmavida", "zhiijangensis"
  )
  transposed_df <- tibble::tibble(
    Orig.Genus = df$Orig.Genus,
    Orig.Species = transposed_species
  ) |>
    wcvp_genus_match() |>
    wcvp_fuzzy_match_species_within_genus()

  expect_true(all(transposed_df$Matched.Species %in% df$Orig.Species))
  expect_true(all(transposed_df$fuzzy_species_dist == 1))
})

test_that("correct two character fuzzy match", {
  skip_if_no_default_backbone()
  # set.seed(100)
  genera <- c("Deguelia", "Koanophyllon", "Maerua", "Dortmanna", "Photinia",
              "Salvia", "Dolicholus", "Mimosa", "Freycinetia", "Photinia")
  species <- c("chrysophylla", "plicatum", "schinzii", "fervens", "chingshuiensis",
               "brachystachys", "pringlei", "weddelliana", "impavida", "zhijiangensis")
  df <- tibble::tibble("Orig.Genus" = genera,
                       "Orig.Species" = species)

  transposed_species <- c("chryssophyla", "pcatum", "shinzi", "fervennes",
                          "chiingshuienis", "braschystacchys", "inglei",
                          "wedeliana", "impavi", "ijiangensis")

  transposed_df <- expect_no_warning(
    tibble::tibble(
      Orig.Genus = df$Orig.Genus,
      Orig.Species = transposed_species
    ) |>
      wcvp_genus_match() |>
      wcvp_fuzzy_match_species_within_genus()
  )

  expect_false(all(transposed_df$Matched.Species %in% df$Orig.Species))
  expect_false(all(transposed_df$fuzzy_species_dist == 2, na.rm = TRUE) &&
    any(!is.na(transposed_df$fuzzy_species_dist)))
})

test_that("fuzzy species tie stores ambiguous candidates in attribute", {
  target_df <- tibble::tibble(
    genus = c("Abc", "Abc"),
    species = c("alba", "alga"),
    infraspecific_rank = NA_character_,
    infraspecies = NA_character_
  )

  input <- tibble::tibble(
    Orig.Genus = "Abc",
    Orig.Species = "alca",
    Matched.Genus = "Abc"
  )

  expect_warning(
    out <- wcvp_fuzzy_match_species_within_genus(input, target_df = target_df, max_dist = 1, method = "osa"),
    "Multiple fuzzy matches"
  )

  amb <- attr(out, "ambiguous_species")
  expect_false(is.null(amb))
  expect_true(nrow(amb) >= 2)
  expect_true(all(amb$Orig.Genus == "Abc"))
  expect_true(all(amb$Orig.Species == "alca"))
  expect_true(all(amb$fuzzy_species_dist == min(amb$fuzzy_species_dist)))
})
