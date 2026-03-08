test_that("correct matches for test6 dataset", {
  df <- get_testset(mutation = 0) |>
    wcvp_matching(prefilter_genus = TRUE)

  expect_false(all(df$direct_match))

  df <- get_testset(mutation = 1) |>
    wcvp_matching(prefilter_genus = TRUE)
  expect_false(any(df$direct_match))
  expect_true(all(df$genus_match))
  expect_false(all(df$suffix_match_species_within_genus | df$fuzzy_match_species_within_genus))
})

#test_that("test random characters", {
#  set.seed(111)
#
#  random <- tibble::tibble(Genus = sapply(vector("list", 10), FUN = function(x) paste(sample(letters, size = 6, replace = TRUE), collapse = '')),
#                        Species = sapply(vector("list", 10), FUN = function(x) paste(sample(letters, size = 8, replace = TRUE), collapse = '')))
#  random
#  matched_random <- random |>
#    dplyr::mutate(Genus = stringr::str_to_title(Genus)) |>
#    wcvp_matching(prefilter_genus = TRUE)
#
#  expect_false(any(matched_random$matched) | any(matched_random$direct_match) | any(matched_random$genus_match) | any(matched_random$fuzzy_match_genus))
#  expect_true(all(is.na(matched_random)[,c('Matched.Genus', 'Matched.Species', 'direct_match_species_within_genus', 'suffix_match_species_within_genus', 'fuzzy_match_species_within_genus', 'fuzzy_genus_dist', 'fuzzy_species_dist')]))
#})

# test_that("test empty dataframe Genus, Species", {
#   res <- get_db() |>
#     dplyr::sample_n(0) |>
#     dplyr::select(Genus, Species) |>
#     wcvp_matching(prefilter_genus = TRUE)
#   res
#   expect_true(nrow(res) == 0)
#   expect_true(all(c("Matched.Genus", 'Matched.Species') %in% colnames(res)))
#   res <- get_db() |>
#     dplyr::sample_n(0) |>
#     dplyr::select(Genus, Species) |>
#     dplyr::rename(Orig.Genus = Genus, Orig.Species = Species) |>
#     wcvp_matching(prefilter_genus = TRUE)
#   expect_true(nrow(res) == 0)
#   expect_true(all(c("Matched.Genus", 'Matched.Species') %in% colnames(res)))
# })

test_that("test inconsistent input types", {
  ###
  # These cases should cause errors
  ###

  ## NA in input
  input <- tibble::tibble(Genus = c('Fagus'), Species = c(NA))
  expect_error(input |>
                 wcvp_matching(prefilter_genus = TRUE))
  input <- tibble::tibble(Genus = c(NA), Species = c('sylvatica'))
  expect_error(input |>
                 wcvp_matching(prefilter_genus = TRUE))
  ## duplicates
  input <- tibble::tibble(Genus = rep('Fagus', 2), Species = rep('sylvatica', 2))
  expect_error(input |>
                 wcvp_matching(prefilter_genus = TRUE))
  ## Genus first letter upper case
  input <- tibble::tibble(Genus = c('fagus'), Species = c('sylvatica'))
  expect_error(input |>
                 wcvp_matching(prefilter_genus = TRUE))
  ## Only one uppercase letter in Genus
  input <- tibble::tibble(Genus = c('Fagus-Pinus'), Species = c('sylvatica'))
  expect_error(input |>
                 wcvp_matching(prefilter_genus = TRUE))
  input <- tibble::tibble(Genus = c('FAGUS'), Species = c('sylvatica'))
  expect_error(input |>
                 wcvp_matching(prefilter_genus = TRUE))
  input <- tibble::tibble(Genus = c('FAgus'), Species = c('sylvatica'))
  expect_error(input |>
                 wcvp_matching(prefilter_genus = TRUE))
  ## No uppercase letter in Species
  input <- tibble::tibble(Genus = c('Fagus'), Species = c('Sylvatica'))
  expect_error(input |>
                 wcvp_matching(prefilter_genus = TRUE))
  input <- tibble::tibble(Genus = c('Fagus'), Species = c('sylvaTica'))
  expect_error(input |>
                 wcvp_matching(prefilter_genus = TRUE))
  input <- tibble::tibble(Genus = c('Fagus'), Species = c('SYLVATICA'))
  expect_error(input |>
                 wcvp_matching(prefilter_genus = TRUE))
  ## no hybrid characters in Genus name
  input <- tibble::tibble(Genus = c('Fagus\u00D7pinus'), Species = c('sylvatica'))
  expect_error(input |>
                 wcvp_matching(prefilter_genus = TRUE))
  input <- tibble::tibble(Genus = c('\u00D7Fagus'), Species = c('sylvatica'))
  expect_error(input |>
                 wcvp_matching(prefilter_genus = TRUE))
  input <- tibble::tibble(Genus = c('Fagus\u00D7'), Species = c('sylvatica'))
  expect_error(input |>
                 wcvp_matching(prefilter_genus = TRUE))

  ###
  # These cases should trigger warnings
  ###
  ## trailing spaces
  input <- tibble::tibble(Genus = c('Fagus'), Species = c('sylvatica '))
  expect_warning(input |>
                   wcvp_matching(prefilter_genus = TRUE))
  input <- tibble::tibble(Genus = c('Fagus '), Species = c('sylvatica'))
  expect_warning(input |>
                   wcvp_matching(prefilter_genus = TRUE))
  ## leading spaces
  input <- tibble::tibble(Genus = c('Fagus'), Species = c(' sylvatica'))
  expect_warning(input |>
                   wcvp_matching(prefilter_genus = TRUE))
  input <- tibble::tibble(Genus = c(' Fagus'), Species = c('sylvatica'))
  expect_warning(input |>
                   wcvp_matching(prefilter_genus = TRUE))

  ###
  # These should trigger messages
  ###
  ## input is data.frame and not tibble::tibble
  input <- data.frame(Genus = c('Fagus'), Species = c('sylvatica'))
  expect_message(input |>
                   wcvp_matching(prefilter_genus = TRUE))

})

test_that("check_df_format trims whitespace before consistency regex checks", {
  input <- tibble::tibble(Orig.Genus = "Fagus", Orig.Species = "sylvatica ", Rank = 2)

  expect_warning(input <- check_df_format(input))
  expect_no_error(check_df_consistency(input))
  expect_equal(input$Orig.Species[1], "sylvatica")
})

test_that("matching output always includes Input.Name", {
  target_df <- tibble::tibble(
    genus = "Fagus",
    species = "sylvatica",
    infraspecific_rank = NA_character_,
    infraspecies = NA_character_
  )

  input_auto <- tibble::tibble(Genus = "Fagus",
                               Species = "sylvatica")

  out_auto <- wcvp_matching(input_auto,
                            target_df = target_df)

  expect_true("input_name" %in% names(out_auto))
  expect_equal(out_auto$input_name[1], "Fagus sylvatica")

  input_keep <- tibble::tibble(
    Genus = "Fagus",
    Species = "sylvatica",
    Input.Name = "Fagus sylvatica L."
  )
  out_keep <- wcvp_matching(input_keep, target_df = target_df)
  expect_equal(out_keep$input_name[1], "Fagus sylvatica L.")
})

test_that("matching with prefilter_genus keeps same result on small custom target", {
  target_df <- tibble::tibble(
    genus = c("Acer", "Quercus", "Abies"),
    species = c("rubrum", "robur", "alba"),
    infraspecific_rank = NA_character_,
    infraspecies = NA_character_
  )

  input <- tibble::tibble(
    Genus = c("Acer", "Acr"),
    Species = c("rubrum", "rubrum")
  )

  out_base <- wcvp_matching(input, target_df = target_df, max_dist = 1, method = "osa")
  out_pref <- wcvp_matching(input, target_df = target_df, prefilter_genus = TRUE, max_dist = 1, method = "osa")

  expect_equal(out_pref$matched, out_base$matched)
  expect_equal(out_pref$matched_genus, out_base$matched_genus)
  expect_equal(out_pref$matched_species, out_base$matched_species)
})

test_that("matching can expand duplicate inputs with input_index when allow_duplicates is TRUE", {
  target_df <- tibble::tibble(
    genus = c("Opuntia", "Opuntia"),
    species = c("corotilla", "yanganucensis"),
    infraspecific_rank = NA_character_,
    infraspecies = NA_character_
  )

  input <- tibble::tibble(
    Genus = c("Opuntia", "Opuntia", "Opuntia"),
    Species = c("yanganucensis", "yanganucensis", "corotilla"),
    Input.Name = c("Opuntia yanganucensis", "Opuntia yanganucensis", "Opuntia corotilla")
  )

  expect_no_error(
    out <- wcvp_matching(input, target_df = target_df, allow_duplicates = TRUE)
  )

  expect_true("input_index" %in% names(out))
  expect_equal(nrow(out), nrow(input))
  expect_equal(out$input_index, c(1, 2, 3))
  expect_true(all(out$matched))
})

test_that("matching duplicate error suggests allow_duplicates option", {
  target_df <- tibble::tibble(
    genus = "Opuntia",
    species = "yanganucensis",
    infraspecific_rank = NA_character_,
    infraspecies = NA_character_
  )

  input <- tibble::tibble(
    Genus = c("Opuntia", "Opuntia"),
    Species = c("yanganucensis", "yanganucensis")
  )

  expect_error(
    wcvp_matching(input, target_df = target_df, allow_duplicates = FALSE),
    "allow_duplicates = TRUE"
  )
})

test_that("wcvp_matching propagates ambiguous genus fuzzy ties", {
  target_df <- tibble::tibble(
    genus = c("Aaa", "Aab"),
    species = c("beta", "beta"),
    infraspecific_rank = NA_character_,
    infraspecies = NA_character_
  )

  input <- tibble::tibble(
    Genus = "Aac",
    Species = "beta"
  )

  expect_warning(
    out <- wcvp_matching(input, target_df = target_df, max_dist = 1, method = "osa"),
    "Multiple fuzzy matches"
  )

  amb <- attr(out, "ambiguous_genus")
  expect_false(is.null(amb))
  expect_true(nrow(amb) >= 2)
})

test_that("wcvp_matching propagates ambiguous species fuzzy ties", {
  target_df <- tibble::tibble(
    genus = c("Abc", "Abc"),
    species = c("alba", "alga"),
    infraspecific_rank = NA_character_,
    infraspecies = NA_character_
  )

  input <- tibble::tibble(
    Genus = "Abc",
    Species = "alca"
  )

  expect_warning(
    out <- wcvp_matching(input, target_df = target_df, max_dist = 1, method = "osa"),
    "Multiple fuzzy matches"
  )

  amb <- attr(out, "ambiguous_species")
  expect_false(is.null(amb))
  expect_true(nrow(amb) >= 2)
})

test_that("matching returns matched and accepted author columns when available", {
  target_df <- tibble::tibble(
    genus = c("Veronica", "Veronica", "Veronica"),
    species = c("vulcanica", "vulcanica", "spathulata"),
    infraspecific_rank = NA_character_,
    infraspecies = NA_character_,
    plant_name_id = c(10, 11, 200),
    taxon_name = c("Veronica vulcanica", "Veronica vulcanica", "Veronica spathulata"),
    taxon_authors = c("A.Author", "B.Author", "C.Author"),
    taxon_status = c("Synonym", "Synonym", "Accepted"),
    accepted_plant_name_id = c(200, 200, 200)
  )

  input <- tibble::tibble(
    Orig.Genus = "Veronica",
    Orig.Species = "vulcanica",
    Rank = 2
  )

  out <- wcvp_matching(input, target_df = target_df)

  expect_true(all(c("matched_taxon_authors", "accepted_taxon_authors") %in% names(out)))
  expect_true(out$matched_taxon_authors[1] %in% c("A.Author", "B.Author"))
  expect_equal(out$accepted_taxon_authors[1], "C.Author")
})

test_that("matching can standardize output names to snake_case", {
  target_df <- tibble::tibble(
    genus = "Fagus",
    species = "sylvatica",
    infraspecific_rank = NA_character_,
    infraspecies = NA_character_
  )

  input <- tibble::tibble(
    Genus = "Fagus",
    Species = "sylvatica"
  )

  out <- wcvp_matching(input, target_df = target_df, output_name_style = "snake_case")

  expect_true(all(c("orig_genus", "orig_species", "matched_genus", "matched_species", "input_name") %in% names(out)))
  expect_false(any(c("Orig.Genus", "Orig.Species", "Matched.Genus", "Input.Name") %in% names(out)))
})

test_that("matching can add fast pairwise input-vs-matched name distance", {
  target_df <- tibble::tibble(
    genus = c("Fagus", "Quercus"),
    species = c("sylvatica", "robur"),
    infraspecific_rank = NA_character_,
    infraspecies = NA_character_
  )

  input <- tibble::tibble(
    Genus = c("Fagus", "Qercus"),
    Species = c("sylvatica", "robur")
  )

  out <- wcvp_matching(
    input,
    target_df = target_df,
    add_name_distance = TRUE,
    name_distance_method = "osa"
  )

  expect_true("matched_dist" %in% names(out))
  expect_true(is.numeric(out$matched_dist))
  expect_true(all(!is.na(out$matched_dist[out$matched])))
})

test_that("matched TRUE is always coherent with matched_taxon_name", {
  target_df <- tibble::tibble(
    genus = c("Aniba", "Jaltomata"),
    species = c("heterotepala", "sagastegui"),
    infraspecific_rank = NA_character_,
    infraspecies = NA_character_,
    plant_name_id = c(1, 2),
    taxon_name = c("Aniba heterotepala", "Jaltomata sagastegui"),
    taxon_status = c("Accepted", "Accepted"),
    accepted_plant_name_id = c(1, 2)
  )

  input <- classify_spnames(c(
    "Aniba heterotepala",
    "Jaltometa sagasteguii"
  ))

  out <- wcvp_matching(
    input,
    target_df = target_df,
    max_dist = 2,
    method = "osa",
    allow_duplicates = TRUE
  )

  expect_true(all(!out$matched | !is.na(out$matched_taxon_name)))
})
