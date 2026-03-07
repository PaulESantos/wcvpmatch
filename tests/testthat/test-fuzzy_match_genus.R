test_that("all fuzzy matches", {
  df1 <- get_testset(mutation = 2) |>
    fuzzy_match_genus()
  df2 <- get_testset(mutation = 3) |>
    fuzzy_match_genus()
  dfs <- list(df1, df2)
  for(df in dfs){
    expect_true("Matched.Genus" %in% colnames(df1))
    expect_true(all(df$fuzzy_match_genus))
    expect_true(all(df$fuzzy_genus_dist <= 1)) ## should be equal 1: but due to a coincidence removeing  the last character led to another genus name
  }
})

test_that("fuzzy genus tie stores ambiguous candidates in attribute", {
  target_df <- tibble::tibble(
    genus = c("Aaa", "Aab"),
    species = c("beta", "beta"),
    infraspecific_rank = NA_character_,
    infraspecies = NA_character_
  )

  input <- tibble::tibble(Genus = "Aac", Species = "beta")

  expect_warning(
    out <- fuzzy_match_genus(input, target_df = target_df, max_dist = 1, method = "osa"),
    "Multiple fuzzy matches"
  )

  amb <- attr(out, "ambiguous_genus")
  expect_false(is.null(amb))
  expect_true(nrow(amb) >= 2)
  expect_true(all(amb$Orig.Genus == "Aac"))
  expect_true(all(amb$fuzzy_genus_dist == min(amb$fuzzy_genus_dist)))
})
