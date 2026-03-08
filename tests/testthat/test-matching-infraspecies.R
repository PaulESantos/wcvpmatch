make_target_infra <- function() {
  tibble::tibble(
    Genus = c("Acer", "Acer"),
    Species = c("rubrum", "rubrum"),
    genus = c("Acer", "Acer"),
    species = c("rubrum", "rubrum"),
    infraspecific_rank = c("var.", "subsp."),
    infraspecies = c("alpina", "montana"),
    scientific_name = c("Acer rubrum var. alpina", "Acer rubrum subsp. montana"),
    accepted_name = c("Acer rubrum var. alpina", "Acer rubrum subsp. montana"),
    threat_category = c("LC", "EN"),
    taxonomic_status = c("accepted", "accepted")
  )
}

test_that("matching supports infraspecific rank + fuzzy epithet (WCVP schema)", {
  target_df <- make_target_infra()

  df <- tibble::tibble(
    Genus = c("Acer", "Acer", "Acer"),
    Species = c("rubrum", "rubrum", "rubrum"),
    `Infra.Rank` = c("var.", "var.", "subsp."),
    Infraspecies = c("alpina", "alpino", "montana"),
    Rank = 3
  )


  out <- wcvp_matching(df, target_df = target_df)

  expect_true(all(out$matched))
  expect_true(all(out$direct_match_infra_rank))
  expect_true(all(out$fuzzy_match_infraspecies))
  expect_true("matched_infraspecies" %in% colnames(out))
  expect_true(any(out$fuzzy_infraspecies_dist == 1, na.rm = TRUE))
})

test_that("matching leaves unmatched when infraspecific rank does not exist", {
  target_df <- make_target_infra()

  df <- tibble::tibble(
    Genus = "Acer",
    Species = "rubrum",
    `Infra.Rank` = "f.",
    Infraspecies = "alpina",
    Rank = 3

  )

  out <- wcvp_matching(df, target_df = target_df)

  expect_false(out$matched)
  expect_false(out$direct_match_infra_rank)
  expect_true(is.na(out$fuzzy_match_infraspecies))
})
