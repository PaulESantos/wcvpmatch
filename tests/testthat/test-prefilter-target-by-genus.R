make_prefilter_target <- function() {
  tibble::tibble(
    plant_name_id = c(1, 2, 3, 4),
    genus = c("Acer", "Acer", "Quercus", "Abies"),
    species = c("rubrum", "saccharum", "robur", "alba"),
    infraspecific_rank = c(NA_character_, NA_character_, NA_character_, NA_character_),
    infraspecies = c(NA_character_, NA_character_, NA_character_, NA_character_)
  )
}

test_that("build_genus_index returns one row per genus with list plant_name_id", {
  idx <- build_genus_index(make_prefilter_target())

  expect_true(all(c("genus", "plant_name_id", "n_records") %in% names(idx)))
  expect_equal(nrow(idx), 3)
  expect_true(is.list(idx$plant_name_id))
  expect_equal(idx$n_records[idx$genus == "Acer"], 2)
})

test_that("prefilter_target_by_genus exact filtering keeps only candidate genera", {
  input <- tibble::tibble(Genus = c("Acer", "Quercus"), Species = c("rubrum", "robur"))
  out <- prefilter_target_by_genus(
    df = input,
    target_df = make_prefilter_target(),
    include_fuzzy = FALSE
  )

  expect_true(all(unique(out$genus) %in% c("Acer", "Quercus")))
  expect_false("Abies" %in% out$genus)
  expect_equal(sort(attr(out, "exact_genera")), c("Acer", "Quercus"))
})

test_that("prefilter_target_by_genus can include fuzzy genus candidates", {
  input <- tibble::tibble(Genus = c("Acr"), Species = c("rubrum"))
  out <- prefilter_target_by_genus(
    df = input,
    target_df = make_prefilter_target(),
    include_fuzzy = TRUE,
    max_dist = 1,
    method = "osa"
  )

  expect_true("Acer" %in% attr(out, "candidate_genera"))
  expect_true("Acer" %in% out$genus)
})
