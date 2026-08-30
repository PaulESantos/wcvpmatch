make_performance_target <- function() {
  tibble::tibble(
    plant_name_id = c(1, 2, 3),
    genus = c("Acer", "Acer", "Quercus"),
    species = c("rubrum", "saccharum", "robur"),
    infraspecific_rank = NA_character_,
    infraspecies = NA_character_,
    taxon_name = c("Acer rubrum", "Acer saccharum", "Quercus robur"),
    taxon_status = "Accepted",
    accepted_plant_name_id = c(1, 2, 3)
  )
}

test_that("prepared target databases are reused without global taxon keys", {
  prepared <- wcvpmatch:::get_db(make_performance_target())

  expect_true(isTRUE(attr(prepared, "wcvpmatch_prepared")))
  expect_false(".taxon_key" %in% names(prepared))
  expect_identical(wcvpmatch:::get_db(prepared), prepared)
})

test_that("target normalization canonicalizes empty taxonomic components", {
  target <- tibble::tibble(
    genus = "Fagus",
    species = "sylvatica",
    infraspecific_rank = " ",
    infraspecies = ""
  )

  normalized <- wcvpmatch:::normalize_target_df(target)

  expect_true(is.na(normalized$infraspecific_rank))
  expect_true(is.na(normalized$infraspecies))
})

test_that("compact genus lookup is sufficient for prefiltering", {
  target <- wcvpmatch:::get_db(make_performance_target())
  lookup <- wcvpmatch:::build_genus_lookup(target)

  expect_named(lookup, c("genus", "genus_nchar"))
  expect_equal(sort(lookup$genus), c("Acer", "Quercus"))

  out <- wcvpmatch:::prefilter_target_by_genus(
    data.frame(Genus = "Acer", Species = "rubrum"),
    target_df = target,
    genus_index = lookup,
    include_fuzzy = FALSE
  )
  expect_true(isTRUE(attr(out, "wcvpmatch_prepared")))
  expect_true(all(out$genus == "Acer"))
})

test_that("deferred context keys preserve accepted-name resolution", {
  out <- wcvp_matching(
    classify_spnames("Acer rubrum"),
    target_df = make_performance_target()
  )

  expect_true(out$matched)
  expect_equal(out$matched_taxon_name, "Acer rubrum")
  expect_equal(out$accepted_taxon_name, "Acer rubrum")
})
