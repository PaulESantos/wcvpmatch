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

test_that("certified default backbones bypass component normalization", {
  target <- make_performance_target()
  attr(target, "wcvpdata_matching_schema") <- "1"
  expected <- normalize_target_df(target)
  local_mocked_bindings(
    normalize_target_df = function(...) stop("Unexpected normalization")
  )
  expect_identical(.normalize_default_target_df(target), expected)
})

test_that("legacy and unknown default schemas still normalize", {
  target <- make_performance_target()
  target$genus[1] <- " Acer "
  target$infraspecific_rank[1] <- "ssp"
  target$infraspecies[1] <- " "
  for (schema in list(NULL, "2", 1L)) {
    attr(target, "wcvpdata_matching_schema") <- schema
    expect_identical(.normalize_default_target_df(target), normalize_target_df(target))
  }
})

test_that("schema shortcuts require the expected columns and types", {
  target <- make_performance_target()
  attr(target, "wcvpdata_matching_schema") <- "1"
  target$genus <- factor(target$genus)
  expect_identical(.normalize_default_target_df(target), normalize_target_df(target))
  target$infraspecies <- NULL
  expect_identical(.normalize_default_target_df(target), normalize_target_df(target))
})

test_that("custom targets do not trust inherited provider certification", {
  target <- make_performance_target()
  attr(target, "wcvpdata_matching_schema") <- "1"
  target$genus[1] <- " Acer "
  target$infraspecific_rank[1] <- "ssp"
  target$infraspecies[1] <- " rubrum "
  out <- get_db(target)
  expect_identical(out$genus[1], "Acer")
  expect_identical(out$infraspecific_rank[1], "SUBSP.")
  expect_identical(out$infraspecies[1], "rubrum")
})

test_that("the default loader uses certified data and caches the result", {
  target <- make_performance_target()
  attr(target, "wcvpdata_matching_schema") <- "1"
  reads <- 0L
  local_mocked_bindings(
    .wcvpmatch_cache = new.env(parent = emptyenv()),
    .require_wcvpdata = function() invisible(TRUE),
    .wcvpmatch_read_wcvpdata_table = function(...) {
      reads <<- reads + 1L
      target
    },
    normalize_target_df = function(...) stop("Unexpected normalization")
  )
  first <- get_db()
  expect_identical(get_db(), first)
  expect_identical(reads, 1L)
  expect_true(is_normalized_target_df(first))
  expect_true(isTRUE(attr(first, "wcvpmatch_prepared")))
  expect_identical(first$Genus, first$genus)
})

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
