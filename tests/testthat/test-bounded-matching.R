test_that("bounded pairs agree with exhaustive distances for all supported metrics", {
  query <- c("alba", "abcdefghij", "a\u00e9c", NA_character_, "none")
  candidate <- c("alga", "alca", "albaa", "abcdxfghij", "abcdefghik", "aec", "a\u00e9b", NA)
  qgroup <- c("a", "b", "a", "a", "missing")
  cgroup <- c("a", "a", "a", "b", "b", "a", "a", "a")
  for (method in c("osa", "levenshtein", "dl", "hamming", "lcs", "qgram",
                   "cosine", "jaccard", "jaro", "jw", "soundex")) {
    for (short in c(FALSE, TRUE)) {
      expected <- lapply(seq_along(query), function(i) {
        ids <- which(cgroup == qgroup[i] & !is.na(candidate))
        if (is.na(query[i]) || !length(ids)) return(NULL)
        if (short && nchar(query[i]) <= 7) ids <- ids[nchar(candidate[ids]) == nchar(query[i])]
        d <- suppressWarnings(.pairwise_string_distance(rep(query[i], length(ids)), candidate[ids], method))
        ok <- which(!is.na(d) & d <= 2)
        if (!length(ok)) return(NULL)
        best <- ok[d[ok] == min(d[ok])]
        tibble::tibble(query = rep.int(i, length(best)), candidate = ids[best], distance = d[best])
      })
      expected <- dplyr::bind_rows(tibble::tibble(query = integer(), candidate = integer(), distance = double()),
        dplyr::bind_rows(expected))
      actual <- suppressWarnings(.nearest_pairs(query, candidate, qgroup, cgroup, method, 2,
        short_rule = short, block_size = 2L))
      expect_equal(actual, expected, info = paste(method, short))
    }
  }
})

test_that("distance batches are bounded and minima and ties span blocks", {
  original <- .pairwise_string_distance
  sizes <- integer()
  local_mocked_bindings(.pairwise_string_distance = function(a, b, method) {
    sizes <<- c(sizes, length(a))
    original(a, b, method)
  })
  out <- .nearest_pairs("abcd", c("zzzz", "abce", "abcf", "abcd", "abcd"),
    "g", rep("g", 5), "osa", 4, block_size = 2)
  expect_true(all(sizes <= 2))
  expect_identical(out$candidate, c(4L, 5L))
  expect_equal(out$distance, c(0, 0))
})

test_that("prepared indexes invalidate on edits and row subsets", {
  target <- get_db(tibble::tibble(genus = c("Acer", "Quercus"), species = c("rubrum", "robur")))
  keys <- .species_candidate_keys(target, "species")
  expect_identical(.species_candidate_keys(target, "species"), keys)
  edited <- target
  edited$species[1] <- "saccharum"
  expect_identical(.species_candidate_keys(edited, "species")$species, c("saccharum", "robur"))
  subset <- get_db(target[2, ])
  expect_identical(.species_candidate_keys(subset, "species")$genus, "Quercus")
  expect_identical(.species_candidate_keys(target, "species"), keys)
  edited <- target
  edited$infraspecific_rank[1] <- "VAR."
  edited$infraspecies[1] <- "minor"
  expect_identical(.species_candidate_keys(edited, "species")$genus, "Quercus")
  expect_identical(.species_candidate_keys(get_db(edited), "species")$genus, "Quercus")
})

test_that("the default target slot reuses only identical columns", {
  local_mocked_bindings(.wcvpmatch_cache = new.env(parent = emptyenv()))
  target <- get_db(tibble::tibble(genus = "Acer", species = "rubrum"))
  first <- .pipeline_target(target, TRUE)
  .target_cached(first, "example", 42)
  same <- .pipeline_target(target, TRUE)
  expect_identical(.target_cached(same, "example", stop("not cached")), 42)
  changed <- target
  changed$species <- "saccharum"
  changed <- .pipeline_target(changed, TRUE)
  expect_identical(.target_cached(changed, "example", 7), 7)
  expect_identical(.wcvpmatch_cache$last_matching_indexes,
    attr(changed, "wcvpmatch_indexes"))
})

test_that("vectorized input names preserve supplied names and component spacing", {
  input <- tibble::tibble(Genus = c("Acer", "Acer", "Acer"),
    Species = c("rubrum", "a  b", NA_character_),
    Infraspecies = c("minor", NA_character_, NA_character_),
    Infra.Rank = c("var.", NA_character_, NA_character_),
    Input.Name = c(" supplied name ", "", NA_character_))
  expect_identical(check_df_format(input)$Input.Name, c("supplied name", "Acer a  b", "Acer"))
})

test_that("accepted ID indexes preserve match semantics and invalidate", {
  target <- .cache_target(tibble::tibble(plant_name_id = c(5, NA, 2, 5, 9)))
  ids <- c(NA, 5, 2, 9, 10, 0)
  expect_identical(.accepted_positions(ids, target), match(ids, target$plant_name_id))
  target$plant_name_id[1] <- 4
  expect_identical(.accepted_positions(ids, target), match(ids, target$plant_name_id))
})

test_that("duplicate taxon keys preserve submitted authorship in context", {
  target <- tibble::tibble(plant_name_id = c(1, 2, 3), genus = "Acer", species = "rubrum",
    taxon_name = c("Acer rubrum A.", "Acer rubrum B.", "Acer rubrum"),
    taxon_authors = c("A.", "B.", ""), taxon_status = c("Synonym", "Synonym", "Accepted"),
    accepted_plant_name_id = 3)
  input <- tibble::tibble(Genus = "Acer", Species = "rubrum",
    Input.Name = rep(target$taxon_name, 10))
  out <- wcvp_matching(input, target_df = target, allow_duplicates = TRUE)
  expect_equal(out$matched_plant_name_id, rep(c(1, 2, 3), 10))
  expect_equal(out$accepted_plant_name_id, rep(3, 30))
  expect_equal(out$input_index, seq_len(30))
})

test_that("pipeline formats inputs once but standalone nodes still normalize", {
  original <- check_df_format
  calls <- 0L
  local_mocked_bindings(check_df_format = function(df) {
    calls <<- calls + 1L
    original(df)
  })
  target <- tibble::tibble(genus = "Acer", species = "rubrum")
  input <- tibble::tibble(Genus = " Acer ", Species = "rubrun")
  suppressWarnings(wcvp_matching(input, target_df = target))
  expect_identical(calls, 1L)
  suppressWarnings(wcvp_direct_match(input, target_df = target))
  expect_identical(calls, 2L)
})
