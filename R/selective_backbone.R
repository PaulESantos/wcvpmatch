# Read only selected genera from the optional, versioned Arrow backbone.
.default_selective_dataset <- function() {
  if (!requireNamespace("wcvpdata", quietly = TRUE)) return(NULL)
  accessor <- tryCatch(getExportedValue("wcvpdata", "wcvp_matching_names"),
    error = function(e) NULL)
  if (!is.function(accessor)) return(NULL)
  dataset <- tryCatch(accessor(), error = function(e) NULL)
  if (is.null(dataset) || !inherits(dataset, "Dataset")) return(NULL)
  version <- tryCatch(dataset$schema$metadata[["wcvpdata.matching_schema_version"]],
    error = function(e) NULL)
  if (!identical(version, "1")) return(NULL)
  dataset
}

.default_genus_catalog <- function(dataset) {
  cached <- .wcvpmatch_cache[["selective_genus_catalog"]]
  if (!is.null(cached)) return(cached)
  genera <- dataset %>% dplyr::select(genus) %>% dplyr::distinct() %>%
    dplyr::collect() %>% dplyr::filter(!is.na(genus), nzchar(genus))
  out <- tibble::tibble(genus = genera$genus,
    genus_nchar = nchar(genera$genus))
  .wcvpmatch_cache[["selective_genus_catalog"]] <- out
  out
}

.default_candidate_genera <- function(df, catalog, max_dist, method) {
  input <- unique(df$Orig.Genus)
  input <- input[!is.na(input) & nzchar(input)]
  exact <- input[input %in% catalog$genus]
  unresolved <- setdiff(input, exact)
  fuzzy <- tibble::tibble(Orig.Genus = character(), genus = character(),
    fuzzy_genus_dist = numeric())
  if (length(unresolved)) {
    candidates <- catalog
    if (.is_edit_distance_method(method)) {
      input_len <- nchar(unresolved)
      allowed <- if (tolower(method) == "hamming") unique(input_len) else
        unique(unlist(lapply(input_len, function(n) seq.int(max(0, n - max_dist), n + max_dist))))
      candidates <- dplyr::filter(candidates, genus_nchar %in% allowed)
    }
    fuzzy <- tibble::tibble(Orig.Genus = unresolved) %>%
      fozziejoin::fozzie_string_left_join(
        dplyr::select(candidates, genus), by = c("Orig.Genus" = "genus"),
        max_distance = max_dist, method = method,
        distance_col = "fuzzy_genus_dist") %>%
      dplyr::filter(!is.na(genus), !is.na(fuzzy_genus_dist),
        fuzzy_genus_dist <= max_dist) %>%
      dplyr::group_by(Orig.Genus) %>%
      dplyr::slice_min(fuzzy_genus_dist, n = 1, with_ties = TRUE) %>%
      dplyr::ungroup()
  }
  list(exact = exact, fuzzy = fuzzy,
    candidates = unique(c(exact, fuzzy$genus)))
}

.default_selective_target <- function(df, dataset, max_dist, method) {
  catalog <- .default_genus_catalog(dataset)
  selected <- .default_candidate_genera(df, catalog, max_dist, method)
  columns <- c("plant_name_id", "taxon_rank", "taxon_status", "family",
    "genus", "species", "infraspecific_rank", "infraspecies", "taxon_name",
    "taxon_authors", "accepted_plant_name_id", "parent_plant_name_id")
  cached <- .wcvpmatch_cache[["selective_target_rows"]]
  if (!is.null(cached) && identical(cached$genera, selected$candidates)) {
    target <- cached$target
  } else {
    query <- dataset %>% dplyr::select(dplyr::all_of(columns)) %>%
      dplyr::filter(genus %in% selected$candidates)
    rows <- dplyr::collect(query)
    attr(rows, "wcvpdata_matching_schema") <- "1"
    target <- prepare_target_db(.normalize_default_target_df(rows))
    .wcvpmatch_cache[["selective_target_rows"]] <- list(
      genera = selected$candidates, target = target)
  }
  attr(target, "wcvpmatch_source") <- "selective_default"
  attr(target, "candidate_genera") <- selected$candidates
  attr(target, "exact_genera") <- selected$exact
  attr(target, "fuzzy_genera") <- unique(selected$fuzzy$genus)
  attr(target, "fuzzy_genus_map") <- selected$fuzzy
  attr(target, "fuzzy_genus_method") <- method
  attr(target, "fuzzy_genus_max_dist") <- max_dist
  attr(target, "fuzzy_genus_target_id") <- .target_id(target)
  target
}

.default_accepted_rows <- function(dataset, ids) {
  columns <- c("plant_name_id", "taxon_name", "taxon_authors")
  dataset %>% dplyr::select(dplyr::all_of(columns)) %>%
    dplyr::filter(plant_name_id %in% ids) %>% dplyr::collect()
}
