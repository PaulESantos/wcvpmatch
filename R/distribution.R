#' Retrieve WCVP Distribution by Species, Genus, or Family
#'
#' `r lifecycle::badge("stable")`
#'
#' Queries distribution records by matching a taxon name against the WCVP names
#' table and then resolving the corresponding rows in the WCVP distribution
#' table. The function is designed around `wcvpdata::wcvp_checklist_names` and
#' `wcvpdata::wcvp_checklist_distribution`, but custom tables with the same
#' schema can also be supplied.
#'
#' Matching is performed with `fozziejoin`, using compact lookup tables and
#' length-based prefiltering to keep the candidate set small. Species queries are
#' resolved in two stages: genus candidates are matched first, then species
#' names are searched only within those candidate genera.
#'
#' If species-level matches resolve to synonyms and the names table contains
#' `accepted_plant_name_id`, distribution is recovered from the accepted taxon.
#' For genus- and family-level queries, accepted names are preferred to avoid
#' double counting synonym records.
#'
#' @param taxon Character vector of taxa to query.
#' @param taxon_rank Character scalar. One of `"species"`, `"genus"`, or
#'   `"family"`.
#' @param native Logical. Include native occurrences? Defaults to `TRUE`.
#' @param introduced Logical. Include introduced occurrences? Defaults to
#'   `TRUE`.
#' @param extinct Logical. Include extinct occurrences? Defaults to `TRUE`.
#' @param location_doubtful Logical. Include doubtful occurrences? Defaults to
#'   `TRUE`.
#' @param wcvp_names Optional WCVP names table. If `NULL`, the function loads
#'   `wcvpdata::wcvp_checklist_names`.
#' @param wcvp_distributions Optional WCVP distribution table. If `NULL`, the
#'   function loads `wcvpdata::wcvp_checklist_distribution`.
#' @param prefilter_genus Logical. Forwarded to `wcvp_matching()` for
#'   species-level queries. Ignored for genus/family lookups.
#' @param fallback_to_genus Logical. If `TRUE` and `taxon_rank = "species"`,
#'   inputs without species-level distribution are retried at genus level.
#' @param summarise_by_input Logical. If `TRUE`, return one row per input taxon
#'   with collapsed distribution fields. In this mode, `area_codes`, `areas`,
#'   `regions`, `continents`, and `distribution` are returned as character
#'   strings separated by `" - "` rather than list-columns.
#' @param max_dist Maximum string distance. If `NULL`, species queries default
#'   to `2` and genus/family queries to `0`.
#' @param method String distance method passed to `fozziejoin`.
#'
#' @return By default, a tibble with one row per matched query-area combination.
#'   If `summarise_by_input = TRUE`, returns one row per input taxon with
#'   collapsed text fields such as `distribution`, `areas`, `area_codes`,
#'   `regions`, `continents`, and `n_areas`.
#'
#' @examplesIf rlang::is_installed("wcvpdata")
#' \donttest{
#' library(wcvpmatch)
#'
#' wcvp_distribution("Opuntia ficus-indica", taxon_rank = "species")
#' wcvp_distribution("Opuntia", taxon_rank = "genus")
#' wcvp_distribution("Cactaceae", taxon_rank = "family")
#' }
#' @export
wcvp_distribution <- function(taxon,
                              taxon_rank = c("species", "genus", "family"),
                              native = TRUE,
                              introduced = TRUE,
                              extinct = TRUE,
                              location_doubtful = TRUE,
                              wcvp_names = NULL,
                              wcvp_distributions = NULL,
                              prefilter_genus = TRUE,
                              fallback_to_genus = TRUE,
                              summarise_by_input = FALSE,
                              max_dist = NULL,
                              method = "osa") {
  taxon_rank <- match.arg(taxon_rank)
  max_dist <- resolve_distribution_max_dist(taxon_rank = taxon_rank, max_dist = max_dist)

  assertthat::assert_that(
    is.character(taxon),
    msg = "taxon must be a character vector."
  )
  assertthat::assert_that(
    length(taxon) > 0,
    msg = "taxon must contain at least one value."
  )
  names_tbl <- if (is.null(wcvp_names)) {
    default_distribution_names()
  } else {
    normalize_distribution_names(wcvp_names)
  }

  dist_tbl <- if (is.null(wcvp_distributions)) {
    default_distribution_records()
  } else {
    normalize_distribution_records(wcvp_distributions)
  }

  query_tbl <- normalize_distribution_query(taxon, taxon_rank = taxon_rank)

  if (identical(taxon_rank, "species")) {
    name_hits <- match_species_distribution_with_backend(
      taxon = taxon,
      names_tbl = names_tbl,
      prefilter_genus = prefilter_genus,
      max_dist = max_dist,
      method = method
    )
  } else {
    matches <- match_distribution_queries(
      query_tbl = query_tbl,
      lookup_tbl = build_distribution_lookup(names_tbl, taxon_rank = taxon_rank),
      query_col = "query_value",
      lookup_col = "matched_taxon",
      length_col = "taxon_nchar",
      max_dist = max_dist,
      method = method,
      distance_col = "match_distance"
    )

    name_hits <- resolve_distribution_name_hits(
      matches = matches,
      names_tbl = names_tbl,
      taxon_rank = taxon_rank
    )
  }

  out <- assemble_distribution_output(
    query_tbl = query_tbl,
    name_hits = name_hits,
    dist_tbl = dist_tbl,
    taxon_rank = taxon_rank,
    native = native,
    introduced = introduced,
    extinct = extinct,
    location_doubtful = location_doubtful,
    summarise_by_input = summarise_by_input
  )

  if (identical(taxon_rank, "species") && isTRUE(fallback_to_genus)) {
    out <- apply_genus_distribution_fallback(
      out = out,
      query_tbl = query_tbl,
      names_tbl = names_tbl,
      dist_tbl = dist_tbl,
      native = native,
      introduced = introduced,
      extinct = extinct,
      location_doubtful = location_doubtful,
      summarise_by_input = summarise_by_input,
      max_dist = max_dist,
      method = method
    )
  }

  finalize_distribution_output(out)
}

resolve_distribution_max_dist <- function(taxon_rank, max_dist) {
  if (is.null(max_dist)) {
    return(if (identical(taxon_rank, "species")) 2 else 0)
  }

  assertthat::assert_that(
    is.numeric(max_dist),
    length(max_dist) == 1,
    !is.na(max_dist),
    max_dist >= 0,
    msg = "max_dist must be NULL or a single non-negative number."
  )

  as.numeric(max_dist)
}

default_distribution_names <- function() {
  cached <- .wcvpmatch_cache[["default_distribution_names"]]
  if (!is.null(cached)) {
    return(cached)
  }

  .require_wcvpdata()

  env <- new.env(parent = emptyenv())
  utils::data("wcvp_checklist_names", package = "wcvpdata", envir = env)

  if (!exists("wcvp_checklist_names", envir = env, inherits = FALSE)) {
    cli::cli_abort(c(
      "x" = "Object {.val wcvp_checklist_names} was not found in package {.pkg wcvpdata}.",
      "i" = "Reinstall or update {.pkg wcvpdata} from {.url https://paulesantos.r-universe.dev}.",
      "i" = "Or pass the names table explicitly with {.arg wcvp_names}."
    ))
  }

  out <- normalize_distribution_names(get("wcvp_checklist_names", envir = env, inherits = FALSE))
  .wcvpmatch_cache[["default_distribution_names"]] <- out
  out
}

default_distribution_records <- function() {
  cached <- .wcvpmatch_cache[["default_distribution_records"]]
  if (!is.null(cached)) {
    return(cached)
  }

  .require_wcvpdata()

  env <- new.env(parent = emptyenv())
  utils::data("wcvp_checklist_distribution", package = "wcvpdata", envir = env)

  if (!exists("wcvp_checklist_distribution", envir = env, inherits = FALSE)) {
    cli::cli_abort(c(
      "x" = "Object {.val wcvp_checklist_distribution} was not found in package {.pkg wcvpdata}.",
      "i" = "Reinstall or update {.pkg wcvpdata} from {.url https://paulesantos.r-universe.dev}.",
      "i" = "Or pass the distribution table explicitly with {.arg wcvp_distributions}."
    ))
  }

  out <- normalize_distribution_records(get("wcvp_checklist_distribution", envir = env, inherits = FALSE))
  .wcvpmatch_cache[["default_distribution_records"]] <- out
  out
}

normalize_distribution_names <- function(wcvp_names) {
  assertthat::assert_that(
    inherits(wcvp_names, "data.frame"),
    msg = "wcvp_names must be a data.frame/tibble."
  )

  x <- tibble::as_tibble(wcvp_names)

  if (!("family" %in% names(x)) && "Family" %in% names(x)) {
    names(x)[names(x) == "Family"] <- "family"
  }
  if (!("genus" %in% names(x)) && "Genus" %in% names(x)) {
    x <- dplyr::rename(x, genus = Genus)
  }
  if (!("species" %in% names(x)) && "Species" %in% names(x)) {
    x <- dplyr::rename(x, species = Species)
  }

  required <- c("plant_name_id", "family", "genus", "species")
  assertthat::assert_that(
    all(required %in% names(x)),
    msg = paste(
      "wcvp_names must contain:",
      paste(required, collapse = ", ")
    )
  )

  if (!"accepted_plant_name_id" %in% names(x)) x$accepted_plant_name_id <- NA_real_
  if (!"taxon_status" %in% names(x)) x$taxon_status <- NA_character_
  if (!"taxon_rank" %in% names(x)) x$taxon_rank <- NA_character_
  if (!"taxon_name" %in% names(x)) {
    x <- x %>%
      dplyr::mutate(
        taxon_name = dplyr::if_else(
          !is.na(.data$genus) & !is.na(.data$species),
          paste(.data$genus, .data$species),
          NA_character_
        )
      )
  }

  x %>%
    dplyr::mutate(
      plant_name_id = as.numeric(plant_name_id),
      accepted_plant_name_id = as.numeric(accepted_plant_name_id),
      family = stringr::str_squish(as.character(.data$family)),
      genus = stringr::str_squish(as.character(.data$genus)),
      species = stringr::str_squish(as.character(.data$species)),
      taxon_rank = as.character(.data$taxon_rank),
      taxon_status = as.character(.data$taxon_status),
      taxon_name = stringr::str_squish(as.character(.data$taxon_name)),
      species_key = dplyr::if_else(
        !is.na(.data$genus) & nzchar(.data$genus) & !is.na(.data$species) & nzchar(.data$species),
        paste(.data$genus, .data$species),
        NA_character_
      )
    )
}

normalize_distribution_records <- function(wcvp_distributions) {
  assertthat::assert_that(
    inherits(wcvp_distributions, "data.frame"),
    msg = "wcvp_distributions must be a data.frame/tibble."
  )

  x <- tibble::as_tibble(wcvp_distributions)

  required <- c(
    "plant_name_id",
    "continent_code_l1", "continent",
    "region_code_l2", "region",
    "area_code_l3", "area",
    "introduced", "extinct", "location_doubtful"
  )

  assertthat::assert_that(
    all(required %in% names(x)),
    msg = paste(
      "wcvp_distributions must contain:",
      paste(required, collapse = ", ")
    )
  )

  x %>%
    dplyr::mutate(
      plant_name_id = as.numeric(plant_name_id),
      continent_code_l1 = as.character(continent_code_l1),
      continent = as.character(continent),
      region_code_l2 = as.character(region_code_l2),
      region = as.character(region),
      area_code_l3 = as.character(area_code_l3),
      area = as.character(area),
      introduced = as.numeric(introduced),
      extinct = as.numeric(extinct),
      location_doubtful = as.numeric(location_doubtful)
    )
}

normalize_distribution_query <- function(taxon, taxon_rank) {
  vals <- stringr::str_squish(as.character(taxon))
  vals <- vals[!is.na(vals) & nzchar(vals)]

  if (length(vals) == 0) {
    cli::cli_abort("taxon must contain at least one non-empty value.")
  }

  out <- tibble::tibble(
    input_index = seq_along(vals),
    query = vals,
    query_value = vals
  )

  if (identical(taxon_rank, "species")) {
    parts <- stringr::str_split(vals, "\\s+")
    genus <- vapply(
      parts,
      function(z) if (length(z) >= 1L) z[[1]] else NA_character_,
      FUN.VALUE = character(1)
    )
    species <- vapply(
      parts,
      function(z) if (length(z) >= 2L) z[[2]] else NA_character_,
      FUN.VALUE = character(1)
    )

    n_incomplete <- sum(is.na(species) | !nzchar(species))
    if (n_incomplete > 0) {
      cli::cli_inform(
        "{n_incomplete} input{?s} did not contain a complete species binomial; matching will continue and may fall back to genus-level distribution."
      )
    }

    out <- out %>%
      dplyr::mutate(
        query_genus = stringr::str_to_title(genus),
        query_species = stringr::str_to_lower(species),
        query_value = dplyr::if_else(
          !is.na(query_species) & nzchar(query_species),
          paste(query_genus, query_species),
          query_genus
        )
      )
  } else if (identical(taxon_rank, "genus")) {
    out <- out %>%
      dplyr::mutate(
        query_value = stringr::str_to_title(query_value)
      )
  } else {
    out <- out %>%
      dplyr::mutate(
        query_value = stringr::str_to_title(query_value)
      )
  }

  out %>%
    dplyr::mutate(
      query_nchar = nchar(query_value)
    )
}

build_distribution_lookup <- function(names_tbl, taxon_rank) {
  if (identical(taxon_rank, "genus")) {
    return(
      names_tbl %>%
        dplyr::filter(!is.na(genus), nzchar(genus)) %>%
        dplyr::distinct(matched_taxon = genus) %>%
        dplyr::mutate(taxon_nchar = nchar(matched_taxon))
    )
  }

  if (identical(taxon_rank, "family")) {
    return(
      names_tbl %>%
        dplyr::filter(!is.na(.data$family), nzchar(.data$family)) %>%
        dplyr::distinct(matched_taxon = .data$family) %>%
        dplyr::mutate(taxon_nchar = nchar(matched_taxon))
    )
  }

  names_tbl %>%
    dplyr::filter(!is.na(species_key), nzchar(species_key)) %>%
    dplyr::filter(is.na(.data$taxon_rank) | tolower(.data$taxon_rank) == "species") %>%
    dplyr::distinct(matched_taxon = species_key, genus) %>%
    dplyr::mutate(taxon_nchar = nchar(matched_taxon))
}

match_distribution_queries <- function(query_tbl,
                                       lookup_tbl,
                                       query_col,
                                       lookup_col,
                                       length_col,
                                       max_dist,
                                       method,
                                       distance_col) {
  lookup_work <- prefilter_distribution_lookup(
    lookup_tbl = lookup_tbl,
    query_lengths = query_tbl$query_nchar,
    length_col = length_col,
    max_dist = max_dist,
    method = method
  )

  if (nrow(lookup_work) == 0) {
    return(tibble::tibble())
  }

  by_map <- stats::setNames(lookup_col, query_col)

  out <- query_tbl %>%
    fozziejoin::fozzie_string_left_join(
      lookup_work,
      by = by_map,
      max_distance = max_dist,
      method = method,
      distance_col = distance_col
    ) %>%
    dplyr::filter(!is.na(.data[[lookup_col]]), !is.na(.data[[distance_col]])) %>%
    dplyr::group_by(input_index, query) %>%
    dplyr::slice_min(order_by = .data[[distance_col]], n = 1, with_ties = TRUE) %>%
    dplyr::ungroup() %>%
    dplyr::select(
      input_index,
      query,
      matched_taxon = dplyr::all_of(lookup_col),
      match_distance = dplyr::all_of(distance_col)
    ) %>%
    dplyr::distinct()

  out
}

match_species_distribution_queries <- function(query_tbl,
                                               names_tbl,
                                               max_dist,
                                               method) {
  genus_queries <- query_tbl %>%
    dplyr::distinct(query, query_value, query_genus) %>%
    dplyr::transmute(
      query = query,
      query_value = query_genus,
      query_nchar = nchar(query_value)
    )

  genus_matches <- match_distribution_queries(
    query_tbl = genus_queries,
    lookup_tbl = build_distribution_lookup(names_tbl, taxon_rank = "genus"),
    query_col = "query_value",
    lookup_col = "matched_taxon",
    length_col = "taxon_nchar",
    max_dist = max_dist,
    method = method,
    distance_col = "genus_distance"
  ) %>%
    dplyr::rename(matched_genus = matched_taxon, genus_distance = match_distance)

  if (nrow(genus_matches) == 0) {
    return(tibble::tibble())
  }

  species_lookup <- build_distribution_lookup(names_tbl, taxon_rank = "species")

  purrr::pmap_dfr(
    query_tbl,
    function(query, query_value, query_genus, query_species, query_nchar) {
      candidate_genera <- genus_matches %>%
        dplyr::filter(query == .env$query) %>%
        dplyr::pull(matched_genus) %>%
        unique()

      if (length(candidate_genera) == 0) {
        return(tibble::tibble())
      }

      lookup_work <- species_lookup %>%
        dplyr::filter(genus %in% .env$candidate_genera) %>%
        prefilter_distribution_lookup(
          query_lengths = query_nchar,
          length_col = "taxon_nchar",
          max_dist = max_dist,
          method = method
        )

      if (nrow(lookup_work) == 0) {
        return(tibble::tibble())
      }

      matched_one <- tibble::tibble(
        query = query,
        query_value = query_value,
        query_nchar = query_nchar
      ) %>%
        fozziejoin::fozzie_string_left_join(
          lookup_work,
          by = c("query_value" = "matched_taxon"),
          max_distance = max_dist,
          method = method,
          distance_col = "species_distance"
        ) %>%
        dplyr::filter(!is.na(matched_taxon), !is.na(species_distance)) %>%
        dplyr::group_by(query) %>%
        dplyr::filter(species_distance == min(species_distance, na.rm = TRUE)) %>%
        dplyr::ungroup() %>%
        dplyr::left_join(genus_matches, by = "query") %>%
        dplyr::filter(genus %in% matched_genus) %>%
        dplyr::transmute(
          query = query,
          matched_taxon = matched_taxon,
          match_distance = genus_distance + species_distance
        ) %>%
        dplyr::distinct()

      matched_one
    }
  )
}

prefilter_distribution_lookup <- function(lookup_tbl,
                                          query_lengths,
                                          length_col,
                                          max_dist,
                                          method) {
  out <- lookup_tbl

  if (!.is_edit_distance_method(method)) {
    return(out)
  }

  query_lengths <- unique(as.integer(query_lengths))

  allowed_lengths <- if (tolower(method) == "hamming") {
    query_lengths
  } else {
    unique(unlist(lapply(
      query_lengths,
      function(x) seq.int(max(0L, x - as.integer(max_dist)), x + as.integer(max_dist))
    )))
  }

  out %>%
    dplyr::filter(.data[[length_col]] %in% .env$allowed_lengths)
}

match_species_distribution_with_backend <- function(taxon,
                                                    names_tbl,
                                                    prefilter_genus,
                                                    max_dist,
                                                    method) {
  matched <- classify_spnames(taxon) %>%
    wcvp_matching(
      target_df = names_tbl,
      prefilter_genus = prefilter_genus,
      allow_duplicates = TRUE,
      max_dist = max_dist,
      method = method,
      add_name_distance = TRUE,
      output_name_style = "snake_case"
    )

  matched %>%
    dplyr::transmute(
      input_index = .data$input_index,
      query = .data$input_name,
      matched = dplyr::coalesce(.data$matched, FALSE),
      matched_taxon = dplyr::coalesce(.data$accepted_taxon_name, .data$matched_taxon_name),
      match_distance = .data$matched_dist,
      resolved_plant_name_id = dplyr::coalesce(.data$accepted_plant_name_id, .data$matched_plant_name_id),
      resolved_taxon_name = dplyr::coalesce(.data$accepted_taxon_name, .data$matched_taxon_name)
    )
}

resolve_distribution_name_hits <- function(matches, names_tbl, taxon_rank) {
  if (nrow(matches) == 0) {
    return(tibble::tibble())
  }

  accepted_lookup <- names_tbl %>%
    dplyr::select(
      accepted_lookup_id = .data$plant_name_id,
      accepted_taxon_name = .data$taxon_name,
      accepted_family = .data$family,
      accepted_genus = .data$genus,
      accepted_species = .data$species
    ) %>%
    dplyr::distinct()

  if (identical(taxon_rank, "species")) {
    species_rows <- names_tbl %>%
      dplyr::filter(!is.na(species_key), nzchar(species_key)) %>%
      dplyr::filter(is.na(.data$taxon_rank) | tolower(.data$taxon_rank) == "species")

    out <- matches %>%
      dplyr::inner_join(
        species_rows,
        by = c("matched_taxon" = "species_key")
      ) %>%
      dplyr::mutate(
        resolved_plant_name_id = dplyr::coalesce(accepted_plant_name_id, plant_name_id)
      ) %>%
      dplyr::left_join(
        accepted_lookup,
        by = c("resolved_plant_name_id" = "accepted_lookup_id")
      ) %>%
      dplyr::mutate(
        resolved_taxon_name = dplyr::coalesce(accepted_taxon_name, taxon_name)
      ) %>%
      dplyr::select(
        input_index, query, matched_taxon, match_distance,
        matched = TRUE,
        matched_plant_name_id = plant_name_id,
        resolved_plant_name_id,
        matched_taxon_name = taxon_name,
        resolved_taxon_name
      ) %>%
      dplyr::distinct()

    return(out)
  }

  join_col <- if (identical(taxon_rank, "genus")) "genus" else "family"
  base <- names_tbl

  if ("taxon_status" %in% names(base)) {
    accepted_base <- base %>%
      dplyr::filter(is.na(.data$taxon_status) | tolower(.data$taxon_status) == "accepted")

    if (nrow(accepted_base) > 0) {
      base <- accepted_base
    }
  }

  matches %>%
    dplyr::inner_join(
      base,
      by = stats::setNames(join_col, "matched_taxon")
    ) %>%
    dplyr::transmute(
      input_index = .data$input_index,
      query = .data$query,
      matched = TRUE,
      matched_taxon = .data$matched_taxon,
      match_distance = .data$match_distance,
      matched_plant_name_id = .data$plant_name_id,
      resolved_plant_name_id = .data$plant_name_id,
      matched_taxon_name = .data$taxon_name,
      resolved_taxon_name = NA_character_
    ) %>%
    dplyr::distinct()
}

filter_distribution_records <- function(df,
                                        native,
                                        introduced,
                                        extinct,
                                        location_doubtful) {
  out <- df

  native_flag <- out$introduced + out$extinct + out$location_doubtful == 0

  if (!isTRUE(native)) {
    out <- out[native_flag == FALSE, , drop = FALSE]
  }
  if (!isTRUE(introduced)) {
    out <- out[out$introduced == 0, , drop = FALSE]
  }
  if (!isTRUE(extinct)) {
    out <- out[out$extinct == 0, , drop = FALSE]
  }
  if (!isTRUE(location_doubtful)) {
    out <- out[out$location_doubtful == 0, , drop = FALSE]
  }

  tibble::as_tibble(out)
}

summarise_distribution_records <- function(df, taxon_rank) {
  df %>%
    dplyr::group_by(
      input_index,
      query,
      taxon_rank,
      matched_taxon,
      match_distance,
      continent_code_l1,
      continent,
      region_code_l2,
      region,
      area_code_l3,
      area
    ) %>%
    dplyr::summarise(
      matched = TRUE,
      resolved_taxon_name = distribution_label(resolved_taxon_name),
      native = any(introduced + extinct + location_doubtful == 0, na.rm = TRUE),
      introduced = any(introduced == 1, na.rm = TRUE),
      extinct = any(extinct == 1, na.rm = TRUE),
      location_doubtful = any(location_doubtful == 1, na.rm = TRUE),
      occurrence_type = determine_occurrence_type_(introduced, extinct, location_doubtful),
      n_plant_name_ids = dplyr::n_distinct(resolved_plant_name_id),
      distribution_status = "distribution_found",
      .groups = "drop"
    ) %>%
    dplyr::arrange(input_index, matched_taxon, continent, region, area)
}

assemble_distribution_output <- function(query_tbl,
                                         name_hits,
                                         dist_tbl,
                                         taxon_rank,
                                         native,
                                         introduced,
                                         extinct,
                                         location_doubtful,
                                         summarise_by_input = FALSE) {
  matched_hits <- name_hits %>%
    dplyr::filter(dplyr::coalesce(.data$matched, FALSE))

  joined_all <- matched_hits %>%
    dplyr::left_join(
      dist_tbl,
      by = c("resolved_plant_name_id" = "plant_name_id")
    ) %>%
    dplyr::mutate(taxon_rank = taxon_rank)

  pre_filter_ids <- joined_all %>%
    dplyr::filter(!is.na(area_code_l3)) %>%
    dplyr::distinct(input_index)

  found_raw <- joined_all %>%
    dplyr::filter(!is.na(area_code_l3))

  found_filtered <- filter_distribution_records(
    found_raw,
    native = native,
    introduced = introduced,
    extinct = extinct,
    location_doubtful = location_doubtful
  )

  post_filter_ids <- found_filtered %>%
    dplyr::distinct(input_index)

  found <- if (nrow(found_filtered) > 0) {
    summarise_distribution_records(found_filtered, taxon_rank = taxon_rank)
  } else {
    tibble::tibble()
  }

  no_distribution <- matched_hits %>%
    dplyr::anti_join(pre_filter_ids, by = "input_index") %>%
    dplyr::transmute(
      input_index = input_index,
      query = query,
      taxon_rank = taxon_rank,
      matched = TRUE,
      matched_taxon = matched_taxon,
      match_distance = match_distance,
      resolved_taxon_name = resolved_taxon_name,
      continent_code_l1 = NA_character_,
      continent = NA_character_,
      region_code_l2 = NA_character_,
      region = NA_character_,
      area_code_l3 = NA_character_,
      area = NA_character_,
      native = as.logical(NA),
      introduced = as.logical(NA),
      extinct = as.logical(NA),
      location_doubtful = as.logical(NA),
      occurrence_type = NA_character_,
      n_plant_name_ids = 0L,
      distribution_status = "no_distribution"
    ) %>%
    dplyr::distinct()

  no_distribution_after_filters <- matched_hits %>%
    dplyr::semi_join(pre_filter_ids, by = "input_index") %>%
    dplyr::anti_join(post_filter_ids, by = "input_index") %>%
    dplyr::transmute(
      input_index = input_index,
      query = query,
      taxon_rank = taxon_rank,
      matched = TRUE,
      matched_taxon = matched_taxon,
      match_distance = match_distance,
      resolved_taxon_name = resolved_taxon_name,
      continent_code_l1 = NA_character_,
      continent = NA_character_,
      region_code_l2 = NA_character_,
      region = NA_character_,
      area_code_l3 = NA_character_,
      area = NA_character_,
      native = as.logical(NA),
      introduced = as.logical(NA),
      extinct = as.logical(NA),
      location_doubtful = as.logical(NA),
      occurrence_type = NA_character_,
      n_plant_name_ids = 0L,
      distribution_status = "no_distribution_after_filters"
    ) %>%
    dplyr::distinct()

  no_match <- query_tbl %>%
    dplyr::anti_join(matched_hits %>% dplyr::distinct(input_index), by = "input_index") %>%
    dplyr::transmute(
      input_index = input_index,
      query = query,
      taxon_rank = taxon_rank,
      matched = FALSE,
      matched_taxon = NA_character_,
      match_distance = NA_real_,
      resolved_taxon_name = NA_character_,
      continent_code_l1 = NA_character_,
      continent = NA_character_,
      region_code_l2 = NA_character_,
      region = NA_character_,
      area_code_l3 = NA_character_,
      area = NA_character_,
      native = as.logical(NA),
      introduced = as.logical(NA),
      extinct = as.logical(NA),
      location_doubtful = as.logical(NA),
      occurrence_type = NA_character_,
      n_plant_name_ids = 0L,
      distribution_status = "no_match"
    )

  out <- dplyr::bind_rows(
    found,
    no_distribution,
    no_distribution_after_filters,
    no_match
  ) %>%
    dplyr::arrange(input_index, matched_taxon, continent, region, area)

  if (isTRUE(summarise_by_input)) {
    return(summarise_distribution_by_input(out))
  }

  out
}

apply_genus_distribution_fallback <- function(out,
                                              query_tbl,
                                              names_tbl,
                                              dist_tbl,
                                              native,
                                              introduced,
                                              extinct,
                                              location_doubtful,
                                              summarise_by_input,
                                              max_dist,
                                              method) {
  unresolved_status <- c("no_match", "no_distribution", "no_distribution_after_filters")

  unresolved <- if (isTRUE(summarise_by_input)) {
    out %>%
      dplyr::filter(distribution_status %in% unresolved_status) %>%
      dplyr::select(input_index, query, distribution_status)
  } else {
    out %>%
      dplyr::filter(distribution_status %in% unresolved_status) %>%
      dplyr::distinct(input_index, query, distribution_status)
  }

  if (nrow(unresolved) == 0) {
    return(out)
  }

  genus_queries <- query_tbl %>%
    dplyr::semi_join(unresolved, by = c("input_index", "query")) %>%
    dplyr::filter(!is.na(query_genus), nzchar(query_genus)) %>%
    dplyr::transmute(
      input_index = input_index,
      query = query,
      query_value = query_genus,
      query_nchar = nchar(query_genus)
    )

  if (nrow(genus_queries) == 0) {
    return(out)
  }

  genus_matches <- match_distribution_queries(
    query_tbl = genus_queries,
    lookup_tbl = build_distribution_lookup(names_tbl, taxon_rank = "genus"),
    query_col = "query_value",
    lookup_col = "matched_taxon",
    length_col = "taxon_nchar",
    max_dist = max_dist,
    method = method,
    distance_col = "match_distance"
  )

  genus_hits <- resolve_distribution_name_hits(
    matches = genus_matches,
    names_tbl = names_tbl,
    taxon_rank = "genus"
  )

  if (nrow(genus_hits) == 0) {
    return(out)
  }

  genus_out <- assemble_distribution_output(
    query_tbl = genus_queries,
    name_hits = genus_hits,
    dist_tbl = dist_tbl,
    taxon_rank = "genus",
    native = native,
    introduced = introduced,
    extinct = extinct,
    location_doubtful = location_doubtful,
    summarise_by_input = summarise_by_input
  )

  genus_out <- relabel_genus_fallback_output(genus_out)

  successful_fallback <- genus_out %>%
    dplyr::filter(distribution_status == "genus_distribution_fallback") %>%
    dplyr::distinct()

  replaced_ids <- successful_fallback %>%
    dplyr::distinct(input_index)

  if (nrow(replaced_ids) == 0) {
    return(out)
  }

  cli::cli_inform(
    "{nrow(replaced_ids)} input{?s} without species-level distribution were resolved with genus-level distribution."
  )

  dplyr::bind_rows(
    out %>% dplyr::anti_join(replaced_ids, by = "input_index"),
    successful_fallback
  ) %>%
    dplyr::arrange(input_index)
}

relabel_genus_fallback_output <- function(df) {
  df %>%
    dplyr::mutate(
      taxon_rank = "species",
      distribution_status = dplyr::if_else(
        distribution_status == "distribution_found",
        "genus_distribution_fallback",
        distribution_status
      )
    )
}

finalize_distribution_output <- function(df) {
  df %>%
    dplyr::rename(
      submited_name = query,
      accepted_taxon_name = resolved_taxon_name
    )
}

summarise_distribution_by_input <- function(df) {
  collapse_unique <- function(x) {
    vals <- unique(as.character(x[!is.na(x) & nzchar(x)]))
    if (length(vals) == 0) {
      return(NA_character_)
    }
    paste(sort(vals), collapse = " - ")
  }

  pick_status <- function(x) {
    vals <- unique(as.character(x[!is.na(x) & nzchar(x)]))
    priority <- c(
      "distribution_found",
      "no_distribution_after_filters",
      "no_distribution",
      "no_match"
    )
    hit <- priority[priority %in% vals]
    if (length(hit) == 0) NA_character_ else hit[[1]]
  }

  df %>%
    dplyr::group_by(input_index, query, .data$taxon_rank) %>%
    dplyr::summarise(
      matched = any(.data$matched %in% TRUE, na.rm = TRUE),
      matched_taxon = distribution_label(matched_taxon),
      resolved_taxon_name = distribution_label(resolved_taxon_name),
      match_distance = if (all(is.na(match_distance))) NA_real_ else min(match_distance, na.rm = TRUE),
      distribution_status = pick_status(distribution_status),
      n_areas = dplyr::n_distinct(stats::na.omit(area)),
      area_codes = collapse_unique(area_code_l3),
      areas = collapse_unique(area),
      regions = collapse_unique(region),
      continents = collapse_unique(continent),
      distribution = collapse_unique(area),
      occurrence_types = collapse_unique(occurrence_type),
      native = if (all(is.na(native))) as.logical(NA) else any(native %in% TRUE, na.rm = TRUE),
      introduced = if (all(is.na(introduced))) as.logical(NA) else any(introduced %in% TRUE, na.rm = TRUE),
      extinct = if (all(is.na(extinct))) as.logical(NA) else any(extinct %in% TRUE, na.rm = TRUE),
      location_doubtful = if (all(is.na(location_doubtful))) as.logical(NA) else any(location_doubtful %in% TRUE, na.rm = TRUE),
      n_plant_name_ids = if (all(is.na(.data$n_plant_name_ids))) 0L else max(.data$n_plant_name_ids, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::arrange(input_index)
}

distribution_label <- function(x) {
  vals <- x[!is.na(x) & nzchar(x)]
  vals <- unique(as.character(vals))

  if (length(vals) == 0) {
    return(NA_character_)
  }

  if (length(vals) == 1) {
    return(vals[[1]])
  }

  paste(sort(vals), collapse = " | ")
}

determine_occurrence_type_ <- function(introduced, extinct, location_doubtful) {
  introduced <- as.numeric(introduced)
  extinct <- as.numeric(extinct)
  location_doubtful <- as.numeric(location_doubtful)

  if (any(introduced + extinct + location_doubtful == 0, na.rm = TRUE)) {
    return("native")
  }

  if (any(introduced == 1, na.rm = TRUE)) {
    return("introduced")
  }

  if (any(location_doubtful == 1, na.rm = TRUE)) {
    return("location_doubtful")
  }

  "extinct"
}
