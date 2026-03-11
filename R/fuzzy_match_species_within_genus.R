#' Fuzzy Match Species within Genus
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Tries to fuzzy match the species epithet within a matched genus against 'WCVP' (using the optional `wcvpdata` checklist by default when available).
#'
#' @param df `tibble` containing the species binomial split into the columns `Orig.Genus` and `Orig.Species`.
#' @param target_df Optional custom target table. If `NULL`, the optional `wcvpdata` checklist is used when available; otherwise pass a backbone explicitly.
#' @param max_dist Maximum edit distance used for fuzzy species matching within genus.
#' @param method String distance method passed to `fozziejoin` (for example `"osa"`).
#'
#' @return
#' Returns a `tibble` with the additional logical column `fuzzy_match_species_within_genus`, indicating whether the specific epithet was successfully fuzzy matched within the matched genus (`TRUE`) or not (`FALSE`).
#' @examplesIf rlang::is_installed("wcvpdata")
#' library(wcvpmatch)
#' df <- data.frame(Orig.Genus = "Opuntia", Orig.Species = "yanganucensiss", Matched.Genus = "Opuntia")
#' wcvp_fuzzy_match_species_within_genus(df)
#' @export
#'
wcvp_fuzzy_match_species_within_genus <- function(df, target_df = NULL, max_dist = 1, method = "osa"){
  df <- check_df_format(df)
  assertthat::assert_that(all(c('Orig.Genus', 'Orig.Species', 'Matched.Genus') %in% colnames(df)))
  target_df <- get_db(target_df = target_df)
  ambiguous_species <- NULL

  ## handle empty input tibble while preserving expected output schema
  if(nrow(df) == 0){
    if(!all(c('fuzzy_match_species_within_genus', 'fuzzy_species_dist') %in% colnames(df))){
      return(tibble::add_column(df, fuzzy_match_species_within_genus = NA, fuzzy_species_dist = NA_real_))
    }
    else{
      return(df)
    }
  }

  ## avoid duplicated distance columns if function is called repeatedly on the same object
  if('fuzzy_species_dist' %in% colnames(df)){
    df <- df %>% dplyr::mutate(fuzzy_species_dist = NULL)
  }

  df_work <- df %>%
    dplyr::mutate(.row_id = dplyr::row_number())

  # Vectorized candidate generation in a single fuzzy join over candidate genera.
  # This avoids group_split() + map_dfr() overhead for many genera.
  db_subset <- target_df %>%
    dplyr::semi_join(df_work %>% dplyr::distinct(Matched.Genus), by = c("Genus" = "Matched.Genus")) %>%
    dplyr::select(Genus, Species) %>%
    dplyr::distinct()

  matched_temp <- df_work %>%
    fozziejoin::fozzie_string_left_join(
      db_subset,
      by = c("Orig.Species" = "Species"),
      distance_col = "fuzzy_species_dist",
      method = method,
      max_distance = max_dist
    ) %>%
    dplyr::filter(Matched.Genus == Genus) %>%
    dplyr::mutate(
      .orig_len = nchar(Orig.Species),
      .cand_len = nchar(Species)
    ) %>%
    dplyr::filter(
      !is.na(fuzzy_species_dist),
      fuzzy_species_dist <= max_dist,
      !is.na(Orig.Species),
      !is.na(Species),
      !(.orig_len <= 7 & .orig_len != .cand_len)
    ) %>%
    dplyr::mutate(Matched.Species = Species) %>%
    dplyr::select(-c(Species, Genus, .orig_len, .cand_len)) %>%
    dplyr::group_by(.row_id) %>%
    dplyr::slice_min(order_by = fuzzy_species_dist, n = 1, with_ties = TRUE) %>%
    dplyr::ungroup()

  ambiguous_keys <- matched_temp %>%
    dplyr::count(.row_id, name = "n") %>%
    dplyr::filter(n > 1)

  if (nrow(ambiguous_keys) > 0) {
    cli::cli_warn(c(
      "!" = "Multiple fuzzy matches for some species within genus (tied distances).",
      "i" = "The first match is selected."
    ))
    ambiguous_species <- matched_temp %>%
      dplyr::semi_join(ambiguous_keys, by = ".row_id") %>%
      dplyr::arrange(.row_id, fuzzy_species_dist, Matched.Species)
  }

  matched <- matched_temp %>%
    dplyr::group_by(.row_id) %>%
    dplyr::slice_head(n = 1) %>%
    dplyr::ungroup()

  unmatched <- df_work %>%
    dplyr::anti_join(
      matched %>% dplyr::select(.row_id),
      by = ".row_id"
    )

  assertthat::assert_that(nrow(df_work) == (nrow(matched) + nrow(unmatched)))

  res <- dplyr::bind_rows(matched, unmatched, .id = 'fuzzy_match_species_within_genus') %>%
    dplyr::mutate(fuzzy_match_species_within_genus = (fuzzy_match_species_within_genus == 1)) %>%
    dplyr::select(-dplyr::any_of(".row_id")) %>%
    dplyr::relocate(c('Orig.Genus', 'Orig.Species'))

  if (!is.null(ambiguous_species) && nrow(ambiguous_species) > 0) {
    attr(res, "ambiguous_species") <- ambiguous_species
  }

  return(res)
}


fuzzy_match_species_within_genus_helper <- function(df, target_df, max_dist = 1, method = "osa"){
  # subset database
  genus <- df %>% dplyr::distinct(Matched.Genus) %>% unlist()
  database_subset <- get_trees_of_genus(genus, target_df)

  # Fuzzy match candidates.
  # For short epithets (<= 7 chars), allow fuzzy only when candidate length is equal.
  # This keeps 1-letter substitutions (e.g. blancie -> blancii) and blocks indels.
  matched_candidates <- df %>%
    fozziejoin::fozzie_string_left_join(database_subset,
                                    by = c('Orig.Species' = 'Species'),
                                    distance_col = 'fuzzy_species_dist',
                                    method = method,
                                    max_distance = max_dist) %>%
    dplyr::mutate(
      .orig_len = nchar(Orig.Species),
      .cand_len = nchar(Species)
    ) %>%
    dplyr::filter(
      !is.na(fuzzy_species_dist),
      fuzzy_species_dist <= max_dist,
      !(.orig_len <= 7 & .orig_len != .cand_len)
    ) %>%
    dplyr::mutate(Matched.Species = Species) %>%
    dplyr::select(-c('Species', 'Genus')) %>%
    dplyr::group_by(Orig.Genus, Orig.Species) %>%
    dplyr::slice_min(order_by = fuzzy_species_dist, n = 1, with_ties = TRUE) %>%
    dplyr::ungroup() %>%
    dplyr::select(-.orig_len, -.cand_len)

  matched <- matched_candidates

  unmatched <- df %>%
    dplyr::anti_join(
      matched %>% dplyr::select(Orig.Genus, Orig.Species),
      by = c("Orig.Genus", "Orig.Species")
    )

  assertthat::assert_that(nrow(df) == (nrow(matched) + nrow(unmatched)))

  # combine matched and unmatched and add Boolean indicator: TRUE = matched, FALSE = unmatched
  combined <-  dplyr::bind_rows(matched, unmatched, .id = 'fuzzy_match_species_within_genus') %>%
    dplyr::mutate(fuzzy_match_species_within_genus = (fuzzy_match_species_within_genus == 1)) %>% ## convert to Boolean
    dplyr::relocate(c('Orig.Genus', 'Orig.Species')) ## Genus & Species column at the beginning of tibble
  return(combined)
}

