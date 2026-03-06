#' Fuzzy Match Species within Genus
#' @description
#' Tries to fuzzy match the species epithet within a matched genus against WCVP (`rWCVPdata::wcvp_names` by default).
#'
#' @param df `tibble` containing the species binomial split into the columns `Orig.Genus` and `Orig.Species`.
#' @param target_df Optional custom target table; if `NULL`, uses `rWCVPdata::wcvp_names`.
#'
#' @return
#' Returns a `tibble` with the additional logical column `fuzzy_match_species_within_genus`, indicating whether the specific epithet was successfully fuzzy matched within the matched genus (`r TRUE`) or not (`r FALSE`).
#' @export
#'
#' @examples
#' iucn %>%
#'     dplyr::mutate(Orig.Genus = stringr::str_replace(Orig.Genus, '.{1}$', '')) %>%
#'     dplyr::mutate(Matched.Genus = Orig.Genus) %>%
#'     fuzzy_match_species_within_genus()
fuzzy_match_species_within_genus <- function(df, target_df = NULL, max_dist = 1, method = "osa"){
  df <- check_df_format(df)
  assertthat::assert_that(all(c('Orig.Genus', 'Orig.Species', 'Matched.Genus') %in% colnames(df)))
  target_df <- get_db(target_df = target_df)

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

  # Vectorized candidate generation in a single fuzzy join over candidate genera.
  # This avoids group_split() + map_dfr() overhead for many genera.
  db_subset <- target_df %>%
    dplyr::semi_join(df %>% dplyr::distinct(Matched.Genus), by = c("Genus" = "Matched.Genus")) %>%
    dplyr::select(Genus, Species) %>%
    dplyr::distinct()

  matched_candidates <- df %>%
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
    dplyr::group_by(Orig.Genus, Orig.Species) %>%
    dplyr::slice_min(order_by = fuzzy_species_dist, n = 1, with_ties = FALSE) %>%
    dplyr::ungroup()

  matched <- matched_candidates

  unmatched <- df %>%
    dplyr::anti_join(
      matched %>% dplyr::select(Orig.Genus, Orig.Species),
      by = c("Orig.Genus", "Orig.Species")
    )

  assertthat::assert_that(nrow(df) == (nrow(matched) + nrow(unmatched)))

  res <- dplyr::bind_rows(matched, unmatched, .id = 'fuzzy_match_species_within_genus') %>%
    dplyr::mutate(fuzzy_match_species_within_genus = (fuzzy_match_species_within_genus == 1)) %>%
    dplyr::relocate(c('Orig.Genus', 'Orig.Species'))

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
    dplyr::slice_min(order_by = fuzzy_species_dist, n = 1, with_ties = FALSE) %>%
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
