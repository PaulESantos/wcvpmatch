#' Direct Match Species within Genus
#'
#' @description
#' Tries to directly match the specific epithet within an already matched genus in WCVP.
#'
#' @param df `tibble` containing the species binomial split into the columns `Orig.Genus` and `Orig.Species`.
#' @param target_df Optional custom target table; if `NULL`, uses `rWCVPdata::wcvp_names`.
#'
#' @return
#' Returns a `tibble` with the additional logical column `direct_match_species_within_genus`, indicating whether the specific epithet was successfully matched within the matched genus (`r TRUE`) or not (`r FALSE`).
#' @export
#'
direct_match_species_within_genus <- function(df, target_df = NULL){

  assertthat::assert_that(all(c('Orig.Genus', 'Orig.Species', 'Matched.Genus') %in% colnames(df)))
  target_df <- get_db(target_df = target_df)

  ## handle empty input tibble while preserving expected output schema
  if(nrow(df) == 0){
    if(!all(c('direct_match_species_within_genus') %in% colnames(df))){
      return(tibble::add_column(df, direct_match_species_within_genus = NA))
    }
    else{
      return(df)
    }
  }

  res <- df %>%
    dplyr::group_by(Matched.Genus) %>%
    dplyr::group_split() %>%
    purrr::map_dfr(direct_match_species_within_genus_helper, target_df)

  return(res)
}

direct_match_species_within_genus_helper <- function(df, target_df){
  # subset database
  genus <- df %>% dplyr::distinct(Matched.Genus) %>% unlist()
  database_subset <- get_trees_of_genus(genus, target_df)

  # match specific epithet within genus
  matched <- df %>%
    dplyr::semi_join(database_subset, by = c('Orig.Species' = 'Species')) %>%
    dplyr::mutate(Matched.Species = Orig.Species)
  unmatched <- df %>%
    dplyr::anti_join(database_subset, by = c('Orig.Species' = 'Species'))

    assertthat::assert_that(nrow(df) == (nrow(matched) + nrow(unmatched)))

  # combine matched and unmatched and add Boolean indicator: TRUE = matched, FALSE = unmatched
  combined <-  dplyr::bind_rows(matched, unmatched, .id = 'direct_match_species_within_genus') %>%
    dplyr::mutate(direct_match_species_within_genus = (direct_match_species_within_genus == 1)) %>% ## convert to Boolean
    dplyr::relocate(c('Orig.Genus', 'Orig.Species')) ## Genus & Species column at the beginning of tibble
  return(combined)
}
