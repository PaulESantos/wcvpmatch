#' Direct Match Species within Genus
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Tries to directly match the specific epithet within an already matched genus in 'WCVP'.
#'
#' @param df `tibble` containing the species binomial split into the columns `Orig.Genus` and `Orig.Species`.
#' @param target_df Optional custom target table. If `NULL`, the optional `wcvpdata` checklist is used when available; otherwise pass a backbone explicitly.
#'
#' @return
#' Returns a `tibble` with the additional logical column `direct_match_species_within_genus`, indicating whether the specific epithet was successfully matched within the matched genus (`TRUE`) or not (`FALSE`).
#' @examplesIf rlang::is_installed("wcvpdata")
#' library(wcvpmatch)
#' df <- data.frame(Orig.Genus = "Opuntia", Orig.Species = "yanganucensis", Matched.Genus = "Opuntia")
#' wcvp_direct_match_species_within_genus(df)
#' @export
#'
wcvp_direct_match_species_within_genus <- function(df, target_df = NULL){
  df <- check_df_format(df)

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

  # Match must be constrained by both Matched.Genus and Orig.Species.
  # Using species-only joins can produce false positives when the same epithet
  # exists in other genera.
  db_subset <- target_df %>%
    dplyr::select(genus, species) %>%
    dplyr::distinct()

  matched <- df %>%
    dplyr::semi_join(
      db_subset,
      by = c("Matched.Genus" = "genus", "Orig.Species" = "species")
    ) %>%
    dplyr::mutate(Matched.Species = Orig.Species)

  unmatched <- df %>%
    dplyr::anti_join(
      db_subset,
      by = c("Matched.Genus" = "genus", "Orig.Species" = "species")
    )

  assertthat::assert_that(nrow(df) == (nrow(matched) + nrow(unmatched)))

  # combine matched and unmatched and add Boolean indicator: TRUE = matched, FALSE = unmatched
  combined <- dplyr::bind_rows(matched, unmatched, .id = 'direct_match_species_within_genus') %>%
    dplyr::mutate(direct_match_species_within_genus = (direct_match_species_within_genus == 1)) %>% ## convert to Boolean
    dplyr::relocate(c('Orig.Genus', 'Orig.Species')) ## Genus & Species column at the beginning of tibble

  return(combined)
}

