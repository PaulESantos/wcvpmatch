#' Match Genus name
#'
#' @description
#' #' Tries to match the genus name to `WCVP data`.
#'
#' @param df `tibble` containing the species binomial split into the columns `Orig.Genus` and `Orig.Species`.
#' @param target_df Optional custom target table; if `NULL`, uses `rWCVPdata::wcvp_names`.
#'
#' @return
#' Returns a `tibble` with the additional logical column `genus_match`, indicating whether the genus was successfully matched (`r TRUE`) or not (`r FALSE`)
#' @export

genus_match <- function(df, target_df = NULL){
  df <- check_df_format(df)
  assertthat::assert_that(all(c('Orig.Genus', 'Orig.Species') %in% colnames(df)))
  target_df <- get_db(target_df = target_df)

  target_genera <- target_df %>%
    dplyr::distinct(genus)

  ## handle empty input tibble while preserving expected output schema
  if(nrow(df) == 0){
    if(!all(c('genus_match') %in% colnames(df))){
      return(tibble::add_column(df, genus_match = NA))
    }
    else{
      return(df)
    }
  }

  matched <- df %>%
    dplyr::semi_join(target_genera,
                     by = c('Orig.Genus' = 'genus')) %>%
    dplyr::mutate(Matched.Genus = Orig.Genus)

  unmatched <- df %>%
    dplyr::anti_join(target_genera,
                     by = c('Orig.Genus' = 'genus'))

  assertthat::assert_that(nrow(df) == (nrow(matched) + nrow(unmatched)))

  # combine matched and unmatched and add Boolean indicator: TRUE = matched, FALSE = unmatched
  combined <-  dplyr::bind_rows(matched, unmatched, .id = 'genus_match') %>%
    dplyr::mutate(genus_match = (genus_match == 1)) %>% ## convert to Boolean
    dplyr::relocate(c('Orig.Genus', 'Orig.Species')) ## Genus & Species column at the beginning of tibble

  return(combined)
}
