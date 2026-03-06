#' Fuzzy Match Genus Name
#' @description
#' Tries to fuzzy match the genus name to the WCVP table (`rWCVPdata::wcvp_names` by default).
#'
#' @param df `tibble` containing the species binomial split into the columns `Orig.Genus` and `Orig.Species`.
#' @param target_df Optional custom target table; if `NULL`, uses `rWCVPdata::wcvp_names`.
#'
#' @return
#' Returns a `tibble` with the additional logical column `fuzzy_match_genus`, indicating whether the genus was successfully matched (`r TRUE`) or not (`r FALSE`).
#' Further, the additional column `fuzzy_genus_dist` returns the distance for every match.
#' @export
#'
#' @examples
#' iucn %>%
#'     dplyr::mutate(Orig.Genus = stringr::str_replace(Orig.Genus, '.{1}$', '')) %>%
#'     fuzzy_match_genus()
fuzzy_match_genus <- function(df, target_df = NULL, max_dist = 1, method = "osa"){
  df <- check_df_format(df)
  assertthat::assert_that(all(c('Orig.Genus', 'Orig.Species') %in% colnames(df)))
  target_df <- get_db(target_df = target_df)

  ## handle empty input tibble while preserving expected output schema
  if(nrow(df) == 0){
    if(!all(c('fuzzy_match_genus', 'fuzzy_genus_dist') %in% colnames(df))){
      return(tibble::add_column(df, fuzzy_match_genus = NA, fuzzy_genus_dist = NA_real_))
    }
    else{
      return(df)
    }
  }

  ## avoid duplicated distance columns if function is called repeatedly on the same object
  if('fuzzy_genus_dist' %in% colnames(df)){
    df <- df %>% dplyr::mutate(fuzzy_genus_dist = NULL)
  } ## TODO: can potentially be removed again????


  Tree.Genera <- target_df |>
    dplyr::distinct(genus)

  # fuzzy match
  matched_temp <- df %>%
    fozziejoin::fozzie_string_left_join(
      Tree.Genera,
      by = c('Orig.Genus' = 'genus'),
      max_distance = max_dist,
      distance_col = 'fuzzy_genus_dist',
      method = method
    ) %>%
    # save matched Genus name to Matched.Genus
    dplyr::mutate(Matched.Genus = genus) %>%
    dplyr::select(-c('genus')) %>%
    dplyr::group_by(Orig.Genus, Orig.Species) %>%
    dplyr::filter(fuzzy_genus_dist == min(fuzzy_genus_dist))

## If there are multiple matches for the same genus: raise warning and advise for manual checking
  if(matched_temp %>% dplyr::filter(dplyr::n() > 1) %>% nrow() > 0){
    warning(
      "Multiple fuzzy matches for some genera (tied distances). The first match is selected.",
      call. = FALSE
    )
  }

 ## continue selecting first genus if more than one match
  matched <- matched_temp %>%
    dplyr::slice_head(n = 1) %>%
    dplyr::ungroup()

  unmatched <- df %>%
    fozziejoin::fozzie_string_anti_join(Tree.Genera,
                                        by = c('Orig.Genus' = 'genus'),
                                        max_distance = max_dist,
                                        method = method)

  assertthat::assert_that(nrow(df) == (nrow(matched) + nrow(unmatched)))

  res <-  dplyr::bind_rows(matched, unmatched, .id = 'fuzzy_match_genus') %>%
    dplyr::mutate(fuzzy_match_genus = (fuzzy_match_genus == 1)) %>% ## convert to Boolean
    dplyr::arrange(Orig.Genus, Orig.Species) %>%
    dplyr::relocate(c('Orig.Genus', 'Orig.Species')) ## Genus & Species column at the beginning of tibble

  return(res)
}
