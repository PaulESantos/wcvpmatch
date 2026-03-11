#' Fuzzy Match Genus Name
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Tries to fuzzy match the genus name to the 'WCVP' table (using the optional `wcvpdata` checklist by default when available).
#'
#' @param df `tibble` containing the species binomial split into the columns `Orig.Genus` and `Orig.Species`.
#' @param target_df Optional custom target table. If `NULL`, the optional `wcvpdata` checklist is used when available; otherwise pass a backbone explicitly.
#' @param max_dist Maximum edit distance used for fuzzy genus matching.
#' @param method String distance method passed to `fozziejoin` (for example `"osa"`).
#'
#' @return
#' Returns a `tibble` with the additional logical column `fuzzy_match_genus`, indicating whether the genus was successfully matched (`TRUE`) or not (`FALSE`).
#' Further, the additional column `fuzzy_genus_dist` returns the distance for every match.
#' @examplesIf rlang::is_installed("wcvpdata")
#' library(wcvpmatch)
#' df <- data.frame(Orig.Genus = "Opuntiaa", Orig.Species = "yanganucensis")
#' wcvp_fuzzy_match_genus(df)
#' @export
#'

wcvp_fuzzy_match_genus <- function(df, target_df = NULL, max_dist = 1, method = "osa"){
  df <- check_df_format(df)
  assertthat::assert_that(all(c('Orig.Genus', 'Orig.Species') %in% colnames(df)))
  target_df <- get_db(target_df = target_df)
  ambiguous_genus <- NULL

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


  df_work <- df %>%
    dplyr::mutate(.row_id = dplyr::row_number())

  Tree.Genera <- target_df |>
    dplyr::distinct(genus)

  # fuzzy match
  matched_temp <- df_work %>%
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
    dplyr::group_by(.row_id) %>%
    dplyr::filter(fuzzy_genus_dist == min(fuzzy_genus_dist))

  ambiguous_keys <- matched_temp %>%
    dplyr::count(.row_id, name = "n") %>%
    dplyr::filter(n > 1)

## If there are multiple matches for the same genus: raise warning and keep ambiguous candidates in an attribute
  if(nrow(ambiguous_keys) > 0){
    cli::cli_warn(c(
      "!" = "Multiple fuzzy matches for some genera (tied distances).",
      "i" = "The first match is selected."
    ))
    ambiguous_genus <- matched_temp %>%
      dplyr::semi_join(ambiguous_keys, by = ".row_id") %>%
      dplyr::arrange(.row_id, fuzzy_genus_dist, Matched.Genus)
  }

 ## continue selecting first genus if more than one match
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

  res <-  dplyr::bind_rows(matched, unmatched, .id = 'fuzzy_match_genus') %>%
    dplyr::mutate(fuzzy_match_genus = (fuzzy_match_genus == 1)) %>% ## convert to Boolean
    dplyr::select(-dplyr::any_of(".row_id")) %>%
    dplyr::arrange(Orig.Genus, Orig.Species) %>%
    dplyr::relocate(c('Orig.Genus', 'Orig.Species')) ## Genus & Species column at the beginning of tibble

  if (!is.null(ambiguous_genus) && nrow(ambiguous_genus) > 0) {
    attr(res, "ambiguous_genus") <- ambiguous_genus
  }

  return(res)
}

