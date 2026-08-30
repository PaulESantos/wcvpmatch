#' Direct Match Species & Genus Binomial or Trinomial names
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Tries to directly match Genus + Species | Genus + Species + Rank + Infraspecies to `WCVP data`.
#'
#' @param df `tibble` containing the species binomial split into the columns `Orig.Genus` and `Orig.Species`.
#' @param target_df Optional custom target table. If `NULL`, the optional `wcvpdata` checklist is used when available; otherwise pass a backbone explicitly.
#'
#' @return
#' Returns a `tibble` with the additional logical column `direct_match`, indicating whether the binomial was successfully matched (`TRUE`) or not (`FALSE`).
#' Returns original columns plus `Matched.Genus`, `Matched.Species`, `Matched.Infra.Rank`, and `Matched.Infraspecies`.
#' @examples
#' \donttest{
#' df_parsed <- classify_spnames("Opuntia yanganucensis")
#' target <- data.frame(genus = "Opuntia", species = "yanganucensis", plant_name_id = 1)
#' wcvpmatch:::wcvp_direct_match(df_parsed, target_df = target)
#' }
#' @keywords internal
wcvp_direct_match <- function(df, target_df = NULL) {
  df <- check_df_format(df)
  target_df <- get_db(target_df = target_df)

  assertthat::assert_that(
    all(c("sorter","Orig.Genus","Orig.Species","Orig.Infraspecies","Infra.Rank","Rank",
          "implied_infra","is_sp","is_spp") %in% names(df)),
    msg = "Input must be normalized with check_df_format() and come from classify_spnames()."
  )

  # Empty input
  if (nrow(df) == 0) {
    if (!"direct_match" %in% names(df)) df$direct_match <- logical(0)
    if (!"Matched.Genus" %in% names(df)) df$Matched.Genus <- character(0)
    if (!"Matched.Species" %in% names(df)) df$Matched.Species <- character(0)
    if (!"Matched.Infraspecies" %in% names(df)) df$Matched.Infraspecies <- character(0)
    if (!"Matched.Infra.Rank" %in% names(df)) df$Matched.Infra.Rank <- character(0)
    return(df)
  }

  needed <- c("genus", "species", "infraspecific_rank", "infraspecies")
  assertthat::assert_that(
    all(needed %in% names(target_df)),
    msg = "Backbone (target_df) must contain columns: genus, species, infraspecific_rank, infraspecies."
  )

  infra_rank_upper <- .rank_to_upper(df$Infra.Rank)
  target_rank_upper <- .rank_to_upper(target_df$infraspecific_rank)
  matched <- rep(FALSE, nrow(df))

  rank1 <- df$Rank == 1 | df$is_sp | df$is_spp
  rank1[is.na(rank1)] <- FALSE
  if (any(rank1)) {
    target_genera <- unique(target_df$genus[!is.na(target_df$genus)])
    matched[rank1] <- df$Orig.Genus[rank1] %in% target_genera
  }

  rank2 <- df$Rank == 2 & !is.na(df$Orig.Species)
  rank2[is.na(rank2)] <- FALSE
  if (any(rank2)) {
    target_ok <- !is.na(target_df$genus) & !is.na(target_df$species)
    target_key <- paste(target_df$genus[target_ok], target_df$species[target_ok], sep = "\r")
    input_key <- paste(df$Orig.Genus[rank2], df$Orig.Species[rank2], sep = "\r")
    matched[rank2] <- input_key %in% target_key
  }

  rank3 <- df$Rank == 3 & !is.na(df$Orig.Species) & !is.na(df$Orig.Infraspecies)
  rank3[is.na(rank3)] <- FALSE
  ranked <- rank3 & !df$implied_infra
  implied <- rank3 & df$implied_infra

  if (any(ranked)) {
    target_ok <- !is.na(target_df$genus) & !is.na(target_df$species) &
      !is.na(target_rank_upper) & !is.na(target_df$infraspecies)
    target_key <- .make_taxon_key(
      target_df$genus[target_ok], target_df$species[target_ok],
      target_rank_upper[target_ok], target_df$infraspecies[target_ok]
    )
    input_key <- .make_taxon_key(
      df$Orig.Genus[ranked], df$Orig.Species[ranked],
      infra_rank_upper[ranked], df$Orig.Infraspecies[ranked]
    )
    matched[ranked] <- input_key %in% target_key
  }

  if (any(implied)) {
    target_ok <- !is.na(target_df$genus) & !is.na(target_df$species) &
      is.na(target_rank_upper) & !is.na(target_df$infraspecies)
    target_key <- .make_taxon_key(
      target_df$genus[target_ok], target_df$species[target_ok],
      NA_character_, target_df$infraspecies[target_ok]
    )
    input_key <- .make_taxon_key(
      df$Orig.Genus[implied], df$Orig.Species[implied],
      NA_character_, df$Orig.Infraspecies[implied]
    )
    matched[implied] <- input_key %in% target_key
  }

  dplyr::mutate(
    df,
    direct_match = matched,
    Matched.Genus = dplyr::if_else(matched, Orig.Genus, NA_character_),
    Matched.Species = dplyr::if_else(matched, Orig.Species, NA_character_),
    Matched.Infraspecies = dplyr::if_else(matched, Orig.Infraspecies, NA_character_),
    Matched.Infra.Rank = dplyr::if_else(matched, Infra.Rank, NA_character_)
  )
}
