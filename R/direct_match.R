#' Direct Match Species & Genus Binomial or Trinomial names
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Tries to directly match Genus + Species | Genus + Species + Rank + Infraspecies to `WCVP data`.
#'
#' @param df `tibble` containing the species binomial split into the columns `Orig.Genus` and `Orig.Species`.
#' @param target_df Optional custom target table; if `NULL`, uses `wcvpdata::wcvp_checklist_names`.
#'
#' @return
#' Returns a `tibble` with the additional logical column `direct_match`, indicating whether the binomial was successfully matched (`TRUE`) or not (`FALSE`).
#' Returns original columns plus `Matched.Genus`, `Matched.Species`, `Matched.Infra.Rank`, and `Matched.Infraspecies`.
#' @examplesIf rlang::is_installed("wcvpdata")
#' library(wcvpmatch)
#' # Simple binomial match
#' df_parsed <- classify_spnames("Opuntia yanganucensis")
#' wcvp_direct_match(df_parsed)
#' @export
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

  df <- df %>%
    dplyr::mutate(.Infra.Rank.upper = .rank_to_upper(Infra.Rank))

  target_keys <- target_df |>
    dplyr::select(genus, species, infraspecific_rank, infraspecies) |>
    dplyr::mutate(infraspecific_rank = .rank_to_upper(infraspecific_rank)) |>
    dplyr::distinct()

  # Helper: given a data frame with a logical `.matched`, add standard outputs
  finalize_block <- function(x) {
    x |>
      dplyr::mutate(
        direct_match = .matched,
        Matched.Genus = dplyr::if_else(.matched, Orig.Genus, NA_character_),
        Matched.Species = dplyr::if_else(.matched, Orig.Species, NA_character_),
        Matched.Infraspecies = dplyr::if_else(.matched, Orig.Infraspecies, NA_character_),
        Matched.Infra.Rank = dplyr::if_else(.matched, Infra.Rank, NA_character_)
      ) |>
      dplyr::select(-dplyr::any_of(c(".matched", ".Infra.Rank.upper")))
  }

  # -----------------------
  # Rank 1: genus-only (includes sp./spp.)
  df_r1 <- df |>
    dplyr::filter(Rank == 1 | is_sp | is_spp)

  key_r1 <- target_keys |>
    dplyr::filter(!is.na(genus)) |>
    dplyr::distinct(genus)

  df_r1_out <- df_r1 |>
    dplyr::mutate(.key = TRUE) |>
    dplyr::left_join(key_r1 |> dplyr::mutate(.matched = TRUE),
                     by = c("Orig.Genus" = "genus")) |>
    dplyr::mutate(.matched = dplyr::coalesce(.matched, FALSE)) |>
    dplyr::select(-.key) |>
    finalize_block()

  # -----------------------
  # Rank 2: genus + species
  df_r2 <- df |>
    dplyr::filter(Rank == 2 & !is.na(Orig.Species))

  key_r2 <- target_keys |>
    dplyr::filter(!is.na(genus), !is.na(species)) |>
    dplyr::distinct(genus, species) |>
    dplyr::mutate(.matched = TRUE)

  df_r2_out <- df_r2 |>
    dplyr::left_join(key_r2,
                     by = c("Orig.Genus" = "genus", "Orig.Species" = "species")) |>
    dplyr::mutate(.matched = dplyr::coalesce(.matched, FALSE)) |>
    finalize_block()

  # -----------------------
  # Rank 3: infra present
  df_r3 <- df |>
    dplyr::filter(Rank == 3 & !is.na(Orig.Species) & !is.na(Orig.Infraspecies))

  # A) ranked infra (implied_infra == FALSE): rank must match too
  df_r3_ranked <- df_r3 |>
    dplyr::filter(!implied_infra)

  key_r3_ranked <- target_keys |>
    dplyr::filter(!is.na(genus), !is.na(species), !is.na(infraspecific_rank), !is.na(infraspecies)) |>
    dplyr::distinct(genus, species, infraspecific_rank, infraspecies) |>
    dplyr::mutate(.matched = TRUE)

  df_r3_ranked_out <- df_r3_ranked |>
    dplyr::left_join(
      key_r3_ranked,
      by = c(
        "Orig.Genus" = "genus",
        "Orig.Species" = "species",
        ".Infra.Rank.upper" = "infraspecific_rank",
        "Orig.Infraspecies" = "infraspecies"
      )
    ) |>
    dplyr::mutate(.matched = dplyr::coalesce(.matched, FALSE)) |>
    finalize_block()

  # B) implied infra (implied_infra == TRUE): ignore rank, expect backbone rank NA
  df_r3_implied <- df_r3 |>
    dplyr::filter(implied_infra)

  key_r3_implied <- target_keys |>
    dplyr::filter(!is.na(genus), !is.na(species), is.na(infraspecific_rank), !is.na(infraspecies)) |>
    dplyr::distinct(genus, species, infraspecies) |>
    dplyr::mutate(.matched = TRUE)

  df_r3_implied_out <- df_r3_implied |>
    dplyr::left_join(
      key_r3_implied,
      by = c(
        "Orig.Genus" = "genus",
        "Orig.Species" = "species",
        "Orig.Infraspecies" = "infraspecies"
      )
    ) |>
    dplyr::mutate(.matched = dplyr::coalesce(.matched, FALSE)) |>
    finalize_block()

  # -----------------------
  # Other rows: default no match
  used_sorter <- dplyr::bind_rows(
    df_r1 |> dplyr::select(sorter),
    df_r2 |> dplyr::select(sorter),
    df_r3 |> dplyr::select(sorter)
  ) |>
    dplyr::distinct()

  df_other <- df |>
    dplyr::anti_join(used_sorter, by = "sorter") |>
    dplyr::mutate(.matched = FALSE) |>
    finalize_block()

  # Combine and restore original order
  dplyr::bind_rows(df_r1_out, df_r2_out, df_r3_ranked_out, df_r3_implied_out, df_other) |>
    dplyr::arrange(sorter)
}

