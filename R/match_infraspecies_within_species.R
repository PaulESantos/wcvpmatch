#' Direct Match Infraspecific Rank within Species
#' @keywords internal
direct_match_infra_rank_within_species <- function(df,
                                                   target_df = NULL) {
  required_cols <- c(
    "Orig.Genus", "Orig.Species", "Infra.Rank", "Orig.Infraspecies",
    "Matched.Genus", "Matched.Species"
  )
  assertthat::assert_that(
    all(required_cols %in% colnames(df)),
    msg = "Missing required columns for direct_match_infra_rank_within_species()."
  )
  target_df <- get_db(target_df = target_df)
  assertthat::assert_that("infraspecific_rank" %in% names(target_df), msg = "Missing `infraspecific_rank` in target_df.")

  if (nrow(df) == 0) {
    if (!("direct_match_infra_rank" %in% colnames(df))) {
      return(tibble::add_column(df, direct_match_infra_rank = logical(0)))
    }
    return(df)
  }

  db_subset <- target_df %>%
    dplyr::select(genus, species, infraspecies, infraspecific_rank) %>%
    dplyr::rename(db_rank = infraspecific_rank) %>%
    dplyr::mutate(db_rank = .rank_to_upper(db_rank)) %>%
    tidyr::drop_na(genus, species, infraspecies) %>%
    dplyr::mutate(.db_rank_key = dplyr::coalesce(db_rank, "__NA__")) %>%
    dplyr::distinct()

  df_std <- df %>%
    dplyr::mutate(
      .Infra.Rank.upper = .rank_to_upper(Infra.Rank),
      .rank_key = dplyr::coalesce(.Infra.Rank.upper, "__NA__")
    )

  matched <- df_std %>%
    dplyr::semi_join(
      db_subset,
      by = c("Matched.Genus" = "genus", "Matched.Species" = "species", ".rank_key" = ".db_rank_key")
    ) %>%
    dplyr::mutate(Matched.Infra.Rank = Infra.Rank)

  unmatched <- df_std %>%
    dplyr::anti_join(
      db_subset,
      by = c("Matched.Genus" = "genus", "Matched.Species" = "species", ".rank_key" = ".db_rank_key")
    )

  assertthat::assert_that(nrow(df_std) == (nrow(matched) + nrow(unmatched)))

  dplyr::bind_rows(matched, unmatched, .id = "direct_match_infra_rank") %>%
    dplyr::mutate(direct_match_infra_rank = direct_match_infra_rank == "1") %>%
    dplyr::select(-dplyr::any_of(c(".Infra.Rank.upper", ".rank_key")))
}

#' Fuzzy Match Infraspecific Epithet within Species
#' @keywords internal
fuzzy_match_infraspecies_within_species <- function(df,
                                                    target_df = NULL,
                                                    max_dist = 1,
                                                    method = "osa") {
  required_cols <- c(
    "Orig.Genus", "Orig.Species", "Infra.Rank", "Orig.Infraspecies",
    "Matched.Genus", "Matched.Species"
  )
  assertthat::assert_that(
    all(required_cols %in% colnames(df)),
    msg = paste("Missing required columns:", paste(setdiff(required_cols, colnames(df)), collapse = ", "))
  )

  target_df <- get_db(target_df = target_df)
  assertthat::assert_that("infraspecific_rank" %in% names(target_df), msg = "Missing `infraspecific_rank` in target_df.")

  if (nrow(df) == 0) {
    if (!all(c("fuzzy_match_infraspecies", "fuzzy_infraspecies_dist") %in% colnames(df))) {
      return(tibble::add_column(df, fuzzy_match_infraspecies = logical(0), fuzzy_infraspecies_dist = numeric(0)))
    }
    return(df)
  }

  if ("fuzzy_infraspecies_dist" %in% colnames(df)) {
    df <- dplyr::mutate(df, fuzzy_infraspecies_dist = NULL)
  }

  db_subset <- target_df %>%
    dplyr::select(genus, species, infraspecies, infraspecific_rank) %>%
    dplyr::rename(db_rank = infraspecific_rank) %>%
    dplyr::mutate(db_rank = .rank_to_upper(db_rank)) %>%
    tidyr::drop_na(genus, species, infraspecies) %>%
    dplyr::mutate(.db_rank_key = dplyr::coalesce(db_rank, "__NA__")) %>%
    dplyr::distinct()

  df_std <- df %>%
    dplyr::mutate(
      .Infra.Rank.upper = .rank_to_upper(Infra.Rank),
      .rank_key = dplyr::coalesce(.Infra.Rank.upper, "__NA__")
    )

  matched_temp <- df_std %>%
    fozziejoin::fozzie_string_left_join(
      db_subset,
      by = c("Orig.Infraspecies" = "infraspecies"),
      distance_col = "fuzzy_infraspecies_dist",
      max_distance = max_dist,
      method = method
    ) %>%
    dplyr::filter(
      Matched.Genus == genus,
      Matched.Species == species,
      .rank_key == .db_rank_key
    ) %>%
    dplyr::mutate(Matched.Infraspecies = infraspecies) %>%
    dplyr::select(-dplyr::any_of(c("genus", "species", "infraspecies", "db_rank", ".db_rank_key"))) %>%
    dplyr::group_by(Orig.Genus, Orig.Species, Infra.Rank, Orig.Infraspecies) %>%
    dplyr::filter(fuzzy_infraspecies_dist == min(fuzzy_infraspecies_dist)) %>%
    dplyr::ungroup()

  ambiguous <- matched_temp %>%
    dplyr::count(Orig.Genus, Orig.Species, Infra.Rank, Orig.Infraspecies, name = "n") %>%
    dplyr::filter(n > 1)

  if (nrow(ambiguous) > 0) {
    warning(
      "Found infraspecific names with multiple fuzzy matches (tied distances). Selecting the first match.",
      call. = FALSE
    )
  }

  matched <- matched_temp %>%
    dplyr::group_by(Orig.Genus, Orig.Species, Infra.Rank, Orig.Infraspecies) %>%
    dplyr::slice_head(n = 1) %>%
    dplyr::ungroup()

  unmatched <- df_std %>%
    dplyr::anti_join(
      matched %>% dplyr::select(Orig.Genus, Orig.Species, Infra.Rank, Orig.Infraspecies),
      by = c("Orig.Genus", "Orig.Species", "Infra.Rank", "Orig.Infraspecies")
    )

  out <- dplyr::bind_rows(matched, unmatched, .id = "fuzzy_match_infraspecies") %>%
    dplyr::mutate(fuzzy_match_infraspecies = fuzzy_match_infraspecies == "1") %>%
    dplyr::select(-dplyr::any_of(c(".Infra.Rank.upper", ".rank_key")))

  if (nrow(ambiguous) > 0) {
    attr(out, "ambiguous_infraspecies") <- matched_temp %>%
      dplyr::semi_join(ambiguous, by = c("Orig.Genus", "Orig.Species", "Infra.Rank", "Orig.Infraspecies")) %>%
      dplyr::arrange(Orig.Genus, Orig.Species, Orig.Infraspecies, fuzzy_infraspecies_dist, Matched.Infraspecies)
  }

  out
}
