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

  db_subset <- .infra_candidate_keys(target_df)

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

  db_subset <- .infra_candidate_keys(target_df)

  df_std <- df %>%
    dplyr::mutate(
      .Infra.Rank.upper = .rank_to_upper(Infra.Rank),
      .rank_key = dplyr::coalesce(.Infra.Rank.upper, "__NA__")
    )

  pairs <- .nearest_pairs(
    df_std$Orig.Infraspecies, db_subset$infraspecies,
    .make_taxon_key(df_std$Matched.Genus, df_std$Matched.Species, df_std$.rank_key, NA_character_),
    .make_taxon_key(db_subset$genus, db_subset$species, db_subset$.db_rank_key, NA_character_),
    method, max_dist
  )
  matched_temp <- df_std[pairs$query, , drop = FALSE]
  matched_temp$infraspecies <- db_subset$infraspecies[pairs$candidate]
  matched_temp$fuzzy_infraspecies_dist <- pairs$distance
  matched_temp <- matched_temp %>%
    dplyr::mutate(Matched.Infraspecies = infraspecies) %>%
    dplyr::select(-dplyr::any_of(c("genus", "species", "infraspecies", "db_rank", ".db_rank_key"))) %>%
    dplyr::group_by(Orig.Genus, Orig.Species, Infra.Rank, Orig.Infraspecies) %>%
    dplyr::filter(fuzzy_infraspecies_dist == min(fuzzy_infraspecies_dist)) %>%
    dplyr::ungroup()

  ambiguous <- matched_temp %>%
    dplyr::count(Orig.Genus, Orig.Species, Infra.Rank, Orig.Infraspecies, name = "n") %>%
    dplyr::filter(n > 1)

  if (nrow(ambiguous) > 0) {
    cli::cli_warn(c(
      "!" = "Found infraspecific names with multiple fuzzy matches (tied distances).",
      "i" = "Selecting the first match."
    ))
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
