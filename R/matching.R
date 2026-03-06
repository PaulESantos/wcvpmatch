#' Match Scientific Names Against WCVP
#' @description
#' Runs a matching pipeline with exact and partial matching for binomial and
#' trinomial names, including infraspecific rank validation.
#'
#' @param df Input tibble/data.frame with either `Genus`/`Species` or
#'   `Orig.Genus`/`Orig.Species`. For trinomials, include `Infra.Rank` and
#'   `Infraspecies` (or `Orig.Infra.Rank`/`Orig.Infraspecies`).
#' @param target_df Optional custom target table. If `NULL`, data are read from
#'   `rWCVPdata::wcvp_names`.
#' @param prefilter_genus Logical. If `TRUE`, prefilter `target_df` to candidate
#'   genera (exact + fuzzy) before running the matching pipeline.
#' @param allow_duplicates Logical. If `TRUE`, duplicated taxon keys are
#'   deduplicated internally for matching and then expanded back to original
#'   rows. Output includes `input_index` for traceability to the original input.
#' @param max_dist Maximum distance used in all fuzzy matching stages (genus, species, infraspecies).
#' @param method A string indicating the fuzzy matching method (passed to `fozziejoin`). Supported methods:
#'   - `"levenshtein"`: Levenshtein edit distance (default).
#'   - `"osa"`: Optimal string alignment.
#'   - `"damerau_levensthein"` or `"dl"`: Damerau-Levenshtein distance.
#'   - `"hamming"`: Hamming distance (equal-length strings only).
#'   - `"lcs"`: Longest common subsequence.
#'   - `"qgram"`: Q-gram similarity (requires `q`).
#'   - `"cosine"`: Cosine similarity (requires `q`).
#'   - `"jaccard"`: Jaccard similarity (requires `q`).
#'   - `"jaro"`: Jaro similarity.
#'   - `"jaro_winkler"` or `"jw"`: Jaro-Winkler similarity.
#'   - `"soundex"`: Soundex codes based on the National Archives standard.
#'
#' @return Tibble with matched names and process flags.
#' @export
wcvp_matching <- function(df,
                     target_df = NULL,
                     prefilter_genus = FALSE,
                     allow_duplicates = FALSE,
                     max_dist = 1,
                     method = "osa") {
  coerce_tax_char <- function(x) {
    char_cols <- c("Orig.Genus", "Orig.Species", "Infra.Rank", "Orig.Infraspecies",
                   "Matched.Genus", "Matched.Species", "Matched.Infra.Rank", "Matched.Infraspecies")
    present <- char_cols[char_cols %in% names(x)]
    if (length(present) == 0) return(x)
    dplyr::mutate(x, dplyr::across(dplyr::all_of(present), as.character))
  }

  df <- check_df_format(df)
  df$input_index <- seq_len(nrow(df))

  is_binom <- !is.na(df$Orig.Species) & (is.na(df$Rank) | df$Rank <= 2)
  is_trinom <- !is.na(df$Rank) & df$Rank == 3

  dedup_key <- rep(NA_character_, nrow(df))
  dedup_key[is_binom] <- paste(
    "R2",
    df$Orig.Genus[is_binom],
    df$Orig.Species[is_binom],
    sep = "|"
  )
  dedup_key[is_trinom] <- paste(
    "R3",
    df$Orig.Genus[is_trinom],
    df$Orig.Species[is_trinom],
    dplyr::coalesce(df$Infra.Rank[is_trinom], "__NA__"),
    dplyr::coalesce(df$Orig.Infraspecies[is_trinom], "__NA__"),
    sep = "|"
  )
  dedup_key[is.na(dedup_key)] <- paste0("ROW|", df$input_index[is.na(dedup_key)])
  df$.dedup_key <- dedup_key

  had_duplicates <- any(duplicated(df$.dedup_key))
  if (had_duplicates && !isTRUE(allow_duplicates)) {
    dup_counts <- df %>%
      dplyr::count(.dedup_key, name = "n") %>%
      dplyr::filter(n > 1)
    n_dup_keys <- nrow(dup_counts)
    n_dup_rows <- sum(dup_counts$n) - n_dup_keys
    stop(
      paste(
        "Duplicate taxon keys detected in input (", n_dup_keys, " duplicated key(s), ",
        n_dup_rows, " extra row(s)).",
        "\nUse `allow_duplicates = TRUE` to keep all input rows and expand results by `input_index`.",
        "\nOr deduplicate manually, e.g. `dplyr::distinct(df, Orig.Genus, Orig.Species)` for rank <= 2.",
        sep = ""
      ),
      call. = FALSE
    )
  }

  df_work <- if (isTRUE(allow_duplicates)) {
    df %>%
      dplyr::group_by(.dedup_key) %>%
      dplyr::slice_head(n = 1) %>%
      dplyr::ungroup()
  } else {
    df
  }

  check_df_consistency(df_work)

  target_df <- get_db(target_df = target_df)
  if (isTRUE(prefilter_genus)) {
    target_df <- prefilter_target_by_genus(
      df = df_work,
      target_df = target_df,
      include_fuzzy = TRUE,
      max_dist = max_dist,
      method = method
    )
  }

  node_1 <- direct_match(df_work, target_df = target_df)
  n1_true <- dplyr::filter(node_1, direct_match)
  n1_false <- dplyr::filter(node_1, !direct_match)

  node_2 <- genus_match(n1_false, target_df = target_df)
  n2_true <- dplyr::filter(node_2, genus_match)
  n2_false <- dplyr::filter(node_2, !genus_match)

  node_3 <- fuzzy_match_genus(
    n2_false,
    target_df = target_df,
    max_dist = max_dist,
    method = method
  )
  n3_true <- dplyr::filter(node_3, fuzzy_match_genus)
  n3_false <- dplyr::filter(node_3, !fuzzy_match_genus)

  node_4_in <- dplyr::bind_rows(coerce_tax_char(n2_true), coerce_tax_char(n3_true))
  node_4 <- direct_match_species_within_genus(
    node_4_in,
    target_df = target_df
  )
  n4_true <- dplyr::filter(node_4, direct_match_species_within_genus)
  n4_false <- dplyr::filter(node_4, !direct_match_species_within_genus)

  node_5a <- suffix_match_species_within_genus(
    n4_false,
    target_df = target_df
  )
  n5a_true <- dplyr::filter(node_5a, suffix_match_species_within_genus)
  n5a_false <- dplyr::filter(node_5a, !suffix_match_species_within_genus)

  node_5b <- fuzzy_match_species_within_genus(
    n5a_false,
    target_df = target_df,
    max_dist = max_dist,
    method = method
  )
  n5b_true <- dplyr::filter(node_5b, fuzzy_match_species_within_genus)
  n5b_false <- dplyr::filter(node_5b, !fuzzy_match_species_within_genus)

  species_matched <- dplyr::bind_rows(
    coerce_tax_char(n1_true),
    coerce_tax_char(n4_true),
    coerce_tax_char(n5a_true),
    coerce_tax_char(n5b_true)
  )
  species_unmatched <- dplyr::bind_rows(coerce_tax_char(n3_false), coerce_tax_char(n5b_false))

  has_infra <- all(c("Infra.Rank", "Orig.Infraspecies") %in% names(df_work))
  ambiguous_infraspecies <- NULL

  if (has_infra) {
    sp_with_infra <- dplyr::filter(species_matched, !is.na(Orig.Infraspecies))
    sp_without_infra <- dplyr::filter(species_matched, is.na(Orig.Infraspecies)) %>%
      dplyr::mutate(
        direct_match_infra_rank = NA,
        fuzzy_match_infraspecies = NA,
        fuzzy_infraspecies_dist = NA_real_
      )

    if (nrow(sp_with_infra) > 0) {
      node_6 <- direct_match_infra_rank_within_species(
        sp_with_infra,
        target_df = target_df
      )
      n6_true <- dplyr::filter(node_6, direct_match_infra_rank)
      n6_false <- dplyr::filter(node_6, !direct_match_infra_rank) %>%
        dplyr::mutate(
          fuzzy_match_infraspecies = NA,
          fuzzy_infraspecies_dist = NA_real_
        )

      node_7 <- fuzzy_match_infraspecies_within_species(
        n6_true,
        target_df = target_df,
        max_dist = max_dist,
        method = method
      )
      n7_true <- dplyr::filter(node_7, fuzzy_match_infraspecies)
      n7_false <- dplyr::filter(node_7, !fuzzy_match_infraspecies)

      matched <- dplyr::bind_rows(coerce_tax_char(sp_without_infra), coerce_tax_char(n7_true))
      unmatched <- dplyr::bind_rows(
        coerce_tax_char(species_unmatched),
        coerce_tax_char(n6_false),
        coerce_tax_char(n7_false)
      )
      ambiguous_infraspecies <- attr(node_7, "ambiguous_infraspecies")
    } else {
      matched <- sp_without_infra
      unmatched <- species_unmatched
    }
  } else {
    matched <- species_matched
    unmatched <- species_unmatched
  }

  res <- dplyr::bind_rows(coerce_tax_char(matched), coerce_tax_char(unmatched), .id = "matched") %>%
    dplyr::mutate(
      matched = (matched == "1") & !is.na(Orig.Species)
    ) %>%
    dplyr::arrange(sorter)

  assertthat::assert_that(nrow(df_work) == nrow(res))

  if (isTRUE(allow_duplicates) && had_duplicates) {
    result_cols <- setdiff(
      names(res),
      c(
        "Input.Name", "Orig.Name", "Author", "sorter",
        "Orig.Genus", "Orig.Species", "Infra.Rank", "Orig.Infraspecies",
        "Rank", "has_cf", "has_aff", "is_sp", "is_spp", "had_hybrid",
        "rank_late", "rank_missing_infra", "had_na_author", "implied_infra",
        "input_index"
      )
    )

    original_cols <- intersect(
      c(
        "input_index", "Input.Name", "Orig.Name", "Author", "sorter",
        "Orig.Genus", "Orig.Species", "Infra.Rank", "Orig.Infraspecies",
        "Rank", "has_cf", "has_aff", "is_sp", "is_spp", "had_hybrid",
        "rank_late", "rank_missing_infra", "had_na_author", "implied_infra"
      ),
      names(df)
    )

    res <- df %>%
      dplyr::select(.dedup_key, dplyr::all_of(original_cols)) %>%
      dplyr::left_join(
        res %>% dplyr::select(.dedup_key, dplyr::all_of(result_cols)),
        by = ".dedup_key"
      ) %>%
      dplyr::arrange(sorter)
  }

  # Internal key is only used for dedup/expansion and should not leak to users.
  res <- dplyr::select(res, -dplyr::any_of(".dedup_key"))

  relocate_cols <- c(
    "input_index",
    "Input.Name",
    "Orig.Name",
    "Orig.Genus", "Orig.Species", "Infra.Rank", "Orig.Infraspecies",
    "Matched.Genus", "Matched.Species", "Matched.Infra.Rank", "Matched.Infraspecies",
    "Author",
    "matched",
    "direct_match", "genus_match", "fuzzy_match_genus",
    "direct_match_species_within_genus", "suffix_match_species_within_genus", "fuzzy_match_species_within_genus",
    "direct_match_infra_rank", "fuzzy_match_infraspecies", "fuzzy_infraspecies_dist"
  )
  res <- dplyr::relocate(res, dplyr::all_of(relocate_cols[relocate_cols %in% names(res)]))

  assertthat::assert_that(nrow(df) == nrow(res))

  if (!is.null(ambiguous_infraspecies) && nrow(ambiguous_infraspecies) > 0) {
    attr(res, "ambiguous_infraspecies") <- ambiguous_infraspecies
  }

  res
}
