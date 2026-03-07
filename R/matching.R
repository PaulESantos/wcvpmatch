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
#' @param add_name_distance Logical. If `TRUE`, add
#'   `matched_dist` as pairwise distance between input name
#'   (`Input.Name` fallback `Orig.Name`) and `matched_taxon_name`.
#' @param name_distance_method Method passed to `stringdist::stringdist`
#'   when `add_name_distance = TRUE` (for example `"osa"`).
#' @param output_name_style Naming style for output columns:
#'   - `"snake_case"` returns standardized lower snake_case names.
#'   - `"legacy"` keeps the historical mixed naming convention.
#'
#' @return Tibble with matched names, process flags, and taxonomic context
#'   columns: `matched_plant_name_id`, `matched_taxon_name`, `taxon_status`,
#'   `accepted_plant_name_id`, `accepted_taxon_name`, `is_accepted_name`.
#' @export
wcvp_matching <- function(df,
                     target_df = NULL,
                     prefilter_genus = TRUE,
                     allow_duplicates = FALSE,
                     max_dist = 1,
                     method = "osa",
                     add_name_distance = FALSE,
                     name_distance_method = "osa",
                     output_name_style = c("legacy", "snake_case")) {
  output_name_style <- match.arg(output_name_style)

  standardize_output_names <- function(x) {
    rename_map <- c(
      "Input.Name" = "input_name",
      "Orig.Name" = "orig_name",
      "Orig.Genus" = "orig_genus",
      "Orig.Species" = "orig_species",
      "Infra.Rank" = "infra_rank",
      "Orig.Infraspecies" = "orig_infraspecies",
      "Matched.Genus" = "matched_genus",
      "Matched.Species" = "matched_species",
      "Matched.Infra.Rank" = "matched_infra_rank",
      "Matched.Infraspecies" = "matched_infraspecies",
      "Author" = "author",
      "Rank" = "rank"
    )

    old_names <- names(x)
    mapped <- ifelse(old_names %in% names(rename_map), rename_map[old_names], old_names)
    mapped <- gsub("\\.", "_", mapped)
    mapped <- tolower(mapped)
    names(x) <- mapped
    x
  }
  add_taxonomic_context <- function(x, target_tbl) {
    meta_needed <- c("plant_name_id", "taxon_name", "taxon_status", "accepted_plant_name_id")
    has_taxon_authors <- "taxon_authors" %in% names(target_tbl)
    if (!all(meta_needed %in% names(target_tbl))) {
      x$matched_plant_name_id <- NA_real_
      x$matched_taxon_name <- NA_character_
      x$matched_taxon_authors <- NA_character_
      x$taxon_status <- NA_character_
      x$accepted_plant_name_id <- NA_real_
      x$accepted_taxon_name <- NA_character_
      x$accepted_taxon_authors <- NA_character_
      x$is_accepted_name <- as.logical(NA)
      return(x)
    }

    db_meta <- target_tbl %>%
      dplyr::select(
        genus, species, infraspecific_rank, infraspecies,
        plant_name_id, taxon_name, taxon_status, accepted_plant_name_id,
        dplyr::any_of("taxon_authors")
      ) %>%
      dplyr::mutate(
        taxon_authors = if (has_taxon_authors) as.character(taxon_authors) else NA_character_
      ) %>%
      dplyr::mutate(
        infraspecific_rank = .rank_to_upper(infraspecific_rank),
        .taxon_name_clean = tolower(stringr::str_squish(as.character(taxon_name)))
      ) %>%
      dplyr::distinct()

    accepted_lookup <- db_meta %>%
      dplyr::select(
        plant_name_id,
        accepted_taxon_name = taxon_name,
        accepted_taxon_authors = taxon_authors
      ) %>%
      dplyr::distinct()

    x_work <- x %>%
      dplyr::mutate(
        .row_id = dplyr::row_number(),
        .matched_rank_upper = .rank_to_upper(Matched.Infra.Rank),
        .input_name_clean = tolower(stringr::str_squish(dplyr::coalesce(Input.Name, Orig.Name, "")))
      )

    best_match <- x_work %>%
      dplyr::left_join(
        db_meta,
        by = c(
          "Matched.Genus" = "genus",
          "Matched.Species" = "species",
          ".matched_rank_upper" = "infraspecific_rank",
          "Matched.Infraspecies" = "infraspecies"
        )
      ) %>%
      dplyr::mutate(
        .name_exact = !is.na(taxon_name) & (.input_name_clean == .taxon_name_clean),
        .status_rank = dplyr::case_when(
          tolower(taxon_status) == "accepted" ~ 2L,
          tolower(taxon_status) == "synonym" ~ 1L,
          TRUE ~ 0L
        )
      ) %>%
      dplyr::group_by(.row_id) %>%
      dplyr::arrange(
        dplyr::desc(.name_exact),
        dplyr::desc(.status_rank),
        plant_name_id,
        .by_group = TRUE
      ) %>%
      dplyr::slice_head(n = 1) %>%
      dplyr::ungroup() %>%
      dplyr::select(.row_id, plant_name_id, taxon_name, taxon_authors, taxon_status, accepted_plant_name_id)

    out <- x_work %>%
      dplyr::left_join(best_match, by = ".row_id") %>%
      dplyr::rename(
        matched_plant_name_id = plant_name_id,
        matched_taxon_name = taxon_name,
        matched_taxon_authors = taxon_authors
      ) %>%
      dplyr::left_join(accepted_lookup, by = c("accepted_plant_name_id" = "plant_name_id")) %>%
      dplyr::mutate(
        is_accepted_name = dplyr::if_else(
          is.na(taxon_status),
          as.logical(NA),
          tolower(taxon_status) == "accepted"
        ),
        accepted_plant_name_id = dplyr::if_else(
          is_accepted_name & is.na(accepted_plant_name_id),
          matched_plant_name_id,
          accepted_plant_name_id
        ),
        accepted_taxon_name = dplyr::if_else(
          is_accepted_name & is.na(accepted_taxon_name),
          matched_taxon_name,
          accepted_taxon_name
        ),
        accepted_taxon_authors = dplyr::if_else(
          is_accepted_name & is.na(accepted_taxon_authors),
          matched_taxon_authors,
          accepted_taxon_authors
        )
      ) %>%
      dplyr::select(-dplyr::any_of(c(
        ".row_id", ".matched_rank_upper", ".input_name_clean"
      )))

    out
  }

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

  target_df_full <- get_db(target_df = target_df)
  target_df_match <- target_df_full
  if (isTRUE(prefilter_genus)) {
    target_df_match <- prefilter_target_by_genus(
      df = df_work,
      target_df = target_df_match,
      include_fuzzy = TRUE,
      max_dist = max_dist,
      method = method
    )
  }

  node_1 <- direct_match(df_work, target_df = target_df_match)
  n1_true <- dplyr::filter(node_1, direct_match)
  n1_false <- dplyr::filter(node_1, !direct_match)

  node_2 <- genus_match(n1_false, target_df = target_df_match)
  n2_true <- dplyr::filter(node_2, genus_match)
  n2_false <- dplyr::filter(node_2, !genus_match)

  node_3 <- fuzzy_match_genus(
    n2_false,
    target_df = target_df_match,
    max_dist = max_dist,
    method = method
  )
  ambiguous_genus <- attr(node_3, "ambiguous_genus")
  n3_true <- dplyr::filter(node_3, fuzzy_match_genus)
  n3_false <- dplyr::filter(node_3, !fuzzy_match_genus)

  node_4_in <- dplyr::bind_rows(coerce_tax_char(n2_true), coerce_tax_char(n3_true))
  node_4 <- direct_match_species_within_genus(
    node_4_in,
    target_df = target_df_match
  )
  n4_true <- dplyr::filter(node_4, direct_match_species_within_genus)
  n4_false <- dplyr::filter(node_4, !direct_match_species_within_genus)

  node_5a <- suffix_match_species_within_genus(
    n4_false,
    target_df = target_df_match
  )
  n5a_true <- dplyr::filter(node_5a, suffix_match_species_within_genus)
  n5a_false <- dplyr::filter(node_5a, !suffix_match_species_within_genus)

  node_5b <- fuzzy_match_species_within_genus(
    n5a_false,
    target_df = target_df_match,
    max_dist = max_dist,
    method = method
  )
  ambiguous_species <- attr(node_5b, "ambiguous_species")
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
        target_df = target_df_match
      )
      n6_true <- dplyr::filter(node_6, direct_match_infra_rank)
      n6_false <- dplyr::filter(node_6, !direct_match_infra_rank) %>%
        dplyr::mutate(
          fuzzy_match_infraspecies = NA,
          fuzzy_infraspecies_dist = NA_real_
        )

      node_7 <- fuzzy_match_infraspecies_within_species(
        n6_true,
        target_df = target_df_match,
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
  res <- add_taxonomic_context(res, target_df_full)

  # Safety guard: do not report rows as matched when no taxon context was found.
  # This keeps boolean match flags coherent with matched_taxon_name.
  has_taxon_context <- all(c(
    "plant_name_id", "taxon_name", "taxon_status", "accepted_plant_name_id"
  ) %in% names(target_df_full))

  if (has_taxon_context && all(c("matched", "matched_taxon_name") %in% names(res))) {
    inconsistent_match <- !is.na(res$matched) & res$matched & is.na(res$matched_taxon_name)
    if (any(inconsistent_match, na.rm = TRUE)) {
      res$matched[inconsistent_match] <- FALSE
    }
  }

  if (isTRUE(add_name_distance)) {
    input_name_vec <- dplyr::coalesce(res$Input.Name, res$Orig.Name, "")

    gen_vec <- if ("Matched.Genus" %in% names(res)) as.character(res$Matched.Genus) else rep(NA_character_, nrow(res))
    spp_vec <- if ("Matched.Species" %in% names(res)) as.character(res$Matched.Species) else rep(NA_character_, nrow(res))
    rk_vec <- if ("Matched.Infra.Rank" %in% names(res)) as.character(res$Matched.Infra.Rank) else rep(NA_character_, nrow(res))
    inf_vec <- if ("Matched.Infraspecies" %in% names(res)) as.character(res$Matched.Infraspecies) else rep(NA_character_, nrow(res))

    fallback_matched_name <- vapply(
      seq_len(nrow(res)),
      function(i) {
        parts <- c(gen_vec[i], spp_vec[i], rk_vec[i], inf_vec[i])
        parts <- parts[!is.na(parts) & nzchar(parts)]
        if (length(parts) == 0) return(NA_character_)
        paste(parts, collapse = " ")
      },
      FUN.VALUE = character(1)
    )

    matched_name_vec <- dplyr::coalesce(res$matched_taxon_name, fallback_matched_name, "")
    has_pair <- !is.na(matched_name_vec) &
      nzchar(input_name_vec) &
      nzchar(matched_name_vec)

    dist_out <- rep(NA_real_, nrow(res))
    if (any(has_pair)) {
      left_names <- tolower(stringr::str_squish(input_name_vec[has_pair]))
      right_names <- tolower(stringr::str_squish(matched_name_vec[has_pair]))
      dist_out[has_pair] <- stringdist::stringdist(
        a = left_names,
        b = right_names,
        method = name_distance_method,
        useBytes = TRUE
      )
    }
    res$matched_dist <- dist_out
  }

  relocate_cols <- c(
    "input_index",
    "Input.Name",
    "Orig.Name",
    "Orig.Genus", "Orig.Species", "Infra.Rank", "Orig.Infraspecies",
    "Matched.Genus", "Matched.Species", "Matched.Infra.Rank", "Matched.Infraspecies",
    "Author",
    "matched_plant_name_id", "matched_taxon_name", "matched_taxon_authors", "taxon_status",
    "accepted_plant_name_id", "accepted_taxon_name", "accepted_taxon_authors", "is_accepted_name",
    "matched_dist",
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
  if (!is.null(ambiguous_genus) && nrow(ambiguous_genus) > 0) {
    attr(res, "ambiguous_genus") <- ambiguous_genus
  }
  if (!is.null(ambiguous_species) && nrow(ambiguous_species) > 0) {
    attr(res, "ambiguous_species") <- ambiguous_species
  }

  if (identical(output_name_style, "snake_case")) {
    res <- standardize_output_names(res)
  }

  res
}
