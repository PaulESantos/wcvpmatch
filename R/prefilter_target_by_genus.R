#' Build a Genus Index for Fast Prefiltering
#'
#' @description
#' Creates a compact genus-level index from the target backbone. The index stores
#' one row per genus and a list-column with candidate `plant_name_id` values
#' associated with each genus.
#'
#' If `plant_name_id` is not present in `target_df`, a surrogate integer ID is
#' created to keep the index usable with custom backbones.
#'
#' @param target_df Optional custom target table; if `NULL`, uses `wcvpdata::wcvp_checklist_names`.
#'
#' @return A tibble with columns:
#' \describe{
#'   \item{genus}{Genus name (character).}
#'   \item{plant_name_id}{List-column of unique IDs per genus.}
#'   \item{n_records}{Number of IDs per genus.}
#' }
#' @export
build_genus_index <- function(target_df = NULL) {
  target_norm <- if (is.null(target_df)) default_target_df() else normalize_target_df(target_df)

  if (!"plant_name_id" %in% names(target_norm)) {
    target_norm <- dplyr::mutate(target_norm, plant_name_id = as.numeric(dplyr::row_number()))
  }

  target_norm %>%
    dplyr::filter(!is.na(genus), nzchar(genus)) %>%
    dplyr::group_by(genus) %>%
    dplyr::summarise(
      n_records = dplyr::n_distinct(plant_name_id),
      plant_name_id = list(unique(plant_name_id)),
      .groups = "drop"
    )
}


#' Prefilter Target Backbone by Input Genera (Exact + Fuzzy)
#'
#' @description
#' Reduces the target backbone to genera relevant for the current input names.
#' This is designed as a pre-step before `wcvp_matching()` to reduce search space.
#'
#' Strategy:
#' \itemize{
#'   \item Exact genus candidates are always included.
#'   \item Optional fuzzy genus candidates are included when `include_fuzzy = TRUE`.
#'   \item Returned object preserves the standard target schema used by the package.
#' }
#'
#' @param df Input tibble/data.frame with either `Genus`/`Species` or `Orig.Genus`/`Orig.Species`.
#' @param target_df Optional custom target table; if `NULL`, uses `wcvpdata::wcvp_checklist_names`.
#' @param genus_index Optional pre-built index from `build_genus_index()`. If `NULL`, it is built on the fly.
#' @param include_fuzzy Logical. If `TRUE`, include fuzzy-matched genera.
#' @param max_dist Maximum fuzzy distance for genus matching (used when `include_fuzzy = TRUE`).
#' @param method String distance method passed to `fozziejoin`.
#'
#' @return A prefiltered `target_df` tibble compatible with `wcvp_matching(target_df = ...)`.
#' Attributes:
#' \describe{
#'   \item{candidate_genera}{Character vector of selected genera.}
#'   \item{exact_genera}{Character vector of exact matched genera.}
#'   \item{fuzzy_genera}{Character vector of fuzzy matched genera.}
#' }
#' @export
prefilter_target_by_genus <- function(df,
                                      target_df = NULL,
                                      genus_index = NULL,
                                      include_fuzzy = TRUE,
                                      max_dist = 1,
                                      method = "osa") {
  df <- check_df_format(df)
  target_norm <- get_db(target_df = target_df)

  if (is.null(genus_index)) {
    genus_index <- build_genus_index(target_df = target_norm)
  }

  assertthat::assert_that(
    all(c("genus", "plant_name_id") %in% names(genus_index)),
    msg = "genus_index must contain columns: genus, plant_name_id."
  )

  input_genera <- df %>%
    dplyr::distinct(Orig.Genus) %>%
    dplyr::filter(!is.na(Orig.Genus), nzchar(Orig.Genus))

  if (nrow(input_genera) == 0) {
    out <- target_norm %>% dplyr::slice(0)
    attr(out, "candidate_genera") <- character(0)
    attr(out, "exact_genera") <- character(0)
    attr(out, "fuzzy_genera") <- character(0)
    return(out)
  }

  available_genera <- genus_index %>% dplyr::distinct(genus)

  exact_genera <- input_genera %>%
    dplyr::semi_join(available_genera, by = c("Orig.Genus" = "genus")) %>%
    dplyr::pull(Orig.Genus) %>%
    unique()

  fuzzy_genera <- character(0)
  if (isTRUE(include_fuzzy)) {
    fuzzy_tbl <- input_genera %>%
      fozziejoin::fozzie_string_left_join(
        available_genera,
        by = c("Orig.Genus" = "genus"),
        max_distance = max_dist,
        method = method,
        distance_col = "fuzzy_genus_dist"
      ) %>%
      dplyr::filter(!is.na(genus), !is.na(fuzzy_genus_dist), fuzzy_genus_dist <= max_dist) %>%
      dplyr::group_by(Orig.Genus) %>%
      dplyr::filter(fuzzy_genus_dist == min(fuzzy_genus_dist)) %>%
      dplyr::ungroup()

    fuzzy_genera <- unique(fuzzy_tbl$genus)
  }

  candidate_genera <- unique(c(exact_genera, fuzzy_genera))

  if (length(candidate_genera) == 0) {
    out <- target_norm %>% dplyr::slice(0)
    attr(out, "candidate_genera") <- character(0)
    attr(out, "exact_genera") <- exact_genera
    attr(out, "fuzzy_genera") <- fuzzy_genera
    return(out)
  }

  out <- target_norm %>% dplyr::filter(genus %in% candidate_genera)
  attr(out, "candidate_genera") <- candidate_genera
  attr(out, "exact_genera") <- exact_genera
  attr(out, "fuzzy_genera") <- fuzzy_genera
  out
}
