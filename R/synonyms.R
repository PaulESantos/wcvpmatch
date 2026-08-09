#' Retrieve Synonyms Resolved Through the WCVP Backbone
#'
#' `r lifecycle::badge("stable")`
#'
#' Resolves submitted species names with [wcvp_matching()] and retrieves every
#' WCVP record with `taxon_status = "Synonym"` that points to the resolved
#' accepted name. This means that an accepted name, a synonym, or a minor
#' spelling variant can all be used as the query.
#'
#' Exact submitted names use a direct lookup before invoking the fuzzy matching
#' pipeline. The accepted-name lookup and normalized default backbone are
#' cached for the current R session and shared with the other package functions.
#'
#' @param taxon Character vector of species names to resolve.
#' @param target_df Optional WCVP-like names table. If `NULL`, the optional
#'   `wcvpdata` checklist is used.
#' @param prefilter_genus Logical. Passed to [wcvp_matching()].
#' @param max_dist Maximum string distance used while resolving `taxon`.
#' @param method String distance method passed to [wcvp_matching()].
#' @param include_accepted Logical. Include the accepted name as a row with
#'   `name_role = "accepted"` before its synonyms? Defaults to `FALSE`.
#' @param output Output layout. `"compact"` (the default) returns the accepted
#'   name and its synonym fields. `"full"` additionally returns matching
#'   diagnostics, WCVP identifiers, basionym ID, and retrieval status.
#'
#' @return A tibble with one row per input name and retrieved synonym. The
#'   default compact result contains `submitted_name`, `accepted_taxon_name`,
#'   `accepted_taxon_authors`, `name_role`, `synonym_name`,
#'   `synonym_authors`, and `homotypic_synonym`. Inputs without a match or
#'   without recorded synonyms are retained with missing synonym fields.
#'
#' @examplesIf rlang::is_installed("wcvpdata")
#' \donttest{
#' wcvp_synonyms("Nopalea cochenillifera")
#' }
#' @export
wcvp_synonyms <- function(taxon,
                          target_df = NULL,
                          prefilter_genus = TRUE,
                          max_dist = 2,
                          method = "osa",
                          include_accepted = FALSE,
                          output = c("compact", "full")) {
  assertthat::assert_that(
    is.character(taxon),
    length(taxon) > 0,
    msg = "taxon must be a non-empty character vector."
  )
  assertthat::assert_that(
    is.logical(include_accepted),
    length(include_accepted) == 1,
    !is.na(include_accepted),
    msg = "include_accepted must be TRUE or FALSE."
  )
  output <- match.arg(output)

  use_default <- is.null(target_df)
  names_tbl <- if (use_default) default_target_df() else normalize_target_df(target_df)

  matches <- match_synonym_queries(
    taxon = taxon,
    names_tbl = names_tbl,
    use_default = use_default,
    prefilter_genus = prefilter_genus,
    max_dist = max_dist,
    method = method
  )

  accepted_lookup <- build_synonym_accepted_lookup(names_tbl)
  resolved_ids <- unique(stats::na.omit(matches$accepted_plant_name_id))
  accepted_lookup <- accepted_lookup[
    accepted_lookup$accepted_plant_name_id %in% resolved_ids,
    ,
    drop = FALSE
  ]

  resolved_matches <- matches %>%
    dplyr::left_join(accepted_lookup, by = "accepted_plant_name_id") %>%
    dplyr::mutate(
      accepted_taxon_name = dplyr::coalesce(
        .data$accepted_taxon_name_from_backbone,
        .data$accepted_taxon_name
      )
    ) %>%
    dplyr::select(-accepted_taxon_name_from_backbone)

  requested_ids <- unique(stats::na.omit(resolved_matches$accepted_plant_name_id))
  synonym_rows <- build_synonym_rows(names_tbl, accepted_ids = requested_ids)

  found <- resolved_matches %>%
    dplyr::filter(.data$matched, !is.na(.data$accepted_plant_name_id)) %>%
    dplyr::left_join(synonym_rows, by = "accepted_plant_name_id") %>%
    dplyr::mutate(
      name_role = dplyr::if_else(
        is.na(.data$synonym_plant_name_id),
        "no_synonyms",
        "synonym"
      ),
      synonym_status = dplyr::if_else(is.na(.data$synonym_plant_name_id), "no_synonyms", "synonym_found")
    )

  if (isTRUE(include_accepted)) {
    accepted_rows <- resolved_matches %>%
      dplyr::filter(.data$matched, !is.na(.data$accepted_plant_name_id)) %>%
      dplyr::transmute(
        input_index, submitted_name, matched, matched_taxon_name, match_distance,
        accepted_plant_name_id, accepted_taxon_name, accepted_taxon_authors,
        name_role = "accepted",
        synonym_plant_name_id = NA_real_,
        synonym_name = NA_character_,
        synonym_authors = NA_character_,
        homotypic_synonym = as.logical(NA),
        basionym_plant_name_id = NA_character_,
        synonym_status = "accepted_name"
      )
    found <- dplyr::bind_rows(
      accepted_rows,
      dplyr::filter(found, .data$synonym_status != "no_synonyms")
    )
  }

  no_match <- resolved_matches %>%
    dplyr::filter(!.data$matched | is.na(.data$accepted_plant_name_id)) %>%
    synonym_output_placeholder("no_match")

  out <- dplyr::bind_rows(found, no_match) %>%
    dplyr::select(
      input_index, submitted_name, matched, matched_taxon_name, match_distance,
      accepted_plant_name_id, accepted_taxon_name, accepted_taxon_authors,
      name_role, synonym_plant_name_id, synonym_name, synonym_authors,
      homotypic_synonym, basionym_plant_name_id, synonym_status
    ) %>%
    dplyr::arrange(input_index, dplyr::desc(.data$name_role), .data$synonym_name)

  if (identical(output, "full")) {
    return(out)
  }

  out %>%
    dplyr::select(
      submitted_name,
      accepted_taxon_name,
      accepted_taxon_authors,
      name_role,
      synonym_name,
      synonym_authors,
      homotypic_synonym
    )
}

match_synonym_queries <- function(taxon,
                                  names_tbl,
                                  use_default,
                                  prefilter_genus,
                                  max_dist,
                                  method) {
  submitted <- stringr::str_squish(as.character(taxon))
  exact_idx <- match(submitted, as.character(names_tbl$taxon_name))
  exact_ok <- !is.na(exact_idx)

  # A taxon name can occur more than once in nomenclatural data. Reproduce the
  # matching core's preference for an accepted record when exact candidates
  # are tied, while touching only rows for submitted exact names.
  if (any(exact_ok)) {
    exact_values <- unique(submitted[exact_ok])
    candidate_pos <- which(as.character(names_tbl$taxon_name) %in% exact_values)
    candidates <- names_tbl[candidate_pos, , drop = FALSE]
    status_rank <- ifelse(
      tolower(as.character(candidates$taxon_status)) == "accepted", 2L,
      ifelse(tolower(as.character(candidates$taxon_status)) == "synonym", 1L, 0L)
    )
    candidates$.source_row <- candidate_pos
    candidates$.status_rank <- status_rank
    preferred <- candidates %>%
      dplyr::arrange(.data$taxon_name, dplyr::desc(.data$.status_rank), .data$plant_name_id) %>%
      dplyr::distinct(.data$taxon_name, .keep_all = TRUE)
    preferred_map <- stats::setNames(preferred$.source_row, preferred$taxon_name)
    exact_idx[exact_ok] <- unname(preferred_map[submitted[exact_ok]])
  }

  exact_rows <- tibble::tibble()
  if (any(exact_ok)) {
    x <- names_tbl[exact_idx[exact_ok], , drop = FALSE]
    accepted_id <- dplyr::coalesce(
      as.numeric(x$accepted_plant_name_id),
      as.numeric(x$plant_name_id)
    )
    accepted_pos <- match(accepted_id, names_tbl$plant_name_id)
    accepted_name <- as.character(x$taxon_name)
    has_accepted <- !is.na(accepted_pos)
    accepted_name[has_accepted] <- as.character(names_tbl$taxon_name[accepted_pos[has_accepted]])

    exact_rows <- tibble::tibble(
      input_index = which(exact_ok),
      submitted_name = submitted[exact_ok],
      matched = TRUE,
      matched_taxon_name = as.character(x$taxon_name),
      match_distance = 0,
      accepted_plant_name_id = accepted_id,
      accepted_taxon_name = accepted_name
    )
  }

  unresolved <- which(!exact_ok)
  fuzzy_rows <- tibble::tibble()
  if (length(unresolved) > 0) {
    input_map <- tibble::tibble(
      backend_input_index = seq_along(unresolved),
      input_index = unresolved
    )
    fuzzy_rows <- classify_spnames(submitted[unresolved]) %>%
      wcvp_matching(
        target_df = if (use_default) NULL else names_tbl,
        prefilter_genus = prefilter_genus,
        allow_duplicates = TRUE,
        max_dist = max_dist,
        method = method,
        add_name_distance = TRUE,
        output_name_style = "snake_case",
        output = "full"
      ) %>%
      dplyr::transmute(
        backend_input_index = .data$input_index,
        submitted_name = .data$input_name,
        matched = dplyr::coalesce(.data$matched, FALSE),
        matched_taxon_name = .data$matched_taxon_name,
        match_distance = .data$matched_dist,
        accepted_plant_name_id = dplyr::coalesce(
          .data$accepted_plant_name_id,
          .data$matched_plant_name_id
        ),
        accepted_taxon_name = dplyr::coalesce(
          .data$accepted_taxon_name,
          .data$matched_taxon_name
        )
      ) %>%
      dplyr::left_join(input_map, by = "backend_input_index") %>%
      dplyr::select(-backend_input_index)
  }

  dplyr::bind_rows(exact_rows, fuzzy_rows) %>%
    dplyr::arrange(input_index)
}

build_synonym_accepted_lookup <- function(names_tbl) {
  is_default <- identical(attr(names_tbl, "wcvpmatch_source", exact = TRUE), "default")
  if (is_default) {
    cached <- .wcvpmatch_cache[["default_synonym_accepted_lookup"]]
    if (!is.null(cached)) return(cached)
  }

  keep <- !is.na(names_tbl$plant_name_id) & (
    tolower(as.character(names_tbl$taxon_status)) %in% "accepted" |
      (!is.na(names_tbl$accepted_plant_name_id) &
        names_tbl$plant_name_id == names_tbl$accepted_plant_name_id)
  )
  x <- names_tbl[keep, , drop = FALSE]
  out <- tibble::tibble(
    accepted_plant_name_id = as.numeric(x$plant_name_id),
    accepted_taxon_name_from_backbone = as.character(x$taxon_name),
    accepted_taxon_authors = if ("taxon_authors" %in% names(x)) {
      as.character(x$taxon_authors)
    } else {
      rep(NA_character_, nrow(x))
    }
  )
  if (anyDuplicated(out$accepted_plant_name_id)) {
    out <- dplyr::distinct(out)
  }

  if (is_default) .wcvpmatch_cache[["default_synonym_accepted_lookup"]] <- out
  out
}

build_synonym_rows <- function(names_tbl, accepted_ids = NULL) {
  is_synonym <- tolower(as.character(names_tbl$taxon_status)) %in% "synonym" &
    !is.na(names_tbl$accepted_plant_name_id)
  if (!is.null(accepted_ids)) {
    is_synonym <- is_synonym & names_tbl$accepted_plant_name_id %in% accepted_ids
  }
  x <- names_tbl[is_synonym, , drop = FALSE]

  tibble::tibble(
    accepted_plant_name_id = as.numeric(x$accepted_plant_name_id),
    synonym_plant_name_id = as.numeric(x$plant_name_id),
    synonym_name = as.character(x$taxon_name),
    synonym_authors = if ("taxon_authors" %in% names(x)) as.character(x$taxon_authors) else rep(NA_character_, nrow(x)),
    homotypic_synonym = if ("homotypic_synonym" %in% names(x)) as.logical(x$homotypic_synonym) else rep(as.logical(NA), nrow(x)),
    basionym_plant_name_id = if ("basionym_plant_name_id" %in% names(x)) as.character(x$basionym_plant_name_id) else rep(NA_character_, nrow(x))
  ) %>%
    dplyr::distinct()
}

synonym_output_placeholder <- function(x, status) {
  x %>%
    dplyr::transmute(
      input_index, submitted_name, matched, matched_taxon_name, match_distance,
      accepted_plant_name_id, accepted_taxon_name, accepted_taxon_authors,
      name_role = NA_character_,
      synonym_plant_name_id = NA_real_,
      synonym_name = NA_character_,
      synonym_authors = NA_character_,
      homotypic_synonym = as.logical(NA),
      basionym_plant_name_id = NA_character_,
      synonym_status = status
    )
}
