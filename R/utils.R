check_df_format <- function(df) {
  if (!tibble::is_tibble(df) && inherits(df, "data.frame")) {
    df <- tibble::as_tibble(df)
    message(
      "Input was converted from data.frame to a tibble.\n",
      "See https://tibble.tidyverse.org/ for more details."
    )
  }
  assertthat::assert_that(tibble::is_tibble(df))

  has_classify <- all(c("Orig.Genus", "Orig.Species") %in% names(df))
  has_minimal  <- all(c("Genus", "Species") %in% names(df))

  assertthat::assert_that(
    has_classify || has_minimal,
    msg = "Input must contain either {Orig.Genus, Orig.Species} or {Genus, Species}."
  )

  if (!has_classify && has_minimal) {
    df <- dplyr::rename(df, Orig.Genus = Genus, Orig.Species = Species)
  }

  if ("Orig.Infra.Rank" %in% names(df) && !"Infra.Rank" %in% names(df)) {
    df <- dplyr::rename(df, Infra.Rank = Orig.Infra.Rank)
  }
  if ("Infraspecies" %in% names(df) && !"Orig.Infraspecies" %in% names(df)) {
    df <- dplyr::rename(df, Orig.Infraspecies = Infraspecies)
  }

  n <- nrow(df)
  if (!"Input.Name" %in% names(df)) df$Input.Name <- rep(NA_character_, n)
  if (!"Orig.Name" %in% names(df)) df$Orig.Name <- rep(NA_character_, n)
  if (!"Author" %in% names(df)) df$Author <- rep("", n)

  if (!"Orig.Infraspecies" %in% names(df)) df$Orig.Infraspecies <- rep(NA_character_, n)
  if (!"Infra.Rank" %in% names(df)) df$Infra.Rank <- rep(NA_character_, n)
  if (!"Rank" %in% names(df)) df$Rank <- rep(NA_real_, n)
  df$Infra.Rank <- .rank_to_lower(df$Infra.Rank)

  # Early whitespace hygiene for taxonomic fields
  if (any(stringr::str_detect(df$Orig.Genus, "^\\s|\\s$"), na.rm = TRUE)) {
    ng <- sum(stringr::str_detect(df$Orig.Genus, "^\\s|\\s$"), na.rm = TRUE)
    warning(sprintf(
      "%s leading/trailing space(s) detected in Orig.Genus. Consider stringr::str_trim().",
      ng
    ))
  }
  if (any(!is.na(df$Orig.Species) & stringr::str_detect(df$Orig.Species, "^\\s|\\s$"), na.rm = TRUE)) {
    nsp <- sum(!is.na(df$Orig.Species) & stringr::str_detect(df$Orig.Species, "^\\s|\\s$"), na.rm = TRUE)
    warning(sprintf(
      "%s leading/trailing space(s) detected in Orig.Species. Consider stringr::str_trim().",
      nsp
    ))
  }
  if (any(!is.na(df$Orig.Infraspecies) & stringr::str_detect(df$Orig.Infraspecies, "^\\s|\\s$"), na.rm = TRUE)) {
    ninf <- sum(!is.na(df$Orig.Infraspecies) & stringr::str_detect(df$Orig.Infraspecies, "^\\s|\\s$"), na.rm = TRUE)
    warning(sprintf(
      "%s leading/trailing space(s) detected in Orig.Infraspecies. Consider stringr::str_trim().",
      ninf
    ))
  }
  if (any(!is.na(df$Infra.Rank) & stringr::str_detect(df$Infra.Rank, "^\\s|\\s$"), na.rm = TRUE)) {
    nrk <- sum(!is.na(df$Infra.Rank) & stringr::str_detect(df$Infra.Rank, "^\\s|\\s$"), na.rm = TRUE)
    warning(sprintf(
      "%s leading/trailing space(s) detected in Infra.Rank. Consider stringr::str_trim().",
      nrk
    ))
  }

  df <- dplyr::mutate(
    df,
    Orig.Genus = as.character(Orig.Genus),
    Orig.Species = as.character(Orig.Species),
    Orig.Infraspecies = as.character(Orig.Infraspecies),
    Infra.Rank = as.character(Infra.Rank)
  )

  df <- dplyr::mutate(
    df,
    Orig.Genus = stringr::str_trim(Orig.Genus),
    Orig.Species = stringr::str_trim(Orig.Species),
    Orig.Infraspecies = stringr::str_trim(Orig.Infraspecies),
    Infra.Rank = stringr::str_trim(Infra.Rank)
  )

  # Build Input.Name when not provided, preserving existing non-empty values.
  build_input_name <- function(genus, species, infra_rank, infraspecies) {
    parts <- c(genus, species, infra_rank, infraspecies)
    parts <- parts[!is.na(parts) & nzchar(parts)]
    if (length(parts) == 0) return(NA_character_)
    paste(parts, collapse = " ")
  }
  generated_input_name <- vapply(
    seq_len(nrow(df)),
    function(i) {
      build_input_name(
        df$Orig.Genus[i],
        df$Orig.Species[i],
        df$Infra.Rank[i],
        df$Orig.Infraspecies[i]
      )
    },
    FUN.VALUE = character(1)
  )
  df <- dplyr::mutate(
    df,
    Input.Name = as.character(Input.Name),
    Input.Name = dplyr::if_else(
      !is.na(Input.Name) & nzchar(stringr::str_trim(Input.Name)),
      stringr::str_trim(Input.Name),
      generated_input_name
    )
  )

  # Flags (if absent, add FALSE)
  flag_cols <- c(
    "has_cf","has_aff","is_sp","is_spp","had_hybrid",
    "rank_late","rank_missing_infra","had_na_author","implied_infra"
  )
  for (nm in flag_cols) {
    if (!nm %in% names(df)) df[[nm]] <- rep(FALSE, n)
    df[[nm]] <- as.logical(df[[nm]])
  }

  if (!"sorter" %in% names(df)) df$sorter <- seq_len(n)
  df$sorter <- as.numeric(df$sorter)

  df
}

# ---------------------------------------------------------------

.rank_to_upper <- function(x) {
  ifelse(
    is.na(x),
    NA_character_,
    dplyr::case_when(
      toupper(x) %in% c("SUBSP", "SUBSP.", "SSP", "SSP.") ~ "SUBSP.",
      toupper(x) %in% c("VAR", "VAR.") ~ "VAR.",
      toupper(x) %in% c("SUBVAR", "SUBVAR.") ~ "SUBVAR.",
      toupper(x) %in% c("F", "F.", "FO", "FO.", "FORM", "FORMA") ~ "F.",
      toupper(x) %in% c("SUBF", "SUBF.") ~ "SUBF.",
      TRUE ~ toupper(x)
    )
  )
}

.rank_to_lower <- function(x) {
  ifelse(
    is.na(x),
    NA_character_,
    dplyr::case_when(
      toupper(x) %in% c("SUBSP", "SUBSP.", "SSP", "SSP.") ~ "subsp.",
      toupper(x) %in% c("VAR", "VAR.") ~ "var.",
      toupper(x) %in% c("SUBVAR", "SUBVAR.") ~ "subvar.",
      toupper(x) %in% c("F", "F.", "FO", "FO.", "FORM", "FORMA") ~ "f.",
      toupper(x) %in% c("SUBF", "SUBF.") ~ "subf.",
      TRUE ~ tolower(x)
    )
  )
}

normalize_target_df <- function(target_df) {
  assertthat::assert_that(
    inherits(target_df, "data.frame"),
    msg = "target_df must be a data.frame/tibble."
  )

  x <- tibble::as_tibble(target_df)

  if (!("genus" %in% names(x))) {
    if ("Genus" %in% names(x)) x <- dplyr::rename(x, genus = Genus)
  }
  if (!("species" %in% names(x))) {
    if ("Species" %in% names(x)) x <- dplyr::rename(x, species = Species)
  }
  if (!("infraspecies" %in% names(x))) {
    if ("Orig.Infraspecies" %in% names(x)) x <- dplyr::rename(x, infraspecies = Orig.Infraspecies)
    if ("Infraspecies" %in% names(x)) x <- dplyr::rename(x, infraspecies = Infraspecies)
  }
  if (!("infraspecific_rank" %in% names(x))) {
    if ("Infra.Rank" %in% names(x)) x <- dplyr::rename(x, infraspecific_rank = Infra.Rank)
  }

  assertthat::assert_that(
    all(c("genus", "species") %in% names(x)),
    msg = "target_df must contain genus/species (or Genus/Species)."
  )

  if (!("infraspecific_rank" %in% names(x))) x$infraspecific_rank <- NA_character_
  if (!("infraspecies" %in% names(x))) x$infraspecies <- NA_character_

  x <- x %>%
    dplyr::mutate(
      genus = as.character(genus),
      species = as.character(species),
      infraspecific_rank = .rank_to_upper(as.character(infraspecific_rank)),
      infraspecies = as.character(infraspecies)
    ) %>%
    dplyr::mutate(
      Genus = genus,
      Species = species
    )

  x
}

default_target_df <- function() {
  .require_rwcvpdata()

  # Read dataset directly, fail with clear message if object is unavailable.
  wcvp_data <- tryCatch({
    rWCVPdata::wcvp_names
  }, error = function(e) NULL)

  if (is.null(wcvp_data)) {
    stop(
      paste(
        "Object 'wcvp_names' was not found in package 'rWCVPdata'.",
        "Please update/reinstall from:",
        "remotes::install_github('PaulESantos/rWCVPdata')",
        sep = "\n"
      ),
      call. = FALSE
    )
  }

  return(normalize_target_df(wcvp_data))
}

get_db <- function(target_df = NULL) {
  out <- if (!is.null(target_df)) normalize_target_df(target_df) else default_target_df()
  out %>%
    tidyr::drop_na(genus, species) %>%
    dplyr::distinct()
}

# ---------------------------------------------------------------

check_df_consistency <- function(df) {
  assertthat::assert_that(
    all(c("Orig.Genus","Orig.Species","Rank","is_sp","is_spp","implied_infra") %in% names(df)),
    msg = "Input must be normalized with check_df_format() first."
  )

  # ---- Errors ----

  # 1) Genus mandatory and non-empty
  assertthat::assert_that(
    !any(is.na(df$Orig.Genus)),
    msg = "Orig.Genus contains missing values. Please remove/fix conflicting rows."
  )
  assertthat::assert_that(
    all(nzchar(trimws(df$Orig.Genus))),
    msg = "Orig.Genus contains empty strings. Please remove/fix conflicting rows."
  )

  # 2) Species NA only for genus-only cases (Rank=1 or sp/spp)
  species_na <- is.na(df$Orig.Species)
  allowed_na <- (df$Rank == 1) | df$is_sp | df$is_spp
  assertthat::assert_that(
    all(!species_na | allowed_na),
    msg = paste0(
      "Orig.Species has missing values in rows that are not genus-only.\n",
      "Allowed NA only when Rank == 1 or is_sp/is_spp == TRUE."
    )
  )

  # 3) Rank coherence
  bad_rank1 <- !is.na(df$Rank) & df$Rank == 1 & !is.na(df$Orig.Species)
  assertthat::assert_that(
    !any(bad_rank1),
    msg = "Rank == 1 requires Orig.Species to be NA (genus-only)."
  )

  bad_rank2 <- !is.na(df$Rank) & df$Rank == 2 & is.na(df$Orig.Species)
  assertthat::assert_that(
    !any(bad_rank2),
    msg = "Rank == 2 requires Orig.Species to be present."
  )

  # Rank == 3 rules updated:
  # - must have species present
  # - must have infraspecies present unless rank_missing_infra == TRUE
  # - must have either:
  #   a) ranked infra: Infra.Rank present
  #   b) implied infra: implied_infra == TRUE and Infra.Rank is NA
  if (all(c("Infra.Rank","Orig.Infraspecies","rank_missing_infra") %in% names(df))) {
    is_rank3 <- !is.na(df$Rank) & df$Rank == 3

    missing_species <- is_rank3 & is.na(df$Orig.Species)

    missing_infra_ep <- is_rank3 & is.na(df$Orig.Infraspecies) & !df$rank_missing_infra

    # invalid rank3 structure:
    # if infra epithet exists:
    #   - either Infra.Rank exists, OR implied_infra TRUE with Infra.Rank NA
    # if infra epithet missing but rank_missing_infra TRUE => ok (rank present but infra missing)
    has_infra_ep <- is_rank3 & !is.na(df$Orig.Infraspecies)

    invalid_infra_form <- has_infra_ep & !(
      (!is.na(df$Infra.Rank)) |
        (df$implied_infra & is.na(df$Infra.Rank))
    )

    bad_rank3 <- missing_species | missing_infra_ep | invalid_infra_form

    assertthat::assert_that(
      !any(bad_rank3),
      msg = paste0(
        "Rank == 3 requires:\n",
        "- Orig.Species present, AND\n",
        "- Orig.Infraspecies present (unless rank_missing_infra == TRUE), AND\n",
        "- either Infra.Rank present (ranked infra) OR implied_infra == TRUE with Infra.Rank == NA (unranked infra)."
      )
    )
  }

  # 4) Uniqueness:
  # - binomials must be unique for rank <= 2
  # - trinomials can share the same binomial, but full infra tuple must be unique
  df_binom <- dplyr::filter(df, !is.na(Orig.Species), is.na(Rank) | Rank <= 2)
  assertthat::assert_that(
    nrow(df_binom) == nrow(dplyr::distinct(df_binom, Orig.Genus, Orig.Species)),
    msg = paste(
      "Species names are not unique for Rank <= 2.",
      "Remove duplicates with dplyr::distinct(df, Orig.Genus, Orig.Species)."
    )
  )

  if (all(c("Infra.Rank", "Orig.Infraspecies") %in% names(df))) {
    df_trinom <- dplyr::filter(df, !is.na(Rank), Rank == 3)
    assertthat::assert_that(
      nrow(df_trinom) == nrow(dplyr::distinct(df_trinom, Orig.Genus, Orig.Species, Infra.Rank, Orig.Infraspecies)),
      msg = paste(
        "Trinomial names are not unique.",
        "Use dplyr::distinct(df, Orig.Genus, Orig.Species, Infra.Rank, Orig.Infraspecies)."
      )
    )
  }

  # ---- Format checks (aligned to backbone) ----

  assertthat::assert_that(
    all(stringr::str_detect(df$Orig.Genus, "^[[:upper:]][[:lower:]]+$")),
    msg = paste(
      "Not all genera are in Title Case (e.g., 'Opuntia').",
      "Fix with: dplyr::mutate(Orig.Genus = stringr::str_to_sentence(Orig.Genus))."
    )
  )

  spp_present <- !is.na(df$Orig.Species)
  assertthat::assert_that(
    all(!spp_present | stringr::str_detect(df$Orig.Species, "^[[:lower:]-]+$")),
    msg = paste(
      "Some specific epithets contain uppercase letters or invalid characters.",
      "Fix with: dplyr::mutate(Orig.Species = stringr::str_to_lower(Orig.Species))."
    )
  )

  if ("Infra.Rank" %in% names(df)) {
    valid_ranks <- c("subsp.", "var.", "subvar.", "f.", "subf.", "SUBSP.", "VAR.", "SUBVAR.", "F.", "SUBF.")
    rk_present <- !is.na(df$Infra.Rank)
    assertthat::assert_that(
      all(!rk_present | df$Infra.Rank %in% valid_ranks),
      msg = "Infra.Rank contains unexpected values. Expected one of: subsp., var., subvar., f., subf."
    )
  }

  if ("Orig.Infraspecies" %in% names(df)) {
    infra_present <- !is.na(df$Orig.Infraspecies)
    assertthat::assert_that(
      all(!infra_present | stringr::str_detect(df$Orig.Infraspecies, "^[[:lower:]-]+$")),
      msg = "Some infraspecific epithets contain uppercase letters or invalid characters."
    )
  }

  # ---- Warnings ----
  options(warn = 1)

  if (any(stringr::str_detect(df$Orig.Genus, "^\\s|\\s$"))) {
    nsp <- sum(stringr::str_detect(df$Orig.Genus, "^\\s|\\s$"))
    warning(sprintf(
      "%s leading/trailing space(s) detected in Orig.Genus. Consider stringr::str_trim().",
      nsp
    ), call. = FALSE)
  }

  if (any(!is.na(df$Orig.Species) & stringr::str_detect(df$Orig.Species, "^\\s|\\s$"))) {
    nsp <- sum(!is.na(df$Orig.Species) & stringr::str_detect(df$Orig.Species, "^\\s|\\s$"))
    warning(sprintf(
      "%s leading/trailing space(s) detected in Orig.Species. Consider stringr::str_trim().",
      nsp
    ), call. = FALSE)
  }

  if (any(df$Rank == 1 | df$is_sp | df$is_spp, na.rm = TRUE)) {
    n_genus_only <- sum(df$Rank == 1 | df$is_sp | df$is_spp, na.rm = TRUE)
    warning(sprintf(
      "%s genus-only row(s) detected (Rank==1 / sp./spp.). These will not participate in species-level strict matching.",
      n_genus_only
    ), call. = FALSE)
  }

  if ("implied_infra" %in% names(df) && any(df$implied_infra)) {
    n_imp <- sum(df$implied_infra)
    warning(sprintf(
      "%s unranked infraspecific epithet(s) inferred (implied_infra == TRUE). Matching will use Genus + Species + Infraspecies without an explicit rank.",
      n_imp
    ), call. = FALSE)
  }

  df
}
# ---------------------------------------------------------------
## returns WCVP target rows filtered by a single genus
get_trees_of_genus <- function(genus, target_df = NULL){
  genus_values <- as.character(genus)
  return(get_db(target_df = target_df) %>%
           dplyr::filter(.data$Genus %in% .env$genus_values) %>%
           dplyr::select(c('Genus', 'Species')))
}
## locally save output of get_trees_of_genus of called more than once for the same inputs. --> maybe we should get rid of this, as I suppose it's not effectively speading up things due to the increased memory usage.
memoised_get_trees_of_genus <- memoise::memoise(get_trees_of_genus)




## analog to map_dfr, which additionally prints progress bars using the package progress
map_dfr_progress <- function(.x, .f, ..., .id = NULL) { ## credits to https://www.jamesatkins.net/posts/progress-bar-in-purrr-map-df/
  function_name <- stringr::str_remove(toString(substitute(.f)), '_helper')
  .f <- purrr::as_mapper(.f, ...)
  pb <- progress::progress_bar$new(total = length(.x),
                                   force = TRUE,
                                   format = paste(paste0(eval(...), collapse = ' '), ": ", function_name, "[:bar] :percent", collapse = ''))
  # pb <- progress::progress_bar$new(total = length(.x),
  #                                  force = TRUE,
  #                                  format = paste(paste0(eval(...), collapse = ' '), ": ", substitute(.f), "[:bar] :percent", collapse = ''))


  f <- function(...) {
    pb$tick()
    .f(...)
  }

  #future::plan(future::multicore, workers = 4)
  purrr::map_dfr(.x, f, ..., .id = .id)
}


## analog to map_dfr, which additionally prints progress bars using the package progress
map_progress <- function(.x, .f, ..., .id = NULL) { ## credits to https://www.jamesatkins.net/posts/progress-bar-in-purrr-map-df/
  function_name <- stringr::str_remove(toString(substitute(.f)), '_helper')
  .f <- purrr::as_mapper(.f, ...)
  pb <- progress::progress_bar$new(total = length(.x),
                                   force = TRUE,
                                   format = paste(paste0(eval(...), collapse = ' '), ": enforce_matching: [:bar] :percent", collapse = ''))

  f <- function(...) {
    pb$tick()
    .f(...)
  }

  #future::plan(future::multicore, workers = 4)
  purrr::map(.x, f, ...)
}


### potential implementation of parallel purrr using furrr:
# parallel + progress https://furrr.futureverse.org/articles/progress.html
# parallel: https://byuistats.github.io/M335/parallel_furrr.html

# map_dfr_progress_parallel <- function(.x, .f, ..., .id = NULL) { ## credits to https://www.jamesatkins.net/posts/progress-bar-in-purrr-map-df/
#   function_name <- stringr::str_remove(substitute(.f), '_helper')
#   .f <- purrr::as_mapper(.f, ...)
#   pb <- progress::progress_bar$new(total = length(.x),
#                                    force = TRUE,
#                                    format = paste(paste0(eval(...), collapse = ' '), ": ", function_name, "[:bar] :percent", collapse = ''))
#
#   f <- function(...) {
#     pb$tick()
#     .f(...)
#   }
#
#   #future::plan(future::multicore, workers = 4)
#   purrr::map_dfr(.x, f, ..., .id = .id)
# }

#######
###  Get a testset with specified characteristics of length n (default 10) from specified backbones (default all)
#######
# Possible permutations mutations:
# 1: remove last character of specific epithet
# 2: remove last character of genus
# 3: remove last character of genus & specific epithet
# 4:

get_testset <- function(n = 10,
                        mutation = 0,
                        seed = 112){
  set.seed(seed)
  df <- dplyr::sample_n(get_db(), n) %>%
    dplyr::select(c('Genus', 'Species')) %>%
    dplyr::rename(Orig.Genus = Genus, Orig.Species = Species)
  if(mutation == 0){
    return(df)
  }
  else if(mutation == 1){
    dplyr::mutate(df, Orig.Species = stringr::str_replace(Orig.Species, '.{1}$', '')) %>%
      return()
  }
  else if(mutation == 2){
    dplyr::mutate(df, Orig.Genus = stringr::str_replace(Orig.Genus, '.{1}$', '')) %>%
      return()
  }
  else if(mutation == 3){
    dplyr::mutate(df,
                  Orig.Species = stringr::str_replace(Orig.Species, '.{1}$', ''),
                  Orig.Genus = stringr::str_replace(Orig.Genus, '.{1}$', '')) %>%
      return()
  }
}
