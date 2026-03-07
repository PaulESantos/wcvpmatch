#' Classify Scientific Plant Names into Taxonomic Components
#'
#' @description
#' Parse and classify scientific plant names into taxonomic components:
#' genus, specific epithet, infraspecific rank, infraspecific epithet, and author.
#'
#' Output is aligned to a backbone convention:
#' \itemize{
#'   \item `Orig.Genus` in Title Case (first letter uppercase, rest lowercase).
#'   \item `Orig.Species` and `Orig.Infraspecies` epithets in lowercase.
#'   \item `Infra.Rank` in lowercase (`subsp.`, `var.`, `subvar.`, `f.`, `subf.`).
#'   \item `Author` is recovered from the input and preserved in its original
#'   casing/punctuation (no forced uppercasing).
#'   \item `Orig.Name` is reconstructed as: genus + species + (rank + infra) + author.
#' }
#'
#' Robustness rules:
#' \itemize{
#'   \item `cf.` / `aff.` are removed from parsing but preserved as flags (`has_cf`, `has_aff`).
#'   \item Hybrid markers (`x`/`\u00D7`) as standalone tokens are removed with `had_hybrid = TRUE`.
#'   \item `sp.` / `spp.` triggers genus-only classification (`Rank = 1`, `Orig.Species = NA`)
#'     and sets `is_sp`/`is_spp`.
#'   \item If an infraspecific rank is present but the infraspecific epithet is missing,
#'     sets `rank_missing_infra = TRUE` and keeps `Infra.Rank` while `Orig.Infraspecies = NA`.
#'   \item If rank appears "late" (after author-like tokens), parsing is best-effort and
#'     `rank_late = TRUE`.
#'   \item If there is no explicit rank and a third token exists, the function can infer an
#'     unranked infraspecific epithet when the third token looks epithet-like (all lowercase),
#'     and does not look like the start of an author. In that case `implied_infra = TRUE`,
#'     `Orig.Infraspecies` is filled, `Infra.Rank = NA`, and `Rank = 3`.
#' }
#'
#' @param splist Character vector. Scientific plant names.
#'
#' @return A tibble with one row per input name and standardized columns/flags:
#' \describe{
#'   \item{sorter}{Numeric index of original order.}
#'   \item{Input.Name}{Original input string as provided by user.}
#'   \item{Orig.Name}{Reconstructed standardized name aligned to backbone + original-cased author.}
#'   \item{Orig.Genus}{Genus in Title Case.}
#'   \item{Orig.Species}{Specific epithet in lowercase, or `NA` for genus-only (`sp./spp.`).}
#'   \item{Author}{Recovered author string (original casing/punctuation) or `""`.}
#'   \item{Orig.Infraspecies}{Infraspecific epithet in lowercase (ranked or implied), or `NA`.}
#'   \item{Infra.Rank}{Infraspecific rank in lowercase (`subsp.`, `var.`, `subvar.`, `f.`, `subf.`), or `NA`.}
#'   \item{Rank}{Numeric level: `1` genus-only, `2` genus+species, `3` includes infraspecific epithet.}
#'   \item{has_cf,has_aff,is_sp,is_spp,had_hybrid,rank_late,rank_missing_infra,had_na_author,implied_infra}{Logical flags.}
#' }
#'
#' @examples
#' classify_spnames(c("Opuntia sp.", "Rosa canina subsp. coriifolia (Fr.) Leffler"))
#' classify_spnames(c("Cydonia japonica tricolor")) # implied unranked infra epithet
#'
#' @importFrom tibble as_tibble
#' @export
classify_spnames <- function(splist) {
  if (!is.character(splist)) {
    stop("`splist` must be a character vector.", call. = FALSE)
  }
  if (length(splist) == 0) {
    stop("`splist` must contain at least one name.", call. = FALSE)
  }

  std <- .names_standardize2(splist)

  n <- length(splist)

  # Outputs (aligned to backbone)
  Orig.Genus <- rep(NA_character_, n)        # Title Case
  Orig.Species <- rep(NA_character_, n)      # lowercase
  Author <- rep("", n)                       # preserved casing from input
  Orig.Infraspecies <- rep(NA_character_, n) # lowercase
  Infra.Rank <- rep(NA_character_, n)        # lowercase
  Rank <- rep(NA_real_, n)                   # numeric
  Orig.Name <- rep(NA_character_, n)         # reconstructed standardized name + author

  # flags from standardizer
  has_cf <- std$has_cf
  has_aff <- std$has_aff
  is_sp <- std$is_sp
  is_spp <- std$is_spp
  had_hybrid <- std$had_hybrid
  had_na_author <- std$had_na_author

  # flags computed during parsing
  rank_late <- rep(FALSE, n)
  rank_missing_infra <- rep(FALSE, n)
  implied_infra <- rep(FALSE, n)

  ranks_upper <- c("SUBSP.", "VAR.", "SUBVAR.", "F.", "SUBF.")

  for (i in seq_len(n)) {
    tok_parse <- std$tokens_parse[[i]]   # uppercase + canonical ranks
    tok_auth  <- std$tokens_author[[i]] # original casing for author recovery
    nt <- length(tok_parse)

    if (length(tok_auth) != nt) {
      stop("Internal error: token streams are not aligned. Please report.", call. = FALSE)
    }

    # empty after cleaning
    if (nt == 0L) {
      Orig.Name[i] <- ""
      Author[i] <- ""
      Rank[i] <- NA_real_
      next
    }

    # sp./spp. => genus-only
    if (is_sp[i] || is_spp[i]) {
      Orig.Genus[i] <- .to_title(tok_parse[1])
      Orig.Species[i] <- NA_character_
      Orig.Infraspecies[i] <- NA_character_
      Infra.Rank[i] <- NA_character_
      Rank[i] <- 1
      Author[i] <- ""
      Orig.Name[i] <- Orig.Genus[i]
      next
    }

    # genus/species baseline
    Orig.Genus[i] <- .to_title(tok_parse[1])
    Orig.Species[i] <- if (nt >= 2) tolower(tok_parse[2]) else NA_character_

    # genus only provided
    if (nt < 2) {
      Rank[i] <- 1
      Author[i] <- ""
      Orig.Name[i] <- Orig.Genus[i]
      next
    }

    # Identify explicit infraspecific ranks
    idx_rank <- which(tok_parse %in% ranks_upper)

    # ---- No explicit rank case: decide author vs implied (unranked) infra epithet ----
    if (length(idx_rank) == 0) {
      if (nt >= 3) {
        t3 <- tok_auth[3]
        t4 <- if (nt >= 4) tok_auth[4] else NULL

        author_like <- .looks_like_author_start(t3, next_tok = t4)

        # If token 3 is epithet-like and not author-like => implied unranked infra epithet
        if (!author_like && .is_epithet_like(t3)) {
          implied_infra[i] <- TRUE
          Orig.Infraspecies[i] <- tolower(t3)
          Infra.Rank[i] <- NA_character_
          Rank[i] <- 3

          Author[i] <- if (nt >= 4) paste(tok_auth[4:nt], collapse = " ") else ""
          parts <- c(Orig.Genus[i], Orig.Species[i], Orig.Infraspecies[i])
          if (nzchar(Author[i])) parts <- c(parts, Author[i])
          Orig.Name[i] <- paste(parts, collapse = " ")
          next
        }
      }

      # Default: treat remainder as author
      Author[i] <- if (nt >= 3) paste(tok_auth[3:nt], collapse = " ") else ""
      Rank[i] <- 2
      parts <- c(Orig.Genus[i], Orig.Species[i])
      if (nzchar(Author[i])) parts <- c(parts, Author[i])
      Orig.Name[i] <- paste(parts, collapse = " ")
      next
    }

    # ---- Explicit rank case ----
    rpos <- idx_rank[1]
    rank_late[i] <- rpos > 3

    Infra.Rank[i] <- .rank_upper_to_lower(tok_parse[rpos])

    if (rpos == nt) {
      # rank without infra epithet
      rank_missing_infra[i] <- TRUE
      Orig.Infraspecies[i] <- NA_character_
    } else {
      Orig.Infraspecies[i] <- tolower(tok_parse[rpos + 1])
    }

    # Exclude tokens used for taxon parts; remaining tokens compose Author (original casing)
    keep <- rep(TRUE, nt)
    keep[1] <- FALSE # genus
    keep[2] <- FALSE # species
    keep[rpos] <- FALSE # rank
    if (rpos + 1 <= nt) keep[rpos + 1] <- FALSE # infra epithet (if any)

    Author[i] <- paste(tok_auth[keep], collapse = " ")
    if (Author[i] == "") Author[i] <- ""

    Rank[i] <- if (!is.na(Orig.Infraspecies[i])) 3 else 2

    parts <- c(Orig.Genus[i], Orig.Species[i])
    if (!is.na(Infra.Rank[i]) && !is.na(Orig.Infraspecies[i])) {
      parts <- c(parts, Infra.Rank[i], Orig.Infraspecies[i])
    } else if (!is.na(Infra.Rank[i]) && is.na(Orig.Infraspecies[i])) {
      parts <- c(parts, Infra.Rank[i])
    }
    if (nzchar(Author[i])) parts <- c(parts, Author[i])
    Orig.Name[i] <- paste(parts, collapse = " ")
  }

  # ---- aggregated warnings ----
  if (any(had_hybrid)) {
    .warn_list("Hybrid marker removed from", splist[had_hybrid])
  }
  if (any(is_sp | is_spp)) {
    .warn_list(
      "Undetermined species indicator detected ('sp.'/'spp.'). Classified at genus level only; Orig.Species set to NA for",
      splist[is_sp | is_spp]
    )
  }
  if (any(rank_late)) {
    .warn_list(
      "Infraspecific rank detected late; parsed infraspecific epithet as token following the rank (best effort) for",
      Orig.Name[rank_late]
    )
  }
  if (any(rank_missing_infra)) {
    .warn_list(
      "Infraspecific rank detected without a following infraspecific epithet. Set Orig.Infraspecies = NA (rank_missing_infra = TRUE) for",
      Orig.Name[rank_missing_infra]
    )
  }
  if (any(had_na_author)) {
    .warn_list(
      "Trailing literal 'NA' detected (likely from missing authors concatenated with paste/paste0). Removed for",
      splist[had_na_author]
    )
  }

  out <- tibble::as_tibble(
    data.frame(
      sorter = as.numeric(seq_len(n)),
      Input.Name = splist,
      Orig.Name = Orig.Name,
      Orig.Genus = Orig.Genus,
      Orig.Species = Orig.Species,
      Author = Author,
      Orig.Infraspecies = Orig.Infraspecies,
      Infra.Rank = Infra.Rank,
      Rank = Rank,
      has_cf = has_cf,
      has_aff = has_aff,
      is_sp = is_sp,
      is_spp = is_spp,
      had_hybrid = had_hybrid,
      rank_late = rank_late,
      rank_missing_infra = rank_missing_infra,
      had_na_author = had_na_author,
      implied_infra = implied_infra,
      stringsAsFactors = FALSE
    )
  )

  # Final whitespace cleanup as a safety net on all scientific-name components.
  out <- dplyr::mutate(
    out,
    dplyr::across(
      dplyr::any_of(c("Orig.Name", "Orig.Genus", "Orig.Species", "Author", "Orig.Infraspecies", "Infra.Rank")),
      ~ dplyr::if_else(is.na(.x), .x, stringr::str_trim(.x))
    )
  )

  out
}

# ---- internal helpers ----

# Standardize and return TWO aligned token streams:
# - tokens_parse: uppercase for robust parsing + canonical rank tokens
# - tokens_author: original casing preserved for author reconstruction
.names_standardize2 <- function(x) {
  has_cf  <- grepl("\\bCF\\.?\\b",  x, ignore.case = TRUE, perl = TRUE)
  has_aff <- grepl("\\bAFF\\.?\\b", x, ignore.case = TRUE, perl = TRUE)
  is_sp   <- grepl("\\bSP\\.?\\b",  x, ignore.case = TRUE, perl = TRUE)
  is_spp  <- grepl("\\bSPP\\.?\\b", x, ignore.case = TRUE, perl = TRUE)

  # normalize whitespace/underscores but DO NOT change casing globally
  s <- gsub("_", " ", x, fixed = TRUE)
  s <- trimws(s)
  s <- gsub("[[:space:]]+", " ", s)
  # strip leading non-letters (keeps original case)
  s <- sub("^[^A-Za-z]+", "", s)

  raw_tokens <- strsplit(s, " ", fixed = TRUE)
  raw_tokens <- lapply(raw_tokens, function(tt) tt[tt != ""])

  mult_sign <- "\u00D7" # multiplication sign
  had_hybrid <- logical(length(raw_tokens))
  had_na_author <- logical(length(raw_tokens))

  # tokens to drop (qualifiers) - case-insensitive
  drop_re <- "^(CF\\.?|AFF\\.?)$"

  normalize_rank_lower <- function(tok) {
    t <- tolower(tok)
    t_nodot <- sub("\\.+$", "", t)

    if (t_nodot %in% c("ssp", "subsp"))  return("subsp.")
    if (t_nodot %in% c("var"))          return("var.")
    if (t_nodot %in% c("subvar"))       return("subvar.")
    if (t_nodot %in% c("f", "fo", "form", "forma")) return("f.")
    if (t_nodot %in% c("subf"))         return("subf.")
    tok
  }

  tokens_author <- vector("list", length(raw_tokens))
  tokens_parse  <- vector("list", length(raw_tokens))

  for (i in seq_along(raw_tokens)) {
    tt <- raw_tokens[[i]]
    if (length(tt) == 0L) {
      tokens_author[[i]] <- character(0)
      tokens_parse[[i]]  <- character(0)
      next
    }

    # drop cf/aff tokens
    keep1 <- !grepl(drop_re, tt, ignore.case = TRUE, perl = TRUE)
    tt <- tt[keep1]

    # remove hybrid tokens (standalone) "x" or multiplication sign
    before_len <- length(tt)
    tt <- tt[!(tt %in% c(mult_sign))]
    tt <- tt[!grepl("^x$", tt, ignore.case = TRUE)]
    had_hybrid[i] <- length(tt) != before_len

    # remove trailing literal NA token (common paste artifact)
    if (length(tt) > 0 && identical(tt[length(tt)], "NA")) {
      had_na_author[i] <- TRUE
      tt <- tt[-length(tt)]
    }

    # normalize rank tokens to canonical LOWER (does not harm author because rank tokens are excluded later)
    tt_norm <- if (length(tt) > 0) vapply(tt, normalize_rank_lower, FUN.VALUE = character(1), USE.NAMES = FALSE) else tt

    # Author stream keeps original casing except rank tokens canonicalized (fine)
    tokens_author[[i]] <- tt_norm

    # Parse stream uppercases everything
    tokens_parse[[i]] <- toupper(tt_norm)
  }

  list(
    tokens_author = tokens_author,
    tokens_parse = tokens_parse,
    has_cf = as.logical(has_cf),
    has_aff = as.logical(has_aff),
    is_sp = as.logical(is_sp),
    is_spp = as.logical(is_spp),
    had_hybrid = as.logical(had_hybrid),
    had_na_author = as.logical(had_na_author)
  )
}

.warn_list <- function(prefix, items, max_show = 15) {
  items <- unique(items)
  n <- length(items)
  if (n == 0) return(invisible(NULL))

  show <- items[seq_len(min(n, max_show))]
  msg <- paste0(
    prefix, ": ",
    paste(sprintf("'%s'", show), collapse = ", "),
    if (n > max_show) paste0(" ... (+", n - max_show, " more)") else ""
  )
  warning(msg, immediate. = TRUE, call. = FALSE)
  invisible(NULL)
}

.to_title <- function(x) {
  x <- tolower(x)
  if (is.na(x) || !nzchar(x)) return(NA_character_)
  paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x)))
}

.rank_upper_to_lower <- function(x) {
  if (is.na(x)) return(NA_character_)
  switch(
    x,
    "SUBSP."  = "subsp.",
    "VAR."    = "var.",
    "SUBVAR." = "subvar.",
    "F."      = "f.",
    "SUBF."   = "subf.",
    NA_character_
  )
}

# ---- NEW helpers for implied infra logic ----

.is_epithet_like <- function(tok) {
  # token must be all lowercase letters (and hyphen) after lowering
  t <- tolower(tok)
  grepl("^[a-z-]+$", t)
}

.looks_like_author_start <- function(tok, next_tok = NULL) {
  if (is.na(tok) || !nzchar(tok)) return(FALSE)

  # basionym starts
  if (substr(tok, 1, 1) == "(") return(TRUE)

  # uppercase starts often indicate author
  if (grepl("^[A-Z]", tok)) return(TRUE)

  # punctuation/connectors typical in authors
  if (grepl("[.&]", tok)) return(TRUE)
  if (grepl("\\.", tok)) return(TRUE)
  if (grepl("\\b(ex|et|in)\\b", tok, ignore.case = TRUE)) return(TRUE)

  # lowercase particles: only if followed by an uppercase token
  particle <- tolower(tok)
  is_particle <- particle %in% c("de","del","da","den","der","van","von","du","la","le") || grepl("^d['\\u2019]", particle)
  if (is_particle && !is.null(next_tok) && grepl("^[A-Z]", next_tok)) return(TRUE)

  FALSE
}


# #' Classify Scientific Plant Names into Taxonomic Components
# #'
# #' @description
# #' Parse and classify scientific plant names into taxonomic components:
# #' genus, specific epithet, infraspecific rank, infraspecific epithet, and author.
# #'
# #' Output is aligned to a backbone convention:
# #' \itemize{
# #'   \item Genus in Title Case.
# #'   \item Species and infraspecies epithets in lowercase.
# #'   \item Infraspecific rank in lowercase (subsp., var., subvar., f., subf.).
# #'   \item Author is recovered from the input and preserved in its original casing/punctuation.
# #'   \item Orig.Name is reconstructed as: genus + species + (rank + infra) + author.
# #' }
# #'
# #' @param splist Character vector. Scientific plant names.
# #'
# #' @return A tibble with one row per input name and standardized columns/flags.
# #' @importFrom tibble as_tibble
# #' @export
# classify_spnames <- function(splist) {
#   if (!is.character(splist)) {
#     stop("`splist` must be a character vector.", call. = FALSE)
#   }
#   if (length(splist) == 0) {
#     stop("`splist` must contain at least one name.", call. = FALSE)
#   }
#
#   std <- .names_standardize2(splist)
#
#   n <- length(splist)
#
#   # Outputs (aligned to backbone)
#   Orig.Genus <- rep(NA_character_, n)        # Title Case
#   Orig.Species <- rep(NA_character_, n)      # lowercase
#   Author <- rep("", n)                       # preserved casing from input
#   Orig.Infraspecies <- rep(NA_character_, n) # lowercase
#   Infra.Rank <- rep(NA_character_, n)        # lowercase
#   Rank <- rep(NA_real_, n)                   # numeric
#   Orig.Name <- rep(NA_character_, n)         # reconstructed standardized name + author (original casing)
#
#   # flags
#   has_cf <- std$has_cf
#   has_aff <- std$has_aff
#   is_sp <- std$is_sp
#   is_spp <- std$is_spp
#   had_hybrid <- std$had_hybrid
#   had_na_author <- std$had_na_author
#   rank_late <- rep(FALSE, n)
#   rank_missing_infra <- rep(FALSE, n)
#
#   ranks_upper <- c("SUBSP.", "VAR.", "SUBVAR.", "F.", "SUBF.")
#
#   for (i in seq_len(n)) {
#     tok_parse <- std$tokens_parse[[i]]   # uppercase + canonical ranks
#     tok_auth  <- std$tokens_author[[i]] # original casing for author recovery
#     nt <- length(tok_parse)
#
#     # Guard: keep aligned vectors
#     # (They should always be aligned by construction)
#     if (length(tok_auth) != nt) {
#       stop("Internal error: token streams are not aligned. Please report.", call. = FALSE)
#     }
#
#     # sp./spp. => genus-only
#     if (is_sp[i] || is_spp[i]) {
#       Orig.Genus[i] <- if (nt >= 1) .to_title(tok_parse[1]) else NA_character_
#       Orig.Species[i] <- NA_character_
#       Orig.Infraspecies[i] <- NA_character_
#       Infra.Rank[i] <- NA_character_
#       Rank[i] <- 1
#
#       # Author: for sp./spp. we treat everything beyond genus as not a valid author payload
#       Author[i] <- ""
#
#       # Orig.Name: genus only (backbone alignment)
#       Orig.Name[i] <- if (!is.na(Orig.Genus[i])) Orig.Genus[i] else ""
#       next
#     }
#
#     # genus/species baseline
#     Orig.Genus[i] <- if (nt >= 1) .to_title(tok_parse[1]) else NA_character_
#     Orig.Species[i] <- if (nt >= 2) tolower(tok_parse[2]) else NA_character_
#
#     if (nt < 2) {
#       Rank[i] <- if (!is.na(Orig.Genus[i])) 1 else NA_real_
#       Author[i] <- ""
#       Orig.Name[i] <- if (!is.na(Orig.Genus[i])) Orig.Genus[i] else ""
#       next
#     }
#
#     idx_rank <- which(tok_parse %in% ranks_upper)
#
#     if (length(idx_rank) == 0) {
#       # Author = tokens 3..end using ORIGINAL casing stream
#       Author[i] <- if (nt >= 3) paste(tok_auth[3:nt], collapse = " ") else ""
#       Rank[i] <- 2
#
#       # Orig.Name: genus + species + author
#       parts <- c(Orig.Genus[i], Orig.Species[i])
#       if (nzchar(Author[i])) parts <- c(parts, Author[i])
#       Orig.Name[i] <- paste(parts, collapse = " ")
#       next
#     }
#
#     rpos <- idx_rank[1]
#     rank_late[i] <- rpos > 3
#
#     Infra.Rank[i] <- .rank_upper_to_lower(tok_parse[rpos])
#
#     if (rpos == nt) {
#       rank_missing_infra[i] <- TRUE
#       Orig.Infraspecies[i] <- NA_character_
#     } else {
#       Orig.Infraspecies[i] <- tolower(tok_parse[rpos + 1])
#     }
#
#     # Author: remaining tokens excluding genus, species, rank, infra epithet
#     keep <- rep(TRUE, nt)
#     keep[1] <- FALSE
#     keep[2] <- FALSE
#     keep[rpos] <- FALSE
#     if (rpos + 1 <= nt) keep[rpos + 1] <- FALSE
#
#     # Recover author from ORIGINAL casing token stream
#     Author[i] <- paste(tok_auth[keep], collapse = " ")
#     if (Author[i] == "") Author[i] <- ""
#
#     Rank[i] <- if (!is.na(Orig.Infraspecies[i])) 3 else 2
#
#     # Orig.Name reconstruction: genus species [rank infra]? [author] (author preserved)
#     parts <- c(Orig.Genus[i], Orig.Species[i])
#
#     if (!is.na(Infra.Rank[i]) && !is.na(Orig.Infraspecies[i])) {
#       parts <- c(parts, Infra.Rank[i], Orig.Infraspecies[i])
#     } else if (!is.na(Infra.Rank[i]) && is.na(Orig.Infraspecies[i])) {
#       parts <- c(parts, Infra.Rank[i])
#     }
#
#     if (nzchar(Author[i])) parts <- c(parts, Author[i])
#     Orig.Name[i] <- paste(parts, collapse = " ")
#   }
#
#   # ---- aggregated warnings ----
#   if (any(had_hybrid)) {
#     .warn_list("Hybrid marker removed from", splist[had_hybrid])
#   }
#   if (any(is_sp | is_spp)) {
#     .warn_list(
#       "Undetermined species indicator detected ('sp.'/'spp.'). Classified at genus level only; Orig.Species set to NA for",
#       splist[is_sp | is_spp]
#     )
#   }
#   if (any(rank_late)) {
#     .warn_list(
#       "Infraspecific rank detected after author-like tokens; parsed infraspecific epithet as the token following the rank (best effort) for",
#       Orig.Name[rank_late]
#     )
#   }
#   if (any(rank_missing_infra)) {
#     .warn_list(
#       "Infraspecific rank detected without a following infraspecific epithet. Set Orig.Infraspecies = NA (rank_missing_infra = TRUE) for",
#       Orig.Name[rank_missing_infra]
#     )
#   }
#   if (any(had_na_author)) {
#     .warn_list(
#       "Trailing literal 'NA' detected (likely from missing authors concatenated with paste/paste0). Removed for",
#       splist[had_na_author]
#     )
#   }
#
#   tibble::as_tibble(
#     data.frame(
#       sorter = as.numeric(seq_len(n)),
#       Input.Name = splist,
#       Orig.Name = Orig.Name,
#       Orig.Genus = Orig.Genus,
#       Orig.Species = Orig.Species,
#       Author = Author,
#       Orig.Infraspecies = Orig.Infraspecies,
#       Infra.Rank = Infra.Rank,
#       Rank = Rank,
#       has_cf = has_cf,
#       has_aff = has_aff,
#       is_sp = is_sp,
#       is_spp = is_spp,
#       had_hybrid = had_hybrid,
#       rank_late = rank_late,
#       rank_missing_infra = rank_missing_infra,
#       had_na_author = had_na_author,
#       stringsAsFactors = FALSE
#     )
#   )
# }
#
# # ---- internal helpers ----
#
# # Standardize and return TWO aligned token streams:
# # - tokens_parse: uppercase for robust parsing + canonical rank tokens
# # - tokens_author: original casing preserved for author reconstruction
# .names_standardize2 <- function(x) {
#   has_cf  <- grepl("\\bCF\\.?\\b",  x, ignore.case = TRUE, perl = TRUE)
#   has_aff <- grepl("\\bAFF\\.?\\b", x, ignore.case = TRUE, perl = TRUE)
#   is_sp   <- grepl("\\bSP\\.?\\b",  x, ignore.case = TRUE, perl = TRUE)
#   is_spp  <- grepl("\\bSPP\\.?\\b", x, ignore.case = TRUE, perl = TRUE)
#
#   # normalize whitespace/underscores but DO NOT change casing here
#   s <- gsub("_", " ", x, fixed = TRUE)
#   s <- trimws(s)
#   s <- gsub("[[:space:]]+", " ", s)
#   # strip leading non-letters (keeps original case)
#   s <- sub("^[^A-Za-z]+", "", s)
#
#   raw_tokens <- strsplit(s, " ", fixed = TRUE)
#   raw_tokens <- lapply(raw_tokens, function(tt) tt[tt != ""])
#
#   mult_sign <- "\u00D7" # multiplication sign
#   had_hybrid <- logical(length(raw_tokens))
#   had_na_author <- logical(length(raw_tokens))
#
#   # tokens to drop (qualifiers) - case-insensitive
#   drop_re <- "^(CF\\.?|AFF\\.?)$"
#
#   # rank normalization (case-insensitive matching, canonical lower output)
#   # NOTE: canonical lower forms are what you want in output/backbone.
#   normalize_rank_lower <- function(tok) {
#     t <- tolower(tok)
#     # strip trailing dots for matching convenience
#     t_nodot <- sub("\\.+$", "", t)
#
#     if (t_nodot %in% c("ssp", "subsp"))  return("subsp.")
#     if (t_nodot %in% c("var"))          return("var.")
#     if (t_nodot %in% c("subvar"))       return("subvar.")
#     if (t_nodot %in% c("f", "fo", "form", "forma")) return("f.")
#     if (t_nodot %in% c("subf"))         return("subf.")
#     tok
#   }
#
#   tokens_author <- vector("list", length(raw_tokens))
#   tokens_parse  <- vector("list", length(raw_tokens))
#
#   for (i in seq_along(raw_tokens)) {
#     tt <- raw_tokens[[i]]
#     if (length(tt) == 0L) {
#       tokens_author[[i]] <- character(0)
#       tokens_parse[[i]]  <- character(0)
#       next
#     }
#
#     # drop cf/aff tokens (preserve other tokens)
#     keep1 <- !grepl(drop_re, tt, ignore.case = TRUE, perl = TRUE)
#     tt <- tt[keep1]
#
#     # remove hybrid tokens (standalone) "x" or multiplication sign (case-insensitive for x)
#     before_len <- length(tt)
#     tt <- tt[!(tt %in% c(mult_sign))]                # remove multiplication sign exactly
#     tt <- tt[!grepl("^x$", tt, ignore.case = TRUE)]  # remove x/X token
#     had_hybrid[i] <- length(tt) != before_len
#
#     # remove trailing literal NA token (often from paste0(name, " ", NA))
#     if (length(tt) > 0 && grepl("^NA$", tt[length(tt)], ignore.case = FALSE)) {
#       had_na_author[i] <- TRUE
#       tt <- tt[-length(tt)]
#     }
#
#     # normalize rank tokens to canonical LOWER (this does not affect author since ranks are excluded later)
#     if (length(tt) > 0) {
#       tt_norm <- vapply(tt, normalize_rank_lower, FUN.VALUE = character(1), USE.NAMES = FALSE)
#     } else {
#       tt_norm <- tt
#     }
#
#     # tokens for author preservation use original-case tokens,
#     # except that rank tokens are standardized to canonical lower (fine: ranks won't be part of Author anyway)
#     tokens_author[[i]] <- tt_norm
#
#     # tokens for parsing: uppercase everything (canonical ranks will become SUBSP./VAR./...)
#     tokens_parse[[i]] <- toupper(tt_norm)
#   }
#
#   list(
#     tokens_author = tokens_author,
#     tokens_parse = tokens_parse,
#     has_cf = as.logical(has_cf),
#     has_aff = as.logical(has_aff),
#     is_sp = as.logical(is_sp),
#     is_spp = as.logical(is_spp),
#     had_hybrid = as.logical(had_hybrid),
#     had_na_author = as.logical(had_na_author)
#   )
# }
#
# .warn_list <- function(prefix, items, max_show = 15) {
#   items <- unique(items)
#   n <- length(items)
#   if (n == 0) return(invisible(NULL))
#
#   show <- items[seq_len(min(n, max_show))]
#   msg <- paste0(
#     prefix, ": ",
#     paste(sprintf("'%s'", show), collapse = ", "),
#     if (n > max_show) paste0(" ... (+", n - max_show, " more)") else ""
#   )
#   warning(msg, immediate. = TRUE, call. = FALSE)
#   invisible(NULL)
# }
#
# .to_title <- function(x) {
#   x <- tolower(x)
#   if (is.na(x) || !nzchar(x)) return(NA_character_)
#   paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x)))
# }
#
# .rank_upper_to_lower <- function(x) {
#   if (is.na(x)) return(NA_character_)
#   switch(
#     x,
#     "SUBSP."  = "subsp.",
#     "VAR."    = "var.",
#     "SUBVAR." = "subvar.",
#     "F."      = "f.",
#     "SUBF."   = "subf.",
#     NA_character_
#   )
# }
