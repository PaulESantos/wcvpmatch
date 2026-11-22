# Caches are tied to column contents, not inherited source/row-count markers.
# Subsetting or editing a prepared target invalidates its shared environment.
.target_cache_valid <- function(target, cache) {
  is.environment(cache) && identical(lapply(target, identity), cache$columns)
}
.cache_target <- function(target) {
  cache <- attr(target, "wcvpmatch_indexes", exact = TRUE)
  if (!.target_cache_valid(target, cache)) {
    cache <- new.env(parent = emptyenv())
    cache$columns <- lapply(target, identity)
    attr(target, "wcvpmatch_indexes") <- cache
  }
  target
}
.target_cached <- function(target, key, expr) {
  cache <- attr(target, "wcvpmatch_indexes", exact = TRUE)
  if (!.target_cache_valid(target, cache)) return(force(expr))
  if (exists(key, envir = cache, inherits = FALSE)) return(cache[[key]])
  value <- force(expr)
  cache[[key]] <- value
  value
}
.infra_candidate_keys <- function(target) {
  .target_cached(target, "infra", {
    target %>%
      dplyr::select(genus, species, infraspecies, infraspecific_rank) %>%
      dplyr::rename(db_rank = infraspecific_rank) %>%
      dplyr::mutate(db_rank = .rank_to_upper(db_rank)) %>%
      tidyr::drop_na(genus, species, infraspecies) %>%
      dplyr::mutate(.db_rank_key = dplyr::coalesce(db_rank, "__NA__")) %>%
      dplyr::distinct()
  })
}

# Return only nearest pairs, preserving candidate order and all ties. Each
# distance call is bounded, even when one query has a very large candidate set.
.nearest_pairs <- function(query, candidate, query_group, candidate_group,
                           method, max_dist, short_rule = FALSE,
                           block_size = 50000L) {
  stopifnot(length(block_size) == 1L, is.finite(block_size), block_size >= 1)
  groups <- split(seq_along(candidate), factor(candidate_group,
    levels = unique(candidate_group)))
  group_id <- match(query_group, names(groups))
  qlen <- nchar(query)
  clen <- nchar(candidate)
  qbytes <- nchar(query, type = "bytes")
  cbytes <- nchar(candidate, type = "bytes")
  edit <- .is_edit_distance_method(method)
  best <- rep(Inf, length(query))
  winners <- vector("list", length(query))
  distances <- vector("list", length(query))
  buffer_q <- integer(block_size)
  buffer_c <- integer(block_size)
  used <- 0L
  flush <- function() {
    if (!used) return(invisible(NULL))
    q <- buffer_q[seq_len(used)]
    candidate_rows <- buffer_c[seq_len(used)]
    d <- .pairwise_string_distance(query[q], candidate[candidate_rows], method)
    ok <- which(!is.na(d) & d <= max_dist & d <= best[q])
    for (positions in split(ok, q[ok])) {
      i <- q[positions[1L]]
      minimum <- min(d[positions])
      if (minimum < best[i]) {
        winners[[i]] <<- integer()
        distances[[i]] <<- numeric()
        best[i] <<- minimum
      }
      tied <- positions[d[positions] == best[i]]
      winners[[i]] <<- c(winners[[i]], candidate_rows[tied])
      distances[[i]] <<- c(distances[[i]], d[tied])
    }
    used <<- 0L
    invisible(NULL)
  }
  # Reuse admissible candidate rows for queries with the same group/lengths.
  eligible <- new.env(hash = TRUE, parent = emptyenv())
  for (i in seq_along(query)) {
    if (is.na(query[i]) || is.na(group_id[i])) next
    key <- paste(group_id[i], if (edit) qbytes[i] else "all",
      if (short_rule && qlen[i] <= 7L) qlen[i] else "all", sep = ":")
    ids <- eligible[[key]]
    if (is.null(ids)) {
      ids <- groups[[group_id[i]]]
      keep <- !is.na(candidate[ids])
      if (short_rule && qlen[i] <= 7L) keep <- keep & clen[ids] == qlen[i]
      if (edit) {
        # Distances use bytes; character lengths are unsafe for Unicode pruning.
        bound <- if (tolower(method) == "hamming") 0 else max_dist
        keep <- keep & abs(cbytes[ids] - qbytes[i]) <= bound
      }
      ids <- ids[which(keep)]
      eligible[[key]] <- ids
    }
    start <- 1L
    while (start <= length(ids)) {
      count <- min(length(ids) - start + 1L, block_size - used)
      slots <- seq.int(used + 1L, used + count)
      buffer_q[slots] <- i
      buffer_c[slots] <- ids[seq.int(start, start + count - 1L)]
      used <- used + count
      start <- start + count
      if (used == block_size) flush()
    }
  }
  flush()
  tibble::tibble(
    query = rep.int(seq_along(query), lengths(winners)),
    candidate = as.integer(unlist(winners, use.names = FALSE)),
    distance = as.numeric(unlist(distances, use.names = FALSE))
  )

}

# Keep at most one prefiltered default target between calls. Custom targets
# never use the global slot, even when they inherit provider attributes.
.pipeline_target <- function(target, use_default) {
  if (use_default) {
    cache <- .wcvpmatch_cache[["last_matching_indexes"]]
    if (.target_cache_valid(target, cache)) attr(target, "wcvpmatch_indexes") <- cache
  }
  target <- .cache_target(target)
  if (use_default) .wcvpmatch_cache[["last_matching_indexes"]] <- attr(target, "wcvpmatch_indexes")
  target
}

.accepted_positions <- function(ids, target) {
  if (!is.numeric(ids) || !is.numeric(target$plant_name_id) ||
      is.object(ids) || is.object(target$plant_name_id)) {
    return(match(ids, target$plant_name_id))
  }
  index <- .target_cached(target, "accepted_id_index", {
    rows <- which(!is.na(target$plant_name_id))
    rows <- rows[order(target$plant_name_id[rows], method = "radix")]
    rows <- rows[!duplicated(target$plant_name_id[rows])]
    list(ids = target$plant_name_id[rows], rows = rows)
  })
  out <- rep(NA_integer_, length(ids))
  if (anyNA(ids)) out[is.na(ids)] <- match(ids[is.na(ids)], target$plant_name_id)
  if (!length(index$ids)) return(out)
  pos <- findInterval(ids, index$ids)
  valid <- which(!is.na(pos) & pos > 0L)
  valid <- valid[index$ids[pos[valid]] == ids[valid]]
  out[valid] <- index$rows[pos[valid]]
  out
}
