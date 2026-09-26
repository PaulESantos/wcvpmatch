# tests/testthat/test-classify_spnames.R

test_that("output schema matches backbone-aligned standards", {
  x <- c("Opuntia sp.", "Trichocereus macrogonus var. pachanoi")
  res <- suppressWarnings(classify_spnames(x))

  # Required columns and order (at least presence; order can be enforced if you want)
  required <- c(
    "sorter","Input.Name","Orig.Name","Orig.Genus","Orig.Species","Author",
    "Orig.Infraspecies","Infra.Rank","Rank",
    "has_cf","has_aff","is_sp","is_spp","had_hybrid","rank_late","rank_missing_infra","had_na_author"
  )
  expect_true(all(required %in% names(res)))

  # Types
  expect_true(is.numeric(res$sorter))
  expect_true(is.numeric(res$Rank))
  flags <- c("has_cf","has_aff","is_sp","is_spp","had_hybrid","rank_late","rank_missing_infra","had_na_author")
  for (f in flags) expect_true(is.logical(res[[f]]))

  # Casing rules (genus Title Case; epithets/rank lower)
  expect_equal(res$Orig.Genus[2], "Trichocereus")
  expect_equal(res$Orig.Species[2], "macrogonus")
  expect_equal(res$Infra.Rank[2], "var.")
  expect_equal(res$Orig.Infraspecies[2], "pachanoi")
})

test_that("names without authors: infra rank/epithet are not absorbed into Author", {
  splist <- c(
    "Praecereus euchlorus subsp. diffusus",
    "Trichocereus macrogonus var. pachanoi",
    "Cleistocactus fieldianus"
  )
  res <- suppressWarnings(classify_spnames(splist))

  # subsp
  expect_equal(res$Orig.Genus[1], "Praecereus")
  expect_equal(res$Orig.Species[1], "euchlorus")
  expect_equal(res$Infra.Rank[1], "subsp.")
  expect_equal(res$Orig.Infraspecies[1], "diffusus")
  expect_equal(res$Author[1], "")
  expect_equal(res$Rank[1], 3)
  expect_equal(res$Orig.Name[1], "Praecereus euchlorus subsp. diffusus")

  # var
  expect_equal(res$Orig.Genus[2], "Trichocereus")
  expect_equal(res$Orig.Species[2], "macrogonus")
  expect_equal(res$Infra.Rank[2], "var.")
  expect_equal(res$Orig.Infraspecies[2], "pachanoi")
  expect_equal(res$Author[2], "")
  expect_equal(res$Rank[2], 3)
  expect_equal(res$Orig.Name[2], "Trichocereus macrogonus var. pachanoi")

  # species only
  expect_equal(res$Orig.Genus[3], "Cleistocactus")
  expect_equal(res$Orig.Species[3], "fieldianus")
  expect_true(is.na(res$Infra.Rank[3]))
  expect_true(is.na(res$Orig.Infraspecies[3]))
  expect_equal(res$Author[3], "")
  expect_equal(res$Rank[3], 2)
  expect_equal(res$Orig.Name[3], "Cleistocactus fieldianus")
})

test_that("sp. yields genus-only classification, warns, and preserves casing standards", {
  splist <- c("opuntia sp.")

  expect_warning(
    res <- classify_spnames(splist),
    "Undetermined species indicator detected",
    fixed = TRUE
  )

  expect_equal(res$Orig.Genus[1], "Opuntia")
  expect_true(is.na(res$Orig.Species[1]))
  expect_equal(res$Rank[1], 1)
  expect_true(res$is_sp[1])
  expect_false(res$is_spp[1])

  # Orig.Name should be genus only
  expect_equal(res$Orig.Name[1], "Opuntia")
})

test_that("hybrid marker is removed, flagged, and author keeps original casing", {
  splist <- c("Sinningia × cerina (Paxton) H.E.Moore")

  expect_warning(
    res <- classify_spnames(splist),
    "Hybrid marker removed from",
    fixed = TRUE
  )

  expect_true(res$had_hybrid[1])
  expect_equal(res$Orig.Genus[1], "Sinningia")
  expect_equal(res$Orig.Species[1], "cerina")
  expect_equal(res$Author[1], "(Paxton) H.E.Moore") # preserved casing from input
  expect_equal(res$Orig.Name[1], "Sinningia cerina (Paxton) H.E.Moore")

  # Ensure we did not propagate the hybrid sign into Orig.Name
  expect_false(grepl(intToUtf8(0x00D7), res$Orig.Name[1], fixed = TRUE))
})

test_that("authors are preserved exactly as in input (no forced uppercasing)", {
  splist <- c(
    "Camelina edentula (Waldst. & Kit.) Desv.",
    "Carex vulcani Hochst. ex Seub."
  )

  res <- suppressWarnings(classify_spnames(splist))

  expect_equal(res$Author[1], "(Waldst. & Kit.) Desv.")
  expect_equal(res$Orig.Name[1], "Camelina edentula (Waldst. & Kit.) Desv.")

  expect_equal(res$Author[2], "Hochst. ex Seub.")
  expect_equal(res$Orig.Name[2], "Carex vulcani Hochst. ex Seub.")
})

test_that("rank normalization is stable (no double dots) and output ranks are lowercase", {
  splist <- c(
    "Solidago caesia var. hispida Alph.Wood",
    "Rosa canina subsp. coriifolia (Fr.) Leffler",
    "Malva micans ssp pallescens (Moris) F.Conti & Bartolucci",
    "Thelymitra longifolia fo. forsteri Hatch"
  )

  res <- suppressWarnings(classify_spnames(splist))

  expect_true(all(!grepl("\\.\\.", res$Orig.Name)))

  expect_equal(res$Infra.Rank[1], "var.")
  expect_equal(res$Orig.Infraspecies[1], "hispida")

  expect_equal(res$Infra.Rank[2], "subsp.")
  expect_equal(res$Orig.Infraspecies[2], "coriifolia")

  expect_equal(res$Infra.Rank[3], "subsp.")
  expect_equal(res$Orig.Infraspecies[3], "pallescens")

  expect_equal(res$Infra.Rank[4], "f.")
  expect_equal(res$Orig.Infraspecies[4], "forsteri")
})

test_that("late rank is parsed best-effort and flagged", {
  splist <- "Physalis alkekengi (Pojark.) Grubov var. glabripes"

  expect_warning(
    res <- classify_spnames(splist),
    "Infraspecific rank detected late; parsed infraspecific epithet as token following the rank (best effort)",
    fixed = TRUE
  )

  expect_true(res$rank_late[1])
  expect_equal(res$Orig.Genus[1], "Physalis")
  expect_equal(res$Orig.Species[1], "alkekengi")
  expect_equal(res$Infra.Rank[1], "var.")
  expect_equal(res$Orig.Infraspecies[1], "glabripes")

  # Author preserved from input
  expect_equal(res$Author[1], "(Pojark.) Grubov")
  expect_equal(res$Orig.Name[1], "Physalis alkekengi var. glabripes (Pojark.) Grubov")
})

test_that("rank without infra epithet triggers warning and flag, and keeps rank in Orig.Name", {
  splist <- c("Rosa canina var.")

  expect_warning(
    res <- classify_spnames(splist),
    "rank_missing_infra",
    fixed = TRUE
  )

  expect_true(res$rank_missing_infra[1])
  expect_equal(res$Orig.Genus[1], "Rosa")
  expect_equal(res$Orig.Species[1], "canina")
  expect_equal(res$Infra.Rank[1], "var.")
  expect_true(is.na(res$Orig.Infraspecies[1]))
  expect_equal(res$Rank[1], 2)

  # Orig.Name should keep rank token for traceability
  expect_equal(res$Orig.Name[1], "Rosa canina var.")
})

test_that("trailing literal NA token is removed and flagged", {
  splist <- c("Senecio mustersii var. mustersii NA")

  expect_warning(
    res <- classify_spnames(splist),
    "Trailing literal 'NA' detected",
    fixed = TRUE
  )

  expect_true(res$had_na_author[1])
  expect_equal(res$Orig.Genus[1], "Senecio")
  expect_equal(res$Orig.Species[1], "mustersii")
  expect_equal(res$Infra.Rank[1], "var.")
  expect_equal(res$Orig.Infraspecies[1], "mustersii")

  # Author should be empty and 'NA' should not appear in Orig.Name
  expect_equal(res$Author[1], "")
  expect_false(grepl(" NA$", res$Orig.Name[1]))
})

test_that("cf./aff. are removed from parsing but flags are retained", {
  splist <- c(
    "Opuntia cf. corotilla",
    "Opuntia aff. corotilla",
    "Opuntia cf corotilla"
  )

  res <- suppressWarnings(classify_spnames(splist))

  expect_true(res$has_cf[1])
  expect_true(res$has_aff[2])
  expect_true(res$has_cf[3])

  # Should still classify genus + species (species is the epithet token after qualifier removal)
  expect_equal(res$Orig.Genus[1], "Opuntia")
  expect_equal(res$Orig.Species[1], "corotilla")
  expect_equal(res$Rank[1], 2)

  expect_equal(res$Orig.Genus[2], "Opuntia")
  expect_equal(res$Orig.Species[2], "corotilla")
  expect_equal(res$Rank[2], 2)

  expect_equal(res$Orig.Genus[3], "Opuntia")
  expect_equal(res$Orig.Species[3], "corotilla")
  expect_equal(res$Rank[3], 2)
})

test_that("leading/trailing spaces are removed from parsed components", {
  splist <- c("  Acer   rubrum   var.   alpina   ")

  res <- suppressWarnings(classify_spnames(splist))

  expect_equal(res$Orig.Genus[1], "Acer")
  expect_equal(res$Orig.Species[1], "rubrum")
  expect_equal(res$Infra.Rank[1], "var.")
  expect_equal(res$Orig.Infraspecies[1], "alpina")
  expect_equal(res$Orig.Name[1], "Acer rubrum var. alpina")
  expect_false(grepl("^\\s|\\s$", res$Orig.Name[1]))
})
