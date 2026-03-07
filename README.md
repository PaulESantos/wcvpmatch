
<!-- README.md is generated from README.Rmd. Please edit that file -->

# wcvpmatch

<!-- badges: start -->

[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-blue.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![CRAN
status](https://www.r-pkg.org/badges/version/wcvpmatch)](https://CRAN.R-project.org/package=wcvpmatch)
[![](http://cranlogs.r-pkg.org/badges/grand-total/wcvpmatch?color=green)](https://cran.r-project.org/package=wcvpmatch)
[![](http://cranlogs.r-pkg.org/badges/last-week/wcvpmatch?color=green)](https://cran.r-project.org/package=wcvpmatch)
[![R-CMD-check](https://github.com/PaulESantos/wcvpmatch/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/PaulESantos/wcvpmatch/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

`wcvpmatch` is an R package for scientific plant name standardization
and taxonomic reconciliation against the [World Checklist of Vascular
Plants (WCVP)](https://powo.science.kew.org/about-wcvp).

The package is inspired by the matching workflow implemented in
`treemendous` (especially its staged `matching` logic), and extends that
approach with broader functionality for robust and reproducible
taxonomic resolution workflows.

## What `wcvpmatch` adds

- robust parsing and normalization of scientific names
  (`classify_spnames()`)
- staged exact + fuzzy matching at genus/species/infraspecies levels
  (`wcvp_matching()`)
- infraspecific rank-aware reconciliation (trinomial support)
- optional genus prefiltering for performance
- duplicate-aware matching with full row traceability
  (`allow_duplicates`, `input_index`)
- taxonomic context output (`matched_taxon_name`, `accepted_taxon_name`,
  status fields)
- author metadata propagation from WCVP (`matched_taxon_authors`,
  `accepted_taxon_authors`)
- optional standardized output names in snake_case
  (`output_name_style = "snake_case"`)

## Installation

Install the development version of `wcvpmatch` from GitHub:

``` r
pak::pak("PaulESantos/wcvpmatch")
```

Install **WCVP** data source from GitHub (required to use `wcvpmatch`):

``` r
pak::pak("PaulESantos/rWCVPdata")
```

## Quick start

``` r
library(wcvpmatch)
#> ── Attaching wcvpmatch ecosystem ──────────────────────────── wcvpmatch 1.1.1 ──
#> ✔ rWCVPdata 0.8.0 (wcvp_names available)

splist <- c(
  "Praecereus euchlorus subsp. diffusus",
  "Cleistocactus fieldianus",
  "Opuntia yanganucensis",
  "Trichocereus macrogonus var. pachanoi",
  "opuntia sp."
)

parsed <- classify_spnames(splist)
#> Warning: Undetermined species indicator detected ('sp.'/'spp.'). Classified at
#> genus level only; Orig.Species set to NA for: 'opuntia sp.'

res <- wcvp_matching(
  parsed,
  prefilter_genus = TRUE,
  allow_duplicates = TRUE,
  max_dist = 1,
  method = "osa"
)
#> Warning: 1 genus-only row(s) detected (Rank==1 / sp./spp.). These will not
#> participate in species-level strict matching.

res 
#> # A tibble: 5 × 43
#>   input_index input_name            orig_name orig_genus orig_species infra_rank
#>         <int> <chr>                 <chr>     <chr>      <chr>        <chr>     
#> 1           1 Praecereus euchlorus… Praecere… Praecereus euchlorus    subsp.    
#> 2           2 Cleistocactus fieldi… Cleistoc… Cleistoca… fieldianus   <NA>      
#> 3           3 Opuntia yanganucensis Opuntia … Opuntia    yanganucens… <NA>      
#> 4           4 Trichocereus macrogo… Trichoce… Trichocer… macrogonus   var.      
#> 5           5 opuntia sp.           Opuntia   Opuntia    <NA>         <NA>      
#> # ℹ 37 more variables: orig_infraspecies <chr>, matched_genus <chr>,
#> #   matched_species <chr>, matched_infra_rank <chr>,
#> #   matched_infraspecies <chr>, author <chr>, matched_plant_name_id <dbl>,
#> #   matched_taxon_name <chr>, matched_taxon_authors <chr>, taxon_status <chr>,
#> #   accepted_plant_name_id <dbl>, accepted_taxon_name <chr>,
#> #   accepted_taxon_authors <chr>, is_accepted_name <lgl>, matched <lgl>,
#> #   direct_match <lgl>, genus_match <lgl>, fuzzy_match_genus <lgl>, …
```

## Matching workflow

`wcvp_matching()` follows a staged pipeline:

1.  `direct_match()`
2.  `genus_match()`
3.  `fuzzy_match_genus()`
4.  `direct_match_species_within_genus()`
5.  `suffix_match_species_within_genus()`
6.  `fuzzy_match_species_within_genus()`
7.  infraspecific checks (rank + fuzzy infraspecies, when applicable)

This staged design prioritizes strict evidence first, then progressively
relaxes criteria to recover additional valid matches.

## Input requirements

`wcvp_matching()` accepts:

- parsed output from `classify_spnames()`
- or a tibble/data.frame with minimal columns `Genus` and `Species`

Optional columns:

- `Infra.Rank`, `Infraspecies` for trinomial matching
- `Input.Name` to preserve original query text
- `Author` (from parser) as contextual information in output

## Output highlights

`wcvp_matching()` returns:

- traceable rows via `input_index`
- original taxon components (`Orig.*`) and matched components
  (`Matched.*`)
- pathway flags (`direct_match`, `fuzzy_match_genus`, etc.)
- final logical indicator `matched`
- taxonomic context fields:
- `matched_plant_name_id`
- `matched_taxon_name`
- `matched_taxon_authors`
- `taxon_status`
- `accepted_plant_name_id`
- `accepted_taxon_name`
- `accepted_taxon_authors`
- `is_accepted_name`

## Performance notes

For medium/large inputs, recommended settings are:

``` r
wcvp_matching(
  parsed_df,
  allow_duplicates = TRUE
)
```

This typically reduces runtime by limiting candidate genera before
species-level matching and avoids recomputing duplicated names.

## Custom backbone (optional)

You can bypass `rWCVPdata` by passing your own `target_df`:

``` r
custom_target <- tibble::tibble(
  genus = c("Acer", "Quercus"),
  species = c("rubrum", "robur"),
  infraspecific_rank = NA_character_,
  infraspecies = NA_character_
)

res <- wcvp_matching(parsed, target_df = custom_target)
```

## Core public functions

- `classify_spnames()`
- `wcvp_matching()`
- `build_genus_index()`
- `prefilter_target_by_genus()`
- `direct_match()`
- `genus_match()`
- `fuzzy_match_genus()`
- `direct_match_species_within_genus()`
- `fuzzy_match_species_within_genus()`
- `suffix_match_species_within_genus()`

## Acknowledgement

`wcvpmatch` builds on ideas used in the
[`treemendous`](https://github.com/speckerf/treemendous) matching
workflow and extends them for WCVP-focused reconciliation, richer parser
diagnostics, and reproducible row-level traceability.
