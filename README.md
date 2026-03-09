
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

`wcvpmatch` depends on `fozziejoin` for fuzzy matching. At the moment,
`fozziejoin` is installed from GitHub and requires a working Rust
toolchain.

### 1) Install Rust

Windows (GNU toolchain required for R + Rtools compatibility):

``` bash
rustup install stable-x86_64-pc-windows-gnu
rustup default stable-x86_64-pc-windows-gnu
rustup target add x86_64-pc-windows-gnu --toolchain stable-x86_64-pc-windows-gnu
```

Linux / macOS: install Rust with the official installer at
<https://www.rust-lang.org/tools/install>.

### 2) Install the development version of `fozziejoin` from GitHub:

``` r
pak::pak("fozzieverse/fozziejoin/fozziejoin-r")
```

Additional information is available in the official
[`fozziejoin`](https://github.com/fozzieverse/fozziejoin/tree/main/fozziejoin-r)
GitHub repository.

### 3) Install the development version of `wcvpdata` from GitHub:

``` r
pak::pak("PaulESantos/wcvpdata")
```

### 4) Install the development version of `wcvpmatch` from GitHub:

``` r
pak::pak("PaulESantos/wcvpmatch")
```

## Quick start

``` r
library(wcvpmatch)
#> ── Attaching wcvpmatch ecosystem ──────────────────────────── wcvpmatch 0.0.1 ──
#> ✔ wcvpdata 0.5.0 (wcvp_checklist_names available)

sp_names <- c(
  "Aniba heterotepala",          
  "Anthurium quipuscoae",        
  "Centropogon reflexus",        
  "Chuquiraga jonhstonii",       
  "Cyathea carolinae",           
  "Ditassa violascens",   
  "Cleistocactus fieldianus",
  "Opuntia yanganucensis",
  "Epidendrum trachydipterum",  
  "Hebeclinium hylophorbum",     
  "Jaltometa sagastegui",        # Jaltomata sagastegui
  "Lepechinia tomentosa",        
  "Lupinus cookianos",           # Lupinus cookianus
  "Oxalis hochreutinerii",       # Oxalis hochreutineri
  "Passiflora heterohelix",     
  "Peperomia arborigaudens",     # Peperromia arborigaudens
  "Piper setulosum",             
  "Pycnophyllum aristattum",     # Pycnophyllum aristatum
  "Salvia subscadens",           # Salvia subscandens
  "Stellaria macbridei",         
  "Stemodia piurenses",          # Stemodia piurensis
  "Weberbauerella brongnartioides" 
)


res <- 
  classify_spnames(sp_names) |> 
  wcvp_matching(
  prefilter_genus = TRUE,
  allow_duplicates = TRUE,
  max_dist = 2,
  method = "osa",
  output_name_style = "snake_case"
)

res |>  dim()
#> [1] 22 43

res |> 
  dplyr::select(input_name,
                matched_taxon_name,
                accepted_taxon_name, 
                taxon_status) |> 
  as.data.frame()
#>                        input_name              matched_taxon_name
#> 1              Aniba heterotepala              Aniba heterotepala
#> 2            Anthurium quipuscoae            Anthurium quipuscoae
#> 3            Centropogon reflexus            Centropogon reflexus
#> 4           Chuquiraga jonhstonii           Chuquiraga johnstonii
#> 5               Cyathea carolinae               Cyathea carolinae
#> 6              Ditassa violascens              Ditassa violascens
#> 7        Cleistocactus fieldianus        Cleistocactus fieldianus
#> 8           Opuntia yanganucensis           Opuntia yanganucensis
#> 9       Epidendrum trachydipterum       Epidendrum trachydipterum
#> 10        Hebeclinium hylophorbum         Hebeclinium hylophorbum
#> 11           Jaltometa sagastegui            Jaltomata sagastegui
#> 12           Lepechinia tomentosa            Lepechinia tomentosa
#> 13              Lupinus cookianos               Lupinus cookianus
#> 14          Oxalis hochreutinerii            Oxalis hochreutineri
#> 15         Passiflora heterohelix          Passiflora heterohelix
#> 16        Peperomia arborigaudens         Peperomia arborigaudens
#> 17                Piper setulosum                 Piper setulosum
#> 18        Pycnophyllum aristattum          Pycnophyllum aristatum
#> 19              Salvia subscadens              Salvia subscandens
#> 20            Stellaria macbridei             Stellaria macbridei
#> 21             Stemodia piurenses              Stemodia piurensis
#> 22 Weberbauerella brongnartioides Weberbauerella brongniartioides
#>                accepted_taxon_name taxon_status
#> 1               Aniba heterotepala     Accepted
#> 2             Anthurium quipuscoae     Accepted
#> 3             Centropogon reflexus     Accepted
#> 4            Chuquiraga johnstonii     Accepted
#> 5                Cyathea carolinae     Accepted
#> 6               Ditassa violascens     Accepted
#> 7           Borzicactus fieldianus      Synonym
#> 8    Austrocylindropuntia floccosa      Synonym
#> 9        Epidendrum trachydipterum     Accepted
#> 10         Hebeclinium hylophorbum     Accepted
#> 11            Jaltomata sagastegui     Accepted
#> 12            Lepechinia tomentosa     Accepted
#> 13               Lupinus cookianus     Accepted
#> 14            Oxalis hochreutineri     Accepted
#> 15          Passiflora heterohelix     Accepted
#> 16         Peperomia arborigaudens     Accepted
#> 17                 Piper setulosum     Accepted
#> 18          Pycnophyllum aristatum     Accepted
#> 19              Salvia subscandens     Accepted
#> 20             Stellaria macbridei     Accepted
#> 21              Stemodia piurensis     Accepted
#> 22 Weberbauerella brongniartioides     Accepted
```

## Matching workflow

`wcvp_matching()` follows a staged pipeline:

1.  `wcvp_direct_match()`
2.  `wcvp_genus_match()`
3.  `wcvp_fuzzy_match_genus()`
4.  `wcvp_direct_match_species_within_genus()`
5.  `wcvp_suffix_match_species_within_genus()`
6.  `wcvp_fuzzy_match_species_within_genus()`
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

## Example: FIA data and ambiguous genus ties

When input genera have tied fuzzy candidates, `wcvp_matching()` emits:

`Multiple fuzzy matches for some genera (tied distances). The first match is selected.`

Example with an FIA-like table (`Orig.Genus`, `Orig.Species`):

``` r
fia
#> # A tibble: 2,169 × 2
#>    Orig.Genus Orig.Species
#>    <chr>      <chr>       
#>  1 Abies      amabilis    
#>  2 Abies      balsamea    
#>  3 Abies      bracteata   
#>  4 Abies      concolor    
#>  5 Abies      fraseri     
#>  6 Abies      grandis     
#>  7 Abies      lasiocarpa  
#>  8 Abies      magnifica   
#>  9 Abies      procera     
#> 10 Abies      shastensis  
#> # ℹ 2,159 more rows

fia_result <- fia |>
  wcvp_matching(
    allow_duplicates = TRUE,
    max_dist = 2
  ) |>
  dplyr::select(
    input_name,
    orig_genus,
    matched_genus,
    orig_species,
    matched_species,
    taxon_status,
    accepted_taxon_name
  )
#> Warning: Multiple fuzzy matches for some genera (tied distances). The first
#> match is selected.
fia_result
#> # A tibble: 2,169 × 7
#>    input_name orig_genus matched_genus orig_species matched_species taxon_status
#>    <chr>      <chr>      <chr>         <chr>        <chr>           <chr>       
#>  1 Abies ama… Abies      Abies         amabilis     amabilis        Accepted    
#>  2 Abies bal… Abies      Abies         balsamea     balsamea        Accepted    
#>  3 Abies bra… Abies      Abies         bracteata    bracteata       Accepted    
#>  4 Abies con… Abies      Abies         concolor     concolor        Accepted    
#>  5 Abies fra… Abies      Abies         fraseri      fraseri         Accepted    
#>  6 Abies gra… Abies      Abies         grandis      grandis         Accepted    
#>  7 Abies las… Abies      Abies         lasiocarpa   lasiocarpa      Accepted    
#>  8 Abies mag… Abies      Abies         magnifica    magnifica       Accepted    
#>  9 Abies pro… Abies      Abies         procera      procera         Accepted    
#> 10 Abies sha… Abies      Abies         shastensis   shastensis      Synonym     
#> # ℹ 2,159 more rows
#> # ℹ 1 more variable: accepted_taxon_name <chr>
```

Inspect ambiguous genus cases from the result attribute:

``` r
amb_g <- attr(fia_result, "ambiguous_genus")
amb_g
#> # A tibble: 4 × 28
#> # Groups:   .row_id [2]
#>   Orig.Genus Orig.Species genus_match Input.Name           Orig.Name Author
#>   <chr>      <chr>        <lgl>       <chr>                <chr>     <chr> 
#> 1 Howeia     forsteriana  FALSE       Howeia forsteriana   <NA>      ""    
#> 2 Howeia     forsteriana  FALSE       Howeia forsteriana   <NA>      ""    
#> 3 Hyeronima  clusioides   FALSE       Hyeronima clusioides <NA>      ""    
#> 4 Hyeronima  clusioides   FALSE       Hyeronima clusioides <NA>      ""    
#> # ℹ 22 more variables: Orig.Infraspecies <chr>, Infra.Rank <chr>, Rank <dbl>,
#> #   has_cf <lgl>, has_aff <lgl>, is_sp <lgl>, is_spp <lgl>, had_hybrid <lgl>,
#> #   rank_late <lgl>, rank_missing_infra <lgl>, had_na_author <lgl>,
#> #   implied_infra <lgl>, sorter <dbl>, input_index <int>, .dedup_key <chr>,
#> #   direct_match <lgl>, Matched.Genus <chr>, Matched.Species <chr>,
#> #   Matched.Infraspecies <chr>, Matched.Infra.Rank <chr>, .row_id <int>,
#> #   fuzzy_genus_dist <dbl>
```

Re-run only those ambiguous names for focused review:

``` r
amb_g |>
  dplyr::ungroup() |>
  dplyr::select(Orig.Genus, Orig.Species) |>
  wcvp_matching(
    allow_duplicates = TRUE,
    max_dist = 2
  ) |>
  dplyr::select(
    input_name,
    orig_genus,
    matched_genus,
    orig_species,
    matched_species,
    taxon_status,
    accepted_taxon_name
  )
#> Warning: Multiple fuzzy matches for some genera (tied distances). The first
#> match is selected.
#> # A tibble: 4 × 7
#>   input_name  orig_genus matched_genus orig_species matched_species taxon_status
#>   <chr>       <chr>      <chr>         <chr>        <chr>           <chr>       
#> 1 Howeia for… Howeia     Howea         forsteriana  forsteriana     Accepted    
#> 2 Howeia for… Howeia     Howea         forsteriana  forsteriana     Accepted    
#> 3 Hyeronima … Hyeronima  Hieronyma     clusioides   clusioides      Accepted    
#> 4 Hyeronima … Hyeronima  Hieronyma     clusioides   clusioides      Accepted    
#> # ℹ 1 more variable: accepted_taxon_name <chr>
```

In this example, the ambiguous genus spellings (`Howeia`, `Hyeronima`)
are resolved to accepted genera (`Howea`, `Hieronyma`) after
species-level checks.

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

## Core public functions

- `classify_spnames()`
- `wcvp_matching()`
- `build_genus_index()`
- `prefilter_target_by_genus()`
- `wcvp_direct_match()`
- `wcvp_genus_match()`
- `wcvp_fuzzy_match_genus()`
- `wcvp_direct_match_species_within_genus()`
- `wcvp_fuzzy_match_species_within_genus()`
- `wcvp_suffix_match_species_within_genus()`

## Acknowledgement

`wcvpmatch` builds on ideas used in the
[`treemendous`](https://github.com/speckerf/treemendous) matching
workflow and extends them for WCVP-focused reconciliation, richer parser
diagnostics, and reproducible row-level traceability.
