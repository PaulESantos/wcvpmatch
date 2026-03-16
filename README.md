
<!-- README.md is generated from README.Rmd. Please edit that file -->

# wcvpmatch

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![R-CMD-check](https://github.com/PaulESantos/wcvpmatch/actions/workflows/R-CMD-check.yml/badge.svg)](https://github.com/PaulESantos/wcvpmatch/actions/workflows/R-CMD-check.yml)
<!-- badges: end -->

`wcvpmatch` is an R package for scientific plant name standardization
and taxonomic reconciliation against the [World Checklist of Vascular
Plants (WCVP)](https://powo.science.kew.org/about-wcvp).

The package is inspired by the matching workflow implemented in
‘treemendous’ (especially its staged ‘matching’ logic), and extends that
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
- author metadata propagation from ‘WCVP’ (`matched_taxon_authors`,
  `accepted_taxon_authors`)
- optional standardized output names in snake_case
  (`output_name_style = "snake_case"`)

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("PaulESantos/wcvpmatch")
```

`wcvpmatch` depends on `fozziejoin` for fuzzy matching in the backend.
For that reason, a working Rust toolchain is a practical prerequisite
for a fully functional installation of `wcvpmatch`, especially when
`fozziejoin` is installed from source.

### 1) Install Rust

Install Rust on your computer first. This is required so that the
`fozziejoin` backend used by `wcvpmatch` can compile and run the fuzzy
matching components correctly. You can download the installer from
<https://rust-lang.org/tools/install/>.

On Windows, configure Rust for R + Rtools compatibility by running this
command in the R terminal:

``` bash
rustup override set stable-x86_64-pc-windows-gnu
```

### 2) Install dependencies:

``` r
pak::pak("fozziejoin")
```

Or install the development version of `fozziejoin`:

``` r
pak::pak("fozzieverse/fozziejoin/fozziejoin-r")
```

If you want the package to use the default WCVP backbone automatically,
install the companion package `wcvpdata` from `r-universe`:

``` r
install.packages(
  "wcvpdata",
  repos = c("https://paulesantos.r-universe.dev", "https://cloud.r-project.org")
)
```

After loading `wcvpmatch`, you can check whether the default backbone is
available with:

``` r
library(wcvpmatch)
wcvp_setup_info()
```

## Quick start

``` r
library(wcvpmatch)

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
#> 1               Aniba heterotepala     accepted
#> 2             Anthurium quipuscoae     accepted
#> 3             Centropogon reflexus     accepted
#> 4            Chuquiraga johnstonii     accepted
#> 5                Cyathea carolinae     accepted
#> 6               Ditassa violascens     accepted
#> 7           Borzicactus fieldianus      synonym
#> 8    Austrocylindropuntia floccosa      synonym
#> 9        Epidendrum trachydipterum     accepted
#> 10         Hebeclinium hylophorbum     accepted
#> 11            Jaltomata sagastegui     accepted
#> 12            Lepechinia tomentosa     accepted
#> 13               Lupinus cookianus     accepted
#> 14            Oxalis hochreutineri     accepted
#> 15          Passiflora heterohelix     accepted
#> 16         Peperomia arborigaudens     accepted
#> 17                 Piper setulosum     accepted
#> 18          Pycnophyllum aristatum     accepted
#> 19              Salvia subscandens     accepted
#> 20             Stellaria macbridei     accepted
#> 21              Stemodia piurensis     accepted
#> 22 Weberbauerella brongniartioides     accepted
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
#> Warning: ! Multiple fuzzy matches for some genera (tied distances).
#> ℹ The first match is selected.
fia_result
#> # A tibble: 2,169 × 7
#>    input_name orig_genus matched_genus orig_species matched_species taxon_status
#>    <chr>      <chr>      <chr>         <chr>        <chr>           <chr>       
#>  1 Abies ama… Abies      Abies         amabilis     amabilis        accepted    
#>  2 Abies bal… Abies      Abies         balsamea     balsamea        accepted    
#>  3 Abies bra… Abies      Abies         bracteata    bracteata       accepted    
#>  4 Abies con… Abies      Abies         concolor     concolor        accepted    
#>  5 Abies fra… Abies      Abies         fraseri      fraseri         accepted    
#>  6 Abies gra… Abies      Abies         grandis      grandis         accepted    
#>  7 Abies las… Abies      Abies         lasiocarpa   lasiocarpa      accepted    
#>  8 Abies mag… Abies      Abies         magnifica    magnifica       accepted    
#>  9 Abies pro… Abies      Abies         procera      procera         accepted    
#> 10 Abies sha… Abies      Abies         shastensis   shastensis      synonym     
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
#> Warning: ! Multiple fuzzy matches for some genera (tied distances).
#> ℹ The first match is selected.
#> # A tibble: 4 × 7
#>   input_name  orig_genus matched_genus orig_species matched_species taxon_status
#>   <chr>       <chr>      <chr>         <chr>        <chr>           <chr>       
#> 1 Howeia for… Howeia     Howea         forsteriana  forsteriana     accepted    
#> 2 Howeia for… Howeia     Howea         forsteriana  forsteriana     accepted    
#> 3 Hyeronima … Hyeronima  Hieronyma     clusioides   clusioides      accepted    
#> 4 Hyeronima … Hyeronima  Hieronyma     clusioides   clusioides      accepted    
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
