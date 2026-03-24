# Build a Genus Index for Fast Prefiltering

**\[experimental\]**

Creates a compact genus-level index from the target backbone. The index
stores one row per genus and a list-column with candidate
`plant_name_id` values associated with each genus.

If `plant_name_id` is not present in `target_df`, a surrogate integer ID
is created to keep the index usable with custom backbones.

## Usage

``` r
build_genus_index(target_df = NULL)
```

## Arguments

- target_df:

  Optional custom target table. If `NULL`, the optional `wcvpdata`
  checklist is used when available; otherwise pass a backbone
  explicitly.

## Value

A tibble with columns:

- genus:

  Genus name (character).

- plant_name_id:

  List-column of unique IDs per genus.

- n_records:

  Number of IDs per genus.

- genus_nchar:

  Number of characters in the genus name.

## Examples

``` r
# \donttest{
library(wcvpmatch)
build_genus_index()
#> # A tibble: 39,847 × 3
#>    genus         n_records plant_name_id
#>    <chr>             <int> <list>       
#>  1 Aa                   45 <dbl [45]>   
#>  2 Aakia                 2 <dbl [2]>    
#>  3 Aalius               21 <dbl [21]>   
#>  4 Aamaealoe             1 <dbl [1]>    
#>  5 Aapaca                1 <dbl [1]>    
#>  6 Aaronsohnia           5 <dbl [5]>    
#>  7 Abacopterella         2 <dbl [2]>    
#>  8 Abacopteris          52 <dbl [52]>   
#>  9 Abacosa              13 <dbl [13]>   
#> 10 Abalemis              1 <dbl [1]>    
#> # ℹ 39,837 more rows
# }
```
