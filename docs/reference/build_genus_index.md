# Build a Genus Index for Fast Prefiltering

**\[stable\]**

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
target <- data.frame(genus = "Opuntia", species = "ficus-indica", plant_name_id = 1)
wcvpmatch:::build_genus_index(target)
#> # A tibble: 1 × 4
#>   genus   n_records plant_name_id genus_nchar
#>   <chr>       <int> <list>              <int>
#> 1 Opuntia         1 <dbl [1]>               7
# }
```
