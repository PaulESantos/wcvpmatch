# Fuzzy Match Genus Name

**\[stable\]**

Tries to fuzzy match the genus name to the 'WCVP' table (using the
optional `wcvpdata` checklist by default when available).

## Usage

``` r
wcvp_fuzzy_match_genus(df, target_df = NULL, max_dist = 1, method = "osa")
```

## Arguments

- df:

  `tibble` containing the species binomial split into the columns
  `Orig.Genus` and `Orig.Species`.

- target_df:

  Optional custom target table. If `NULL`, the optional `wcvpdata`
  checklist is used when available; otherwise pass a backbone
  explicitly.

- max_dist:

  Maximum edit distance used for fuzzy genus matching.

- method:

  String distance method passed to `fozziejoin` (for example `"osa"`).

## Value

Returns a `tibble` with the additional logical column
`fuzzy_match_genus`, indicating whether the genus was successfully
matched (`TRUE`) or not (`FALSE`). Further, the additional column
`fuzzy_genus_dist` returns the distance for every match.

## Examples

``` r
# \donttest{
df <- data.frame(Orig.Genus = "Opuntiaa", Orig.Species = "yanganucensis")
target <- data.frame(genus = "Opuntia", species = "yanganucensis", plant_name_id = 1)
wcvpmatch:::wcvp_fuzzy_match_genus(df, target_df = target)
#> i Input was converted from <data.frame> to a <tibble>.
#>   See <https://tibble.tidyverse.org/> for more details.
#> # A tibble: 1 x 21
#>   Orig.Genus Orig.Species  fuzzy_match_genus Input.Name         Orig.Name Author
#>   <chr>      <chr>         <lgl>             <chr>              <chr>     <chr> 
#> 1 Opuntiaa   yanganucensis TRUE              Opuntiaa yanganuc~ NA        ""    
#> # i 15 more variables: Orig.Infraspecies <chr>, Infra.Rank <chr>, Rank <dbl>,
#> #   has_cf <lgl>, has_aff <lgl>, is_sp <lgl>, is_spp <lgl>, had_hybrid <lgl>,
#> #   rank_late <lgl>, rank_missing_infra <lgl>, had_na_author <lgl>,
#> #   implied_infra <lgl>, sorter <dbl>, fuzzy_genus_dist <dbl>,
#> #   Matched.Genus <chr>
# }
```
