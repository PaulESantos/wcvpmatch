# Direct Match Species & Genus Binomial or Trinomial names

**\[experimental\]**

Tries to directly match Genus + Species \| Genus + Species + Rank +
Infraspecies to `WCVP data`.

## Usage

``` r
wcvp_direct_match(df, target_df = NULL)
```

## Arguments

- df:

  `tibble` containing the species binomial split into the columns
  `Orig.Genus` and `Orig.Species`.

- target_df:

  Optional custom target table. If `NULL`, the optional `wcvpdata`
  checklist is used when available; otherwise pass a backbone
  explicitly.

## Value

Returns a `tibble` with the additional logical column `direct_match`,
indicating whether the binomial was successfully matched (`TRUE`) or not
(`FALSE`). Returns original columns plus `Matched.Genus`,
`Matched.Species`, `Matched.Infra.Rank`, and `Matched.Infraspecies`.

## Examples

``` r
# \donttest{
library(wcvpmatch)
# Simple binomial match
df_parsed <- classify_spnames("Opuntia yanganucensis")
wcvp_direct_match(df_parsed)
#> # A tibble: 1 × 23
#>   sorter Input.Name   Orig.Name Orig.Genus Orig.Species Author Orig.Infraspecies
#>    <dbl> <chr>        <chr>     <chr>      <chr>        <chr>  <chr>            
#> 1      1 Opuntia yan… Opuntia … Opuntia    yanganucens… ""     NA               
#> # ℹ 16 more variables: Infra.Rank <chr>, Rank <dbl>, has_cf <lgl>,
#> #   has_aff <lgl>, is_sp <lgl>, is_spp <lgl>, had_hybrid <lgl>,
#> #   rank_late <lgl>, rank_missing_infra <lgl>, had_na_author <lgl>,
#> #   implied_infra <lgl>, direct_match <lgl>, Matched.Genus <chr>,
#> #   Matched.Species <chr>, Matched.Infraspecies <chr>, Matched.Infra.Rank <chr>
# }
```
