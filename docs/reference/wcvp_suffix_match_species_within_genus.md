# Suffix Match Species within Genus

**\[experimental\]**

Tries to match the specific epithet by exchanging common suffixes within
an already matched genus in 'WCVP'. The following suffixes are captured:
`c("a", "i", "is", "um", "us", "ae")`.

## Usage

``` r
wcvp_suffix_match_species_within_genus(df, target_df = NULL)
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

Returns a `tibble` with the additional logical column
`suffix_match_species_within_genus`, indicating whether the specific
epithet was successfully matched within the matched genus (`TRUE`) or
not (`FALSE`).

## Examples

``` r
# \donttest{
library(wcvpmatch)
df <- data.frame(Orig.Genus = "Opuntia", Orig.Species = "yanganucensa", Matched.Genus = "Opuntia")
wcvp_suffix_match_species_within_genus(df)
#> ℹ Input was converted from <data.frame> to a <tibble>.
#>   See <https://tibble.tidyverse.org/> for more details.
#> # A tibble: 1 × 21
#>   Orig.Genus Orig.Species suffix_match_species_within…¹ Matched.Genus Input.Name
#>   <chr>      <chr>        <lgl>                         <chr>         <chr>     
#> 1 Opuntia    yanganucensa TRUE                          Opuntia       Opuntia y…
#> # ℹ abbreviated name: ¹​suffix_match_species_within_genus
#> # ℹ 16 more variables: Orig.Name <chr>, Author <chr>, Orig.Infraspecies <chr>,
#> #   Infra.Rank <chr>, Rank <dbl>, has_cf <lgl>, has_aff <lgl>, is_sp <lgl>,
#> #   is_spp <lgl>, had_hybrid <lgl>, rank_late <lgl>, rank_missing_infra <lgl>,
#> #   had_na_author <lgl>, implied_infra <lgl>, sorter <dbl>,
#> #   Matched.Species <chr>
# }
```
