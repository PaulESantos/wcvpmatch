# Prefilter Target Backbone by Input Genera (Exact + Fuzzy)

**\[experimental\]**

Reduces the target backbone to genera relevant for the current input
names. This is designed as a pre-step before
[`wcvp_matching()`](https://paulesantos.github.io/wcvpmatch/reference/wcvp_matching.md)
to reduce search space.

Strategy:

- Exact genus candidates are always included.

- Optional fuzzy genus candidates are included when
  `include_fuzzy = TRUE`.

- Returned object preserves the standard target schema used by the
  package.

## Usage

``` r
prefilter_target_by_genus(
  df,
  target_df = NULL,
  genus_index = NULL,
  include_fuzzy = TRUE,
  max_dist = 1,
  method = "osa"
)
```

## Arguments

- df:

  Input tibble/data.frame with either `Genus`/`Species` or
  `Orig.Genus`/`Orig.Species`.

- target_df:

  Optional custom target table. If `NULL`, the optional `wcvpdata`
  checklist is used when available; otherwise pass a backbone
  explicitly.

- genus_index:

  Optional pre-built index from
  [`build_genus_index()`](https://paulesantos.github.io/wcvpmatch/reference/build_genus_index.md).
  If `NULL`, it is built on the fly.

- include_fuzzy:

  Logical. If `TRUE`, include fuzzy-matched genera.

- max_dist:

  Maximum fuzzy distance for genus matching (used when
  `include_fuzzy = TRUE`).

- method:

  String distance method passed to `fozziejoin`.

## Value

A prefiltered `target_df` tibble compatible with
`wcvp_matching(target_df = ...)`. Attributes:

- candidate_genera:

  Character vector of selected genera.

- exact_genera:

  Character vector of exact matched genera.

- fuzzy_genera:

  Character vector of fuzzy matched genera.

## Examples

``` r
# \donttest{
library(wcvpmatch)
df <- data.frame(Genus = "Opuntia", Species = "yanganucensis")
prefilter_target_by_genus(df)
#> ℹ Input was converted from <data.frame> to a <tibble>.
#>   See <https://tibble.tidyverse.org/> for more details.
#> # A tibble: 1,509 × 33
#>    plant_name_id ipni_id    taxon_rank taxon_status family    genus_hybrid genus
#>            <dbl> <chr>      <chr>      <chr>        <chr>     <chr>        <chr>
#>  1       2401900 175654-2   Variety    Synonym      Cactaceae NA           Opun…
#>  2       3294155 77302436-1 Subspecies Synonym      Cactaceae NA           Opun…
#>  3       2877217 175097-2   Variety    Accepted     Cactaceae NA           Opun…
#>  4       3294157 77302438-1 Subspecies Synonym      Cactaceae NA           Opun…
#>  5       2877218 175098-2   Variety    Accepted     Cactaceae NA           Opun…
#>  6       3245753 77212806-1 Subspecies Synonym      Cactaceae NA           Opun…
#>  7       2877219 175099-2   Variety    Accepted     Cactaceae NA           Opun…
#>  8       2877222 175102-2   Variety    Accepted     Cactaceae NA           Opun…
#>  9       2877259 175376-2   Variety    Synonym      Cactaceae NA           Opun…
#> 10       2400776 175023-2   Variety    Synonym      Cactaceae NA           Opun…
#> # ℹ 1,499 more rows
#> # ℹ 26 more variables: species_hybrid <chr>, species <chr>,
#> #   infraspecific_rank <chr>, infraspecies <chr>, parenthetical_author <chr>,
#> #   primary_author <chr>, publication_author <chr>, place_of_publication <chr>,
#> #   volume_and_page <chr>, first_published <chr>, nomenclatural_remarks <chr>,
#> #   geographic_area <chr>, lifeform_description <chr>,
#> #   climate_description <chr>, taxon_name <chr>, taxon_authors <chr>, …
# }
```
