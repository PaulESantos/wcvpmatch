# Prefilter Target Backbone by Input Genera (Exact + Fuzzy)

**\[stable\]**

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
df <- data.frame(Genus = "Opuntia", Species = "yanganucensis")
target <- data.frame(genus = "Opuntia", species = "yanganucensis", plant_name_id = 1)
wcvpmatch:::prefilter_target_by_genus(df, target_df = target)
#> i Input was converted from <data.frame> to a <tibble>.
#>   See <https://tibble.tidyverse.org/> for more details.
#> # A tibble: 1 x 7
#>   genus   species    plant_name_id infraspecific_rank infraspecies Genus Species
#>   <chr>   <chr>              <dbl> <chr>              <chr>        <chr> <chr>  
#> 1 Opuntia yanganuce~             1 NA                 NA           Opun~ yangan~
# }
```
