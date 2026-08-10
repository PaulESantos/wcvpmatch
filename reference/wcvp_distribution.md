# Retrieve Tabular WCVP Distribution by Taxonomic Rank

**\[stable\]**

## Usage

``` r
wcvp_distribution(
  taxon,
  taxon_rank = c("species", "genus", "family", "order", "higher"),
  native = TRUE,
  introduced = TRUE,
  extinct = TRUE,
  location_doubtful = TRUE,
  wcvp_names = NULL,
  wcvp_distributions = NULL,
  prefilter_genus = TRUE,
  fallback_to_genus = TRUE,
  summarise_by_input = FALSE,
  max_dist = NULL,
  method = "osa",
  output = c("standard", "full", "spatial", "summary")
)
```

## Arguments

- taxon:

  Character vector of taxa to query.

- taxon_rank:

  Character scalar. One of `"species"`, `"genus"`, `"family"`,
  `"order"`, or `"higher"`. The last two require corresponding `order`
  or `higher` columns in `wcvp_names`.

- native:

  Logical. Include native occurrences? Defaults to `TRUE`.

- introduced:

  Logical. Include introduced occurrences? Defaults to `TRUE`.

- extinct:

  Logical. Include extinct occurrences? Defaults to `TRUE`.

- location_doubtful:

  Logical. Include doubtful occurrences? Defaults to `TRUE`.

- wcvp_names:

  Optional WCVP names table. If `NULL`, the function loads
  [`wcvpdata::wcvp_matching_names()`](https://rdrr.io/pkg/wcvpdata/man/wcvp_matching_names.html).

- wcvp_distributions:

  Optional WCVP distribution table. If `NULL`, the function loads
  [`wcvpdata::wcvp_distribution()`](https://rdrr.io/pkg/wcvpdata/man/wcvp_distribution.html).

- prefilter_genus:

  Logical. Forwarded to
  [`wcvp_matching()`](https://paulesantos.github.io/wcvpmatch/reference/wcvp_matching.md)
  for species-level queries. Ignored for all other taxonomic ranks.

- fallback_to_genus:

  Logical. If `TRUE` and `taxon_rank = "species"`, inputs without
  species-level distribution are retried at genus level.

- summarise_by_input:

  Logical. If `TRUE`, return one row per input taxon with collapsed
  distribution fields. In this mode, `area_codes`, `areas`, `regions`,
  `continents`, and `distribution` are returned as character strings
  separated by `" - "` rather than list-columns.

- max_dist:

  Maximum string distance. If `NULL`, species queries default to `2` and
  genus/family queries to `0`.

- method:

  String distance method passed to `fozziejoin`.

- output:

  Output layout: `"standard"` (the default analytical taxon-area table),
  `"full"` (the complete audit table), `"spatial"` (the compact
  taxon-area table for a later spatial join), or `"summary"` (one row
  per submitted taxon). `summarise_by_input = TRUE` is retained as a
  backwards-compatible alias for `output = "summary"`.

## Value

A non-spatial tibble. The default `output = "standard"` returns one row
per matched query-area combination with the submitted and resolved taxa,
geographic hierarchy, and four occurrence flags (12 columns).
`output = "full"` additionally returns matching provenance and
identifiers. `output = "spatial"` returns only the taxon-area fields
needed for a later spatial join. `output = "summary"` returns one row
per input taxon with collapsed text fields such as `distribution`,
`areas`, `area_codes`, `regions`, `continents`, and `n_areas`.

## Details

Queries distribution records by matching a taxon name against the WCVP
names table and then resolving the corresponding rows in the WCVP
distribution table. The function is designed around
[`wcvpdata::wcvp_matching_names()`](https://rdrr.io/pkg/wcvpdata/man/wcvp_matching_names.html)
and
[`wcvpdata::wcvp_distribution()`](https://rdrr.io/pkg/wcvpdata/man/wcvp_distribution.html),
but custom tables with the same schema can also be supplied.

Matching is performed with `fozziejoin`, using compact lookup tables and
length-based prefiltering to keep the candidate set small. Species
queries are resolved in two stages: genus candidates are matched first,
then species names are searched only within those candidate genera.

If species-level matches resolve to synonyms and the names table
contains `accepted_plant_name_id`, distribution is recovered from the
accepted taxon. For queries above species, accepted names are preferred
to avoid double counting synonym records.

The result deliberately contains no geometry and does not require `sf`.
`area_code_l3` is the stable WGSrpd level-3 key intended for a later
join to a user-supplied spatial object.

Default names and distribution tables are cached for the current R
session. Exact species queries use a direct lookup; lowercase strings
and species keys over the full backbone are only built when a fuzzy
query needs them.

## Examples

``` r
# \donttest{
library(wcvpmatch)

wcvp_distribution("Opuntia ficus-indica", taxon_rank = "species")
#> # A tibble: 82 × 15
#>    submited_name        taxon_rank matched_taxon match_distance continent region
#>    <chr>                <chr>      <chr>                  <dbl> <chr>     <chr> 
#>  1 Opuntia ficus-indica species    Opuntia ficu…              0 AFRICA    East …
#>  2 Opuntia ficus-indica species    Opuntia ficu…              0 AFRICA    Macar…
#>  3 Opuntia ficus-indica species    Opuntia ficu…              0 AFRICA    Macar…
#>  4 Opuntia ficus-indica species    Opuntia ficu…              0 AFRICA    Macar…
#>  5 Opuntia ficus-indica species    Opuntia ficu…              0 AFRICA    Macar…
#>  6 Opuntia ficus-indica species    Opuntia ficu…              0 AFRICA    Middl…
#>  7 Opuntia ficus-indica species    Opuntia ficu…              0 AFRICA    North…
#>  8 Opuntia ficus-indica species    Opuntia ficu…              0 AFRICA    North…
#>  9 Opuntia ficus-indica species    Opuntia ficu…              0 AFRICA    North…
#> 10 Opuntia ficus-indica species    Opuntia ficu…              0 AFRICA    North…
#> # ℹ 72 more rows
#> # ℹ 9 more variables: area_code_l3 <chr>, area <chr>,
#> #   accepted_taxon_name <chr>, occurrence_type <chr>, native <lgl>,
#> #   introduced <lgl>, extinct <lgl>, location_doubtful <lgl>,
#> #   distribution_status <chr>
wcvp_distribution("Opuntia", taxon_rank = "genus")
#> # A tibble: 198 × 15
#>    submited_name taxon_rank matched_taxon match_distance continent region       
#>    <chr>         <chr>      <chr>                  <dbl> <chr>     <chr>        
#>  1 Opuntia       genus      Opuntia                    0 AFRICA    East Tropica…
#>  2 Opuntia       genus      Opuntia                    0 AFRICA    East Tropica…
#>  3 Opuntia       genus      Opuntia                    0 AFRICA    Macaronesia  
#>  4 Opuntia       genus      Opuntia                    0 AFRICA    Macaronesia  
#>  5 Opuntia       genus      Opuntia                    0 AFRICA    Macaronesia  
#>  6 Opuntia       genus      Opuntia                    0 AFRICA    Macaronesia  
#>  7 Opuntia       genus      Opuntia                    0 AFRICA    Middle Atlan…
#>  8 Opuntia       genus      Opuntia                    0 AFRICA    Middle Atlan…
#>  9 Opuntia       genus      Opuntia                    0 AFRICA    Northeast Tr…
#> 10 Opuntia       genus      Opuntia                    0 AFRICA    Northeast Tr…
#> # ℹ 188 more rows
#> # ℹ 9 more variables: area_code_l3 <chr>, area <chr>,
#> #   accepted_taxon_name <chr>, occurrence_type <chr>, native <lgl>,
#> #   introduced <lgl>, extinct <lgl>, location_doubtful <lgl>,
#> #   distribution_status <chr>
wcvp_distribution("Cactaceae", taxon_rank = "family")
#> # A tibble: 233 × 15
#>    submited_name taxon_rank matched_taxon match_distance continent region       
#>    <chr>         <chr>      <chr>                  <dbl> <chr>     <chr>        
#>  1 Cactaceae     family     Cactaceae                  0 AFRICA    East Tropica…
#>  2 Cactaceae     family     Cactaceae                  0 AFRICA    East Tropica…
#>  3 Cactaceae     family     Cactaceae                  0 AFRICA    East Tropica…
#>  4 Cactaceae     family     Cactaceae                  0 AFRICA    Macaronesia  
#>  5 Cactaceae     family     Cactaceae                  0 AFRICA    Macaronesia  
#>  6 Cactaceae     family     Cactaceae                  0 AFRICA    Macaronesia  
#>  7 Cactaceae     family     Cactaceae                  0 AFRICA    Macaronesia  
#>  8 Cactaceae     family     Cactaceae                  0 AFRICA    Middle Atlan…
#>  9 Cactaceae     family     Cactaceae                  0 AFRICA    Middle Atlan…
#> 10 Cactaceae     family     Cactaceae                  0 AFRICA    Northeast Tr…
#> # ℹ 223 more rows
#> # ℹ 9 more variables: area_code_l3 <chr>, area <chr>,
#> #   accepted_taxon_name <chr>, occurrence_type <chr>, native <lgl>,
#> #   introduced <lgl>, extinct <lgl>, location_doubtful <lgl>,
#> #   distribution_status <chr>
# When `order` is present in a custom names table:
# \dontrun{wcvp_distribution("Caryophyllales", taxon_rank = "order", wcvp_names = custom_names)}
# }
```
