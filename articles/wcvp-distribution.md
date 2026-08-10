# Retrieve Distribution with wcvp_distribution()

[`wcvp_distribution()`](https://paulesantos.github.io/wcvpmatch/reference/wcvp_distribution.md)
retrieves geographic distribution from a pair of WCVP-like tables: one
names table and one distribution table. In regular use those tables can
come from
[`wcvpdata::wcvp_matching_names()`](https://rdrr.io/pkg/wcvpdata/man/wcvp_matching_names.html)
and
[`wcvpdata::wcvp_distribution()`](https://rdrr.io/pkg/wcvpdata/man/wcvp_distribution.html),
but the function also works with user-supplied tables that follow the
same schema.

It always returns a plain tibble, never an `sf` object. The row-level
result retains `area_code_l3`, the WGSrpd level-3 identifier to use when
geometries are joined in a separate spatial workflow.

This vignette uses a compact reproducible example so every code block
runs without depending on external data packages.

## Example backbone

``` r

make_distribution_names <- function() {
  tibble(
    plant_name_id = c(1, 2, 3, 4, 5, 6),
    accepted_plant_name_id = c(NA, 3, NA, NA, 1, NA),
    taxon_rank = c("Species", "Species", "Species", "Species", "Species", "Species"),
    taxon_status = c("Accepted", "Synonym", "Accepted", "Accepted", "Synonym", "Accepted"),
    family = c("Cactaceae", "Cactaceae", "Cactaceae", "Fagaceae", "Cactaceae", "Cactaceae"),
    genus = c("Opuntia", "Nopalea", "Opuntia", "Quercus", "Opuntia", "Mammillaria"),
    species = c("ficus-indica", "cochenillifera", "cochenillifera", "robur", "tuna", "elongata"),
    taxon_name = c(
      "Opuntia ficus-indica",
      "Nopalea cochenillifera",
      "Opuntia cochenillifera",
      "Quercus robur",
      "Opuntia tuna",
      "Mammillaria elongata"
    )
  )
}

make_distribution_records <- function() {
  tibble(
    plant_locality_id = 1:7,
    plant_name_id = c(1, 2, 3, 3, 4, 5, 6),
    continent_code_l1 = c("8", "8", "8", "4", "1", "8", "8"),
    continent = c(
      "SOUTHERN AMERICA", "SOUTHERN AMERICA", "SOUTHERN AMERICA",
      "NORTHERN AMERICA", "EUROPE", "SOUTHERN AMERICA", "SOUTHERN AMERICA"
    ),
    region_code_l2 = c("83", "83", "83", "41", "10", "85", "83"),
    region = c(
      "Western South America", "Western South America", "Western South America",
      "Mexico", "Europe", "Southern South America", "Western South America"
    ),
    area_code_l3 = c("MEX", "PER", "COL", "MEX", "ESP", "GAL", "MEX"),
    area = c("Mexico", "Peru", "Colombia", "Mexico", "Spain", "Galapagos", "Mexico"),
    introduced = c(0, 0, 0, 1, 0, 0, 0),
    extinct = c(0, 0, 0, 0, 0, 0, 0),
    location_doubtful = c(0, 0, 0, 0, 0, 0, 0)
  )
}

distribution_names <- make_distribution_names()
distribution_records <- make_distribution_records()

distribution_names
#> # A tibble: 6 × 8
#>   plant_name_id accepted_plant_name_id taxon_rank taxon_status family    genus  
#>           <dbl>                  <dbl> <chr>      <chr>        <chr>     <chr>  
#> 1             1                     NA Species    Accepted     Cactaceae Opuntia
#> 2             2                      3 Species    Synonym      Cactaceae Nopalea
#> 3             3                     NA Species    Accepted     Cactaceae Opuntia
#> 4             4                     NA Species    Accepted     Fagaceae  Quercus
#> 5             5                      1 Species    Synonym      Cactaceae Opuntia
#> 6             6                     NA Species    Accepted     Cactaceae Mammil…
#> # ℹ 2 more variables: species <chr>, taxon_name <chr>
distribution_records
#> # A tibble: 7 × 11
#>   plant_locality_id plant_name_id continent_code_l1 continent     region_code_l2
#>               <int>         <dbl> <chr>             <chr>         <chr>         
#> 1                 1             1 8                 SOUTHERN AME… 83            
#> 2                 2             2 8                 SOUTHERN AME… 83            
#> 3                 3             3 8                 SOUTHERN AME… 83            
#> 4                 4             3 4                 NORTHERN AME… 41            
#> 5                 5             4 1                 EUROPE        10            
#> 6                 6             5 8                 SOUTHERN AME… 85            
#> 7                 7             6 8                 SOUTHERN AME… 83            
#> # ℹ 6 more variables: region <chr>, area_code_l3 <chr>, area <chr>,
#> #   introduced <dbl>, extinct <dbl>, location_doubtful <dbl>
```

## Species-level retrieval

At species level,
[`wcvp_distribution()`](https://paulesantos.github.io/wcvpmatch/reference/wcvp_distribution.md)
parses input names, matches them through the same backend used by
[`wcvp_matching()`](https://paulesantos.github.io/wcvpmatch/reference/wcvp_matching.md),
resolves accepted names when possible, and then joins the result to the
distribution table.

``` r

species_out <- wcvp_distribution(
  c("Nopalea cochenilliferaa", "Taxon inexistente"),
  taxon_rank = "species",
  wcvp_names = distribution_names,
  wcvp_distributions = distribution_records
)

species_out |>
  select(
    submited_name,
    matched_taxon,
    accepted_taxon_name,
    area_code_l3,
    area,
    distribution_status
  )
#> # A tibble: 3 × 6
#>   submited_name           matched_taxon   accepted_taxon_name area_code_l3 area 
#>   <chr>                   <chr>           <chr>               <chr>        <chr>
#> 1 Nopalea cochenilliferaa Opuntia cochen… Opuntia cochenilli… MEX          Mexi…
#> 2 Nopalea cochenilliferaa Opuntia cochen… Opuntia cochenilli… COL          Colo…
#> 3 Taxon inexistente       NA              NA                  NA           NA   
#> # ℹ 1 more variable: distribution_status <chr>
```

The first query is fuzzy-matched to a synonym and then resolved to the
accepted taxon `Opuntia cochenillifera`. The second query is preserved
in the output with `distribution_status = "no_match"`.

## One row per submitted name

When `summarise_by_input = TRUE`, the function collapses the
area-related columns to one row per input taxon.

``` r

species_summary <- wcvp_distribution(
  c("Nopalea cochenilliferaa", "Taxon inexistente"),
  taxon_rank = "species",
  summarise_by_input = TRUE,
  wcvp_names = distribution_names,
  wcvp_distributions = distribution_records
)

species_summary |>
  select(
    submited_name,
    accepted_taxon_name,
    distribution_status,
    area_codes,
    distribution,
    n_areas
  )
#> # A tibble: 2 × 6
#>   submited_name  accepted_taxon_name distribution_status area_codes distribution
#>   <chr>          <chr>               <chr>               <chr>      <chr>       
#> 1 Nopalea coche… Opuntia cochenilli… distribution_found  COL - MEX  Colombia - …
#> 2 Taxon inexist… NA                  no_match            NA         NA          
#> # ℹ 1 more variable: n_areas <int>
```

This format is useful for reporting and export because `distribution`,
`areas`, `regions`, and `continents` are returned as collapsed text
values separated by `" - "`.

## Compact table for a spatial join

Use `output = "spatial"` to retain one row per taxon-area combination
while removing repeated matching and geographic-label columns. The
resulting `area_code_l3` remains ready to join to a level-3 polygon
layer later.

``` r

spatial_out <- wcvp_distribution(
  "Nopalea cochenillifera",
  taxon_rank = "species",
  output = "spatial",
  wcvp_names = distribution_names,
  wcvp_distributions = distribution_records
)

spatial_out
#> # A tibble: 2 × 10
#>   input_index submited_name       taxon_name area_code_l3 occurrence_type native
#>         <int> <chr>               <chr>      <chr>        <chr>           <lgl> 
#> 1           1 Nopalea cochenilli… Opuntia c… MEX          introduced      FALSE 
#> 2           1 Nopalea cochenilli… Opuntia c… COL          native          TRUE  
#> # ℹ 4 more variables: introduced <lgl>, extinct <lgl>, location_doubtful <lgl>,
#> #   distribution_status <chr>
```

## Genus-level retrieval

At genus level the function aggregates all accepted taxa in the genus
and then summarises occurrences by area.

``` r

genus_out <- wcvp_distribution(
  "Opuntia",
  taxon_rank = "genus",
  introduced = FALSE,
  wcvp_names = distribution_names,
  wcvp_distributions = distribution_records
)

genus_out |>
  select(matched_taxon, area_code_l3, area, occurrence_type, distribution_status)
#> # A tibble: 2 × 5
#>   matched_taxon area_code_l3 area     occurrence_type distribution_status
#>   <chr>         <chr>        <chr>    <chr>           <chr>              
#> 1 Opuntia       COL          Colombia native          distribution_found 
#> 2 Opuntia       MEX          Mexico   native          distribution_found
```

The introduced Mexico record is excluded here because
`introduced = FALSE`.

## Family-level retrieval

Family-level lookup can also use fuzzy matching through `fozziejoin`.

``` r

family_out <- wcvp_distribution(
  "Cactacee",
  taxon_rank = "family",
  max_dist = 1,
  wcvp_names = distribution_names,
  wcvp_distributions = distribution_records
)

family_out |>
  select(matched_taxon, match_distance, area_code_l3, area) |>
  distinct()
#> # A tibble: 2 × 4
#>   matched_taxon match_distance area_code_l3 area    
#>   <chr>                  <dbl> <chr>        <chr>   
#> 1 Cactaceae                  1 MEX          Mexico  
#> 2 Cactaceae                  1 COL          Colombia
```

## Fallback from species to genus

If a species query cannot recover species-level distribution, the
function can fall back to the matched genus without interrupting
execution.

``` r

fallback_out <- wcvp_distribution(
  "Opuntia especieinventada",
  taxon_rank = "species",
  wcvp_names = distribution_names,
  wcvp_distributions = distribution_records
)

fallback_out |>
  select(submited_name, matched_taxon, area_code_l3, area, distribution_status)
#> # A tibble: 3 × 5
#>   submited_name            matched_taxon area_code_l3 area   distribution_status
#>   <chr>                    <chr>         <chr>        <chr>  <chr>              
#> 1 Opuntia especieinventada Opuntia       MEX          Mexico genus_distribution…
#> 2 Opuntia especieinventada Opuntia       COL          Colom… genus_distribution…
#> 3 Opuntia especieinventada Opuntia       MEX          Mexico genus_distribution…
```

This fallback is marked with
`distribution_status = "genus_distribution_fallback"`.

## Practical notes

- Use `summarise_by_input = TRUE` when you need one row per submitted
  name.
- Prefer `output = "summary"` in new code; `summarise_by_input = TRUE`
  remains available for compatibility.
- Use `output = "spatial"` before joining to a spatial layer; use
  `output = "full"` when matching diagnostics and complete geographic
  labels are needed.
- Keep the default output when you need one row per taxon-area
  combination.
- Join a level-3 spatial table later with
  `left_join(..., by = c("LEVEL3_COD" = "area_code_l3"))`; this package
  intentionally does not create geometries.
- `taxon_rank = "order"` and `taxon_rank = "higher"` are available when
  the names table includes matching `order` and `higher` columns.
- The columns `native`, `introduced`, `extinct`, and `location_doubtful`
  can be used as filters before the final summary is returned.
- If `wcvpdata` is installed, you can omit `wcvp_names` and
  `wcvp_distributions` to use the default backbone.
