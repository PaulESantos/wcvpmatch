# wcvpmatch

`wcvpmatch` standardizes scientific plant names and reconciles them
against a World Checklist of Vascular Plants (WCVP)-style backbone. It
combines parsing, exact and fuzzy matching, accepted-name resolution,
and optional distribution retrieval in a single workflow.

The package is built around three main pieces:

- [`classify_spnames()`](https://paulesantos.github.io/wcvpmatch/reference/classify_spnames.md)
  parses and normalizes submitted names.
- [`wcvp_matching()`](https://paulesantos.github.io/wcvpmatch/reference/wcvp_matching.md)
  resolves names against a WCVP-like backbone.
- [`wcvp_distribution()`](https://paulesantos.github.io/wcvpmatch/reference/wcvp_distribution.md)
  retrieves species, genus, or family distribution from WCVP name and
  distribution tables.

## Installation

Install the development version from GitHub:

``` r

# install.packages("pak")
pak::pak("PaulESantos/wcvpmatch")
```

Install the CRAN release:

``` r

# install.packages("pak")
pak::pak("wcvpmatch")
```

`wcvpmatch` uses `fozziejoin` for fuzzy matching. If `fozziejoin` is
installed from source, a working Rust toolchain is needed.

Install Rust from <https://rust-lang.org/tools/install/>. On Windows,
the most practical setup for R + Rtools is:

``` bash
rustup override set stable-x86_64-pc-windows-gnu
```

Install `fozziejoin`:

``` r

pak::pak("fozziejoin")
```

To use the default WCVP backbone automatically, install `wcvpdata` from
`r-universe`:

``` r

install.packages(
  "wcvpdata",
  repos = c("https://paulesantos.r-universe.dev", "https://cloud.r-project.org")
)
```

## Quick example: `wcvp_matching()`

``` r

matching_backbone <- tibble(
  genus = c("Aniba", "Jaltomata", "Veronica", "Veronica"),
  species = c("heterotepala", "sagastegui", "vulcanica", "spathulata"),
  infraspecific_rank = NA_character_,
  infraspecies = NA_character_,
  plant_name_id = c(1, 2, 10, 200),
  taxon_name = c(
    "Aniba heterotepala",
    "Jaltomata sagastegui",
    "Veronica vulcanica",
    "Veronica spathulata"
  ),
  taxon_authors = c("A.Author", "B.Author", "C.Author", "D.Author"),
  taxon_status = c("Accepted", "Accepted", "Synonym", "Accepted"),
  accepted_plant_name_id = c(1, 2, 200, 200)
)

matching_result <- classify_spnames(
  c("Aniba heterotepala", "Jaltometa sagasteguii", "Veronica vulcanica")
) |>
  wcvp_matching(
    target_df = matching_backbone,
    allow_duplicates = TRUE,
    max_dist = 2,
    method = "osa",
    add_name_distance = TRUE,
    output_name_style = "snake_case"
  ) |>
  select(
    input_name,
    matched_taxon_name,
    accepted_taxon_name,
    taxon_status,
    matched_dist
  )

matching_result
#> # A tibble: 3 × 5
#>   input_name    matched_taxon_name accepted_taxon_name taxon_status matched_dist
#>   <chr>         <chr>              <chr>               <chr>               <dbl>
#> 1 Aniba hetero… Aniba heterotepala Aniba heterotepala  accepted                0
#> 2 Jaltometa sa… Jaltomata sagaste… Jaltomata sagasteg… accepted                2
#> 3 Veronica vul… Veronica vulcanica Veronica spathulata synonym                 0
```

## Quick example: `wcvp_distribution()`

``` r

distribution_names <- tibble(
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

distribution_records <- tibble(
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

distribution_result <- wcvp_distribution(
  c("Nopalea cochenilliferaa", "Taxon inexistente"),
  taxon_rank = "species",
  summarise_by_input = TRUE,
  wcvp_names = distribution_names,
  wcvp_distributions = distribution_records
) |>
  select(
    submited_name,
    accepted_taxon_name,
    distribution_status,
    distribution,
    n_areas
  )

distribution_result
#> # A tibble: 2 × 5
#>   submited_name     accepted_taxon_name distribution_status distribution n_areas
#>   <chr>             <chr>               <chr>               <chr>          <int>
#> 1 Nopalea cochenil… Opuntia cochenilli… distribution_found  Colombia - …       2
#> 2 Taxon inexistente <NA>                no_match            <NA>               0
```

## Learn more

The `README` keeps the examples short. For full guides, see the package
vignettes:

- [`vignette("wcvp-matching", package = "wcvpmatch")`](https://paulesantos.github.io/wcvpmatch/articles/wcvp-matching.md)
- [`vignette("wcvp-distribution", package = "wcvpmatch")`](https://paulesantos.github.io/wcvpmatch/articles/wcvp-distribution.md)

Those articles describe:

- accepted-name resolution and status handling
- staged fuzzy matching and diagnostics
- duplicate handling and profiling
- species, genus, and family distribution retrieval
- occurrence filters and summarised output

## Acknowledgement

`wcvpmatch` builds on ideas used in the
[`treemendous`](https://github.com/speckerf/treemendous) matching
workflow and extends them for WCVP-focused reconciliation and
reproducible row-level traceability.
