# Resolve Names with wcvp_matching()

[`wcvp_matching()`](https://paulesantos.github.io/wcvpmatch/reference/wcvp_matching.md)
is the main reconciliation function in `wcvpmatch`. It takes parsed
names or a minimal genus/species table, matches those names against a
WCVP-like backbone, and returns both the matched taxon and accepted-name
context.

This vignette uses a compact in-memory backbone so every example runs
quickly and reproducibly.

## Example backbone

``` r

make_matching_backbone <- function() {
  tibble(
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
}

matching_backbone <- make_matching_backbone()
matching_backbone
#> # A tibble: 4 × 9
#>   genus     species     infraspecific_rank infraspecies plant_name_id taxon_name
#>   <chr>     <chr>       <chr>              <chr>                <dbl> <chr>     
#> 1 Aniba     heterotepa… NA                 NA                       1 Aniba het…
#> 2 Jaltomata sagastegui  NA                 NA                       2 Jaltomata…
#> 3 Veronica  vulcanica   NA                 NA                      10 Veronica …
#> 4 Veronica  spathulata  NA                 NA                     200 Veronica …
#> # ℹ 3 more variables: taxon_authors <chr>, taxon_status <chr>,
#> #   accepted_plant_name_id <dbl>
```

## Parse the input names

[`classify_spnames()`](https://paulesantos.github.io/wcvpmatch/reference/classify_spnames.md)
standardizes the incoming names before matching. It is the recommended
entry point when you start from raw taxon strings.

``` r

parsed_names <- classify_spnames(
  c("Aniba heterotepala", "Jaltometa sagasteguii", "Veronica vulcanica")
)

parsed_names |>
  select(Input.Name, Orig.Genus, Orig.Species, Rank)
#> # A tibble: 3 × 4
#>   Input.Name            Orig.Genus Orig.Species  Rank
#>   <chr>                 <chr>      <chr>        <dbl>
#> 1 Aniba heterotepala    Aniba      heterotepala     2
#> 2 Jaltometa sagasteguii Jaltometa  sagasteguii      2
#> 3 Veronica vulcanica    Veronica   vulcanica        2
```

## Run the matching pipeline

The example below shows three common outcomes:

- an exact accepted match
- a fuzzy species recovery within the matched genus
- an exact synonym that resolves to a different accepted name

``` r

matched <- wcvp_matching(
  parsed_names,
  target_df = matching_backbone,
  allow_duplicates = TRUE,
  max_dist = 2,
  method = "osa",
  add_name_distance = TRUE,
  output_name_style = "snake_case",
  output = "full"
)

matched |>
  select(
    input_name,
    matched_taxon_name,
    accepted_taxon_name,
    taxon_status,
    matched_dist
  )
#> # A tibble: 3 × 5
#>   input_name    matched_taxon_name accepted_taxon_name taxon_status matched_dist
#>   <chr>         <chr>              <chr>               <chr>               <dbl>
#> 1 Aniba hetero… Aniba heterotepala Aniba heterotepala  accepted                0
#> 2 Jaltometa sa… Jaltomata sagaste… Jaltomata sagasteg… accepted                2
#> 3 Veronica vul… Veronica vulcanica Veronica spathulata synonym                 0
```

## Read the pathway flags

The logical stage flags show how each input was recovered. These are
useful when you want to audit matching behavior or separate exact from
fuzzy recoveries.

``` r

matched |>
  select(
    input_name,
    direct_match,
    genus_match,
    fuzzy_match_genus,
    direct_match_species_within_genus,
    suffix_match_species_within_genus,
    fuzzy_match_species_within_genus,
    matched
  )
#> # A tibble: 3 × 8
#>   input_name   direct_match genus_match fuzzy_match_genus direct_match_species…¹
#>   <chr>        <lgl>        <lgl>       <lgl>             <lgl>                 
#> 1 Aniba heter… TRUE         NA          NA                NA                    
#> 2 Jaltometa s… FALSE        FALSE       TRUE              FALSE                 
#> 3 Veronica vu… TRUE         NA          NA                NA                    
#> # ℹ abbreviated name: ¹​direct_match_species_within_genus
#> # ℹ 3 more variables: suffix_match_species_within_genus <lgl>,
#> #   fuzzy_match_species_within_genus <lgl>, matched <lgl>
```

In this example, `Aniba heterotepala` is recovered directly, while
`Jaltometa sagasteguii` is resolved through fuzzy matching.

## Accepted-name resolution

[`wcvp_matching()`](https://paulesantos.github.io/wcvpmatch/reference/wcvp_matching.md)
returns both the matched taxon and the accepted taxon. That is important
when the submitted name is a synonym.

``` r

matched |>
  filter(input_name == "Veronica vulcanica") |>
  select(
    input_name,
    matched_taxon_name,
    accepted_taxon_name,
    matched_taxon_authors,
    accepted_taxon_authors,
    taxon_status,
    is_accepted_name
  )
#> # A tibble: 1 × 7
#>   input_name        matched_taxon_name accepted_taxon_name matched_taxon_authors
#>   <chr>             <chr>              <chr>               <chr>                
#> 1 Veronica vulcani… Veronica vulcanica Veronica spathulata C.Author             
#> # ℹ 3 more variables: accepted_taxon_authors <chr>, taxon_status <chr>,
#> #   is_accepted_name <lgl>
```

Here the matched taxon is `Veronica vulcanica`, but the accepted taxon
is `Veronica spathulata`.

## Duplicate handling

Duplicate input rows are allowed when `allow_duplicates = TRUE`. In that
case the function preserves row identity with `input_index`.

``` r

duplicate_input <- tibble(
  Genus = c("Aniba", "Aniba"),
  Species = c("heterotepala", "heterotepala"),
  Input.Name = c("Aniba heterotepala", "Aniba heterotepala")
)

wcvp_matching(
  duplicate_input,
  target_df = matching_backbone,
  allow_duplicates = TRUE,
  output_name_style = "snake_case"
) |>
  select(input_index, input_name, matched_taxon_name, accepted_taxon_name)
#> # A tibble: 2 × 4
#>   input_index input_name         matched_taxon_name accepted_taxon_name
#>         <int> <chr>              <chr>              <chr>              
#> 1           1 Aniba heterotepala Aniba heterotepala Aniba heterotepala 
#> 2           2 Aniba heterotepala Aniba heterotepala Aniba heterotepala
```

## Practical notes

- Start from
  [`classify_spnames()`](https://paulesantos.github.io/wcvpmatch/reference/classify_spnames.md)
  when your input is raw text.
- Use `allow_duplicates = TRUE` for production data that may contain
  repeated names.
- Set `output_name_style = "snake_case"` if you want easier downstream
  use with `dplyr`.
- Use `add_name_distance = TRUE` when you want a compact numeric summary
  of how far each submitted name is from the matched name.
