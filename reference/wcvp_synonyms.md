# Retrieve Synonyms Resolved Through the WCVP Backbone

**\[stable\]**

## Usage

``` r
wcvp_synonyms(
  taxon,
  target_df = NULL,
  prefilter_genus = TRUE,
  max_dist = 2,
  method = "osa",
  include_accepted = FALSE,
  output = c("compact", "full")
)
```

## Arguments

- taxon:

  Character vector of species names to resolve.

- target_df:

  Optional WCVP-like names table. If `NULL`, the optional `wcvpdata`
  checklist is used.

- prefilter_genus:

  Logical. Passed to
  [`wcvp_matching()`](https://paulesantos.github.io/wcvpmatch/reference/wcvp_matching.md).

- max_dist:

  Maximum string distance used while resolving `taxon`.

- method:

  String distance method passed to
  [`wcvp_matching()`](https://paulesantos.github.io/wcvpmatch/reference/wcvp_matching.md).

- include_accepted:

  Logical. Include the accepted name as a row with
  `name_role = "accepted"` before its synonyms? Defaults to `FALSE`.

- output:

  Output layout. `"compact"` (the default) returns the accepted name and
  its synonym fields. `"full"` additionally returns matching
  diagnostics, WCVP identifiers, basionym ID, and retrieval status.

## Value

A tibble with one row per input name and retrieved synonym. The default
compact result contains `submitted_name`, `accepted_taxon_name`,
`accepted_taxon_authors`, `name_role`, `synonym_name`,
`synonym_authors`, and `homotypic_synonym`. Inputs without a match or
without recorded synonyms are retained with missing synonym fields.

## Details

Resolves submitted species names with
[`wcvp_matching()`](https://paulesantos.github.io/wcvpmatch/reference/wcvp_matching.md)
and retrieves every WCVP record with `taxon_status = "Synonym"` that
points to the resolved accepted name. This means that an accepted name,
a synonym, or a minor spelling variant can all be used as the query.

Exact submitted names use a direct lookup before invoking the fuzzy
matching pipeline. The accepted-name lookup and normalized default
backbone are cached for the current R session and shared with the other
package functions.

## Examples

``` r
# \donttest{
wcvp_synonyms("Nopalea cochenillifera")
#> # A tibble: 5 × 7
#>   submitted_name         accepted_taxon_name    accepted_taxon_authors name_role
#>   <chr>                  <chr>                  <chr>                  <chr>    
#> 1 Nopalea cochenillifera Opuntia cochenillifera (L.) Mill.             synonym  
#> 2 Nopalea cochenillifera Opuntia cochenillifera (L.) Mill.             synonym  
#> 3 Nopalea cochenillifera Opuntia cochenillifera (L.) Mill.             synonym  
#> 4 Nopalea cochenillifera Opuntia cochenillifera (L.) Mill.             synonym  
#> 5 Nopalea cochenillifera Opuntia cochenillifera (L.) Mill.             synonym  
#> # ℹ 3 more variables: synonym_name <chr>, synonym_authors <chr>,
#> #   homotypic_synonym <lgl>
# }
```
