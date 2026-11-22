# Match Scientific Names Against WCVP

**\[stable\]**

Runs a matching pipeline with exact and partial matching for binomial
and trinomial names, including infraspecific rank validation.

With the default `wcvpdata` backbone and `prefilter_genus = TRUE`, a
matching-schema version 1 Arrow dataset is queried for candidate genera
before records are loaded into R. Accepted-name metadata is retrieved by
identifier, including accepted taxa in other genera. The genus catalog
and most recently selected target are reused within the R session. Older
data schemas and `prefilter_genus = FALSE` use the complete backbone.
Prepared custom backbones are reused, but custom data are not loaded
selectively.

## Usage

``` r
wcvp_matching(
  df,
  target_df = NULL,
  prefilter_genus = TRUE,
  allow_duplicates = FALSE,
  max_dist = 1,
  method = "osa",
  add_name_distance = FALSE,
  name_distance_method = "osa",
  profile = FALSE,
  output_name_style = c("snake_case", "legacy"),
  output = c("standard", "full")
)
```

## Arguments

- df:

  Input tibble/data.frame with either `Genus`/`Species` or
  `Orig.Genus`/`Orig.Species`. For trinomials, include `Infra.Rank` and
  `Infraspecies` (or `Orig.Infra.Rank`/`Orig.Infraspecies`).

- target_df:

  Optional custom target table. If `NULL`, data are read from the
  optional `wcvpdata` checklist when available; otherwise pass
  `target_df` explicitly.

- prefilter_genus:

  Logical. If `TRUE` (default), retain exact and fuzzy candidate genera.
  For a version 1 default `wcvpdata` backbone, only these genera are
  loaded from Arrow. If `FALSE`, load the complete default backbone. A
  supplied `target_df` is filtered in memory.

- allow_duplicates:

  Logical. If `TRUE`, duplicated taxon keys are deduplicated internally
  for matching and then expanded back to original rows. Output includes
  `input_index` for traceability to the original input.

- max_dist:

  Maximum distance used in all fuzzy matching stages (genus, species,
  infraspecies).

- method:

  A string indicating the fuzzy matching method (passed to
  `fozziejoin`). Supported methods:

  - `"levenshtein"`: Levenshtein edit distance (default).

  - `"osa"`: Optimal string alignment.

  - `"damerau_levensthein"` or `"dl"`: Damerau-Levenshtein distance.

  - `"hamming"`: Hamming distance (equal-length strings only).

  - `"lcs"`: Longest common subsequence.

  - `"qgram"`: Q-gram similarity (requires `q`).

  - `"cosine"`: Cosine similarity (requires `q`).

  - `"jaccard"`: Jaccard similarity (requires `q`).

  - `"jaro"`: Jaro similarity.

  - `"jaro_winkler"` or `"jw"`: Jaro-Winkler similarity.

  - `"soundex"`: Soundex codes based on the National Archives standard.

- add_name_distance:

  Logical. If `TRUE`, add `matched_dist` as pairwise distance between
  input name (`Input.Name` fallback `Orig.Name`) and
  `matched_taxon_name`.

- name_distance_method:

  Method passed to
  [`stringdist::stringdist`](https://rdrr.io/pkg/stringdist/man/stringdist.html)
  when `add_name_distance = TRUE` (for example `"osa"`).

- profile:

  Logical. If `TRUE`, attach a timing table in the `"timings"` attribute
  of the returned tibble, with elapsed seconds per pipeline stage.

- output_name_style:

  Naming style for output columns:

  - `"snake_case"` returns standardized lower snake_case names.

  - `"legacy"` keeps the historical mixed naming convention.

- output:

  Output layout. `"standard"` (the default) returns the parsed, matched,
  and accepted-name fields needed for routine reconciliation. `"full"`
  additionally returns internal matching-stage flags, fuzzy distances,
  and parsing diagnostics.

## Value

A tibble with parsed input fields and matched/accepted taxonomic
context. The standard output contains `input_index`, input and matched
name components, authorship, matched and accepted IDs/names, status,
`match_ambiguity`, and `matched`. Use `output = "full"` for matching
diagnostics.

## Examples

``` r
# \donttest{
library(wcvpmatch)
# Match a single name
wcvp_matching(data.frame(Genus = "Opuntia", Species = "yanganucensis"))
#> ℹ Input was converted from <data.frame> to a <tibble>.
#>   See <https://tibble.tidyverse.org/> for more details.
#> # A tibble: 1 × 21
#>   input_index input_name            orig_name orig_genus orig_species infra_rank
#>         <int> <chr>                 <chr>     <chr>      <chr>        <chr>     
#> 1           1 Opuntia yanganucensis NA        Opuntia    yanganucens… NA        
#> # ℹ 15 more variables: orig_infraspecies <chr>, matched_genus <chr>,
#> #   matched_species <chr>, matched_infra_rank <chr>,
#> #   matched_infraspecies <chr>, author <chr>, matched_plant_name_id <dbl>,
#> #   matched_taxon_name <chr>, matched_taxon_authors <chr>, taxon_status <chr>,
#> #   accepted_plant_name_id <dbl>, accepted_taxon_name <chr>,
#> #   accepted_taxon_authors <chr>, is_accepted_name <lgl>, matched <lgl>

# Match multiple names with snake_case output
names <- c("Aniba heterotepala", "Anthurium quipuscoae")
df <- classify_spnames(names)
wcvp_matching(df, output_name_style = "snake_case")
#> # A tibble: 2 × 21
#>   input_index input_name           orig_name  orig_genus orig_species infra_rank
#>         <int> <chr>                <chr>      <chr>      <chr>        <chr>     
#> 1           1 Aniba heterotepala   Aniba het… Aniba      heterotepala NA        
#> 2           2 Anthurium quipuscoae Anthurium… Anthurium  quipuscoae   NA        
#> # ℹ 15 more variables: orig_infraspecies <chr>, matched_genus <chr>,
#> #   matched_species <chr>, matched_infra_rank <chr>,
#> #   matched_infraspecies <chr>, author <chr>, matched_plant_name_id <dbl>,
#> #   matched_taxon_name <chr>, matched_taxon_authors <chr>, taxon_status <chr>,
#> #   accepted_plant_name_id <dbl>, accepted_taxon_name <chr>,
#> #   accepted_taxon_authors <chr>, is_accepted_name <lgl>, matched <lgl>

# Attach per-stage timings for profiling
out <- wcvp_matching(df, output_name_style = "snake_case", profile = TRUE)
attr(out, "timings")
#> # A tibble: 14 × 3
#>    stage                                  elapsed_seconds  rows
#>    <chr>                                            <dbl> <int>
#>  1 check_df_format                                0.00400     2
#>  2 deduplicate_input                              0           2
#>  3 check_df_consistency                           0.00300     2
#>  4 selective_backbone                             0.00500     2
#>  5 wcvp_direct_match                              0.00200     2
#>  6 wcvp_genus_match                               0.00100     0
#>  7 wcvp_fuzzy_match_genus                         0.00100     0
#>  8 wcvp_direct_match_species_within_genus         0.00100     0
#>  9 wcvp_suffix_match_species_within_genus         0.00100     0
#> 10 wcvp_fuzzy_match_species_within_genus          0           0
#> 11 prepare_taxonomic_context_data                 0.0780      2
#> 12 add_taxonomic_context                          0.00900     2
#> 13 standardize_output_names                       0           2
#> 14 total                                          0.144       2
# }
```
