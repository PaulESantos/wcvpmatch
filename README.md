# wcvpmatch

`wcvpmatch` standardizes scientific plant names and matches them against the
World Checklist of Vascular Plants (WCVP) using `rWCVPdata::wcvp_names`.

The package supports:

- parsing and normalization of scientific names (`classify_spnames()`)
- exact and fuzzy matching at genus/species/infraspecies levels (`wcvp_matching()`)
- optional genus prefiltering for performance (`prefilter_target_by_genus()`)
- duplicate-aware matching with row traceability (`allow_duplicates`, `input_index`)

## Installation

Install `wcvpmatch`:

```r
pak::pak("PaulESantos/wcvpmatch")
```

Install WCVP data source (required to use `wcvpmatch`):

```r
pak::pak("PaulESantos/rWCVPdata")
```

## Quick Start

{r }
library(wcvpmatch)

splist <- c(
  "Praecereus euchlorus subsp. diffusus",
  "Cleistocactus fieldianus",
  "Opuntia yanganucensis",
  "Trichocereus macrogonus var. pachanoi",
  "opuntia sp."
)

parsed <- classify_spnames(splist)

res <- wcvp_matching(
  parsed,
  prefilter_genus = TRUE,
  allow_duplicates = TRUE,
  max_dist = 1,
  method = "osa"
)

res |>
  dplyr::select(
    input_index, Input.Name, Orig.Name,
    Orig.Genus, Orig.Species, Infra.Rank, Orig.Infraspecies,
    Matched.Genus, Matched.Species, Matched.Infra.Rank, Matched.Infraspecies,
    Author, matched
  )
```

## Input and Output

### Input options

`wcvp_matching()` accepts:

- parsed output from `classify_spnames()`
- or a tibble with minimal columns `Genus` and `Species`

Optional fields:

- `Infra.Rank`, `Infraspecies` for trinomial matching
- `Input.Name` for preserving original text in output

### Output highlights

`wcvp_matching()` returns:

- ordered, traceable rows via `input_index`
- original and matched taxonomy columns
- matching path flags (`direct_match`, `fuzzy_match_genus`, etc.)
- `matched` logical flag

## Performance Notes

For medium/large inputs, recommended settings are:

```r
wcvp_matching(
  parsed_df,
  prefilter_genus = TRUE,
  allow_duplicates = TRUE
)
```

This typically reduces runtime by limiting candidate genera before species-level
matching and avoids recomputing duplicated names.

## Custom Backbone (Optional)

You can bypass `rWCVPdata` by passing your own `target_df`:

```r
custom_target <- tibble::tibble(
  genus = c("Acer", "Quercus"),
  species = c("rubrum", "robur"),
  infraspecific_rank = NA_character_,
  infraspecies = NA_character_
)

res <- wcvp_matching(parsed, target_df = custom_target)
```

## Core Public Functions

- `classify_spnames()`
- `wcvp_matching()`
- `build_genus_index()`
- `prefilter_target_by_genus()`
- `direct_match()`
- `genus_match()`
- `fuzzy_match_genus()`
- `direct_match_species_within_genus()`
- `fuzzy_match_species_within_genus()`
- `suffix_match_species_within_genus()`

## License

CC BY 4.0
