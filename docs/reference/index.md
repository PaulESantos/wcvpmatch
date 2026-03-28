# Package index

## Core workflow

Main entry points for parsing names, checking setup, and running the
full reconciliation pipeline.

- [`classify_spnames()`](https://paulesantos.github.io/wcvpmatch/reference/classify_spnames.md)
  **\[estable\]** : Classify Scientific Plant Names into Taxonomic
  Components
- [`wcvp_matching()`](https://paulesantos.github.io/wcvpmatch/reference/wcvp_matching.md)
  **\[estable\]** : Match Scientific Names Against WCVP
- [`wcvp_distribution()`](https://paulesantos.github.io/wcvpmatch/reference/wcvp_distribution.md)
  **\[estable\]** : Retrieve WCVP Distribution by Species, Genus, or
  Family
- [`wcvp_setup_info()`](https://paulesantos.github.io/wcvpmatch/reference/wcvp_setup_info.md)
  : Check Default Backbone Setup

## Matching stages

Lower-level helpers that expose the staged matching strategy used
internally by the main pipeline.

- [`wcvp_direct_match()`](https://paulesantos.github.io/wcvpmatch/reference/wcvp_direct_match.md)
  **\[estable\]** : Direct Match Species & Genus Binomial or Trinomial
  names
- [`wcvp_genus_match()`](https://paulesantos.github.io/wcvpmatch/reference/wcvp_genus_match.md)
  **\[estable\]** : Match Genus name
- [`wcvp_fuzzy_match_genus()`](https://paulesantos.github.io/wcvpmatch/reference/wcvp_fuzzy_match_genus.md)
  **\[estable\]** : Fuzzy Match Genus Name
- [`wcvp_direct_match_species_within_genus()`](https://paulesantos.github.io/wcvpmatch/reference/wcvp_direct_match_species_within_genus.md)
  **\[estable\]** : Direct Match Species within Genus
- [`wcvp_fuzzy_match_species_within_genus()`](https://paulesantos.github.io/wcvpmatch/reference/wcvp_fuzzy_match_species_within_genus.md)
  **\[estable\]** : Fuzzy Match Species within Genus
- [`wcvp_suffix_match_species_within_genus()`](https://paulesantos.github.io/wcvpmatch/reference/wcvp_suffix_match_species_within_genus.md)
  **\[estable\]** : Suffix Match Species within Genus

## Backbone utilities

Utilities for prefiltering and indexing a WCVP-like target backbone.

- [`build_genus_index()`](https://paulesantos.github.io/wcvpmatch/reference/build_genus_index.md)
  **\[estable\]** : Build a Genus Index for Fast Prefiltering
- [`prefilter_target_by_genus()`](https://paulesantos.github.io/wcvpmatch/reference/prefilter_target_by_genus.md)
  **\[estable\]** : Prefilter Target Backbone by Input Genera (Exact +
  Fuzzy)

## Example data

- [`fia`](https://paulesantos.github.io/wcvpmatch/reference/fia.md) :
  Cleaned Master Tree Species List from FIA
