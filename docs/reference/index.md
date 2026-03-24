# Package index

## Core workflow

Main entry points for parsing names, checking setup, and running the
full reconciliation pipeline.

- [`classify_spnames()`](https://paulesantos.github.io/wcvpmatch/reference/classify_spnames.md)
  **\[experimental\]** : Classify Scientific Plant Names into Taxonomic
  Components
- [`wcvp_matching()`](https://paulesantos.github.io/wcvpmatch/reference/wcvp_matching.md)
  **\[experimental\]** : Match Scientific Names Against WCVP
- [`wcvp_setup_info()`](https://paulesantos.github.io/wcvpmatch/reference/wcvp_setup_info.md)
  : Check Default Backbone Setup

## Matching stages

Lower-level helpers that expose the staged matching strategy used
internally by the main pipeline.

- [`wcvp_direct_match()`](https://paulesantos.github.io/wcvpmatch/reference/wcvp_direct_match.md)
  **\[experimental\]** : Direct Match Species & Genus Binomial or
  Trinomial names
- [`wcvp_genus_match()`](https://paulesantos.github.io/wcvpmatch/reference/wcvp_genus_match.md)
  **\[experimental\]** : Match Genus name
- [`wcvp_fuzzy_match_genus()`](https://paulesantos.github.io/wcvpmatch/reference/wcvp_fuzzy_match_genus.md)
  **\[experimental\]** : Fuzzy Match Genus Name
- [`wcvp_direct_match_species_within_genus()`](https://paulesantos.github.io/wcvpmatch/reference/wcvp_direct_match_species_within_genus.md)
  **\[experimental\]** : Direct Match Species within Genus
- [`wcvp_fuzzy_match_species_within_genus()`](https://paulesantos.github.io/wcvpmatch/reference/wcvp_fuzzy_match_species_within_genus.md)
  **\[experimental\]** : Fuzzy Match Species within Genus
- [`wcvp_suffix_match_species_within_genus()`](https://paulesantos.github.io/wcvpmatch/reference/wcvp_suffix_match_species_within_genus.md)
  **\[experimental\]** : Suffix Match Species within Genus

## Backbone utilities

Utilities for prefiltering and indexing a WCVP-like target backbone.

- [`build_genus_index()`](https://paulesantos.github.io/wcvpmatch/reference/build_genus_index.md)
  **\[experimental\]** : Build a Genus Index for Fast Prefiltering
- [`prefilter_target_by_genus()`](https://paulesantos.github.io/wcvpmatch/reference/prefilter_target_by_genus.md)
  **\[experimental\]** : Prefilter Target Backbone by Input Genera
  (Exact + Fuzzy)

## Example data

- [`fia`](https://paulesantos.github.io/wcvpmatch/reference/fia.md) :
  Cleaned Master Tree Species List from FIA
