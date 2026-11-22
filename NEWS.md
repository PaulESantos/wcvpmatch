# wcvpmatch 0.0.3

* Load only candidate genera from a versioned `wcvpdata` Arrow dataset when
  `prefilter_genus = TRUE`, then fetch accepted-name metadata by identifier.
  Retain the full-table path for custom or older backbones and explicit
  `prefilter_genus = FALSE`.

* Resolve taxonomic context once per taxon key and normalized submitted name,
  preserving authorship-sensitive selection when expanding duplicate inputs.
* Bound species and infraspecies fuzzy distance batches to 50,000 pairs,
  prune impossible lengths before distance calculation, and retain all nearest ties.
* Lazily reuse exact keys, accepted-ID indexes, species and infraspecies tables,
  and suffix roots. Column edits or subsets invalidate cached indexes; only the
  most recent prefiltered default target is retained across calls.
* Normalize pipeline input once and generate missing input names with vector
  operations. Standalone matching nodes retain input normalization.

* Skip full-backbone component normalization when the installed `wcvpdata`
  accessor declares matching schema version 1 with the required character
  columns. Preserve normalization for older or unknown schemas and custom
  backbones, including edited tables carrying provider attributes.

* Restores WCVP identifiers, authorship, taxonomic status, and accepted-name
  context for valid matched names.
* Resolves tied fuzzy genus candidates with exact species evidence when unique,
  and reports the decision in the new `match_ambiguity` output column.

# wcvpmatch 0.0.2

# wcvpmatch 0.0.1

* Initial release to CRAN.
* Standardizes and reconciles scientific plant names against the World Checklist of Vascular Plants (WCVP).
* Implements staged exact and fuzzy matching at genus, species, and infraspecies levels.
* Supports robust scientific name parsing through `classify_spnames()`.
* Provides traceable results with original and matched components.
