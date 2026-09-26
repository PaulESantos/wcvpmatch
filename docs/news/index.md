# Changelog

## wcvpmatch 0.0.3

- Restores WCVP identifiers, authorship, taxonomic status, and
  accepted-name context for valid matched names.
- Resolves tied fuzzy genus candidates with exact species evidence when
  unique, and reports the decision in the new `match_ambiguity` output
  column.

## wcvpmatch 0.0.2

CRAN release: 2026-09-09

## wcvpmatch 0.0.1

CRAN release: 2026-03-23

- Initial release to CRAN.
- Standardizes and reconciles scientific plant names against the World
  Checklist of Vascular Plants (WCVP).
- Implements staged exact and fuzzy matching at genus, species, and
  infraspecies levels.
- Supports robust scientific name parsing through
  [`classify_spnames()`](https://paulesantos.github.io/wcvpmatch/reference/classify_spnames.md).
- Provides traceable results with original and matched components.
