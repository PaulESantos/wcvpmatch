# wcvpmatch 0.0.3

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
