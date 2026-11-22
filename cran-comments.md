## Submission summary

This is an update of `wcvpmatch` from CRAN version 0.0.2 to 0.0.3.

This release improves the performance of matching against the WCVP backbone.
For the versioned default backbone, it loads only candidate genera and retrieves
accepted-name metadata by identifier.
It reuses prepared lookup tables, bounds the size of fuzzy-matching batches,
resolves taxonomic context once per distinct taxon key and submitted name, and
avoids repeated input normalization inside the matching pipeline. When the
optional `wcvpdata` package provides matching schema version 1, the package
also skips redundant normalization of that backbone. User-supplied backbones
remain supported without `wcvpdata`.

## Test environments

* Local Windows 11 x64, R 4.6.1.

## R CMD check results

* `devtools::check(cran = TRUE, manual = TRUE)` on version 0.0.3:
  0 errors, 0 warnings, 0 notes.
* `devtools::test()`: all tests pass with no failures, warnings or skips.

The optional `wcvpdata` package is available through the additional repository
listed in DESCRIPTION. Tests using the default backbone require it; tests with
user-supplied backbones do not.
