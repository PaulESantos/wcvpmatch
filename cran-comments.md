## Submission Summary

This is the initial submission of 'wcvpmatch', an R package for scientific plant name reconciliation against the World Checklist of Vascular Plants (WCVP).

Since the previous draft, the package was adjusted to better align with CRAN expectations:
* `wcvpdata` is used by `wcvpmatch` as the default WCVP backbone source during matching workflows.
* Startup messaging and GitHub installation instructions were removed from package attachment.
* Session-wide side effects from `options(warn = 1)` were fixed by restoring user options on exit.
* Documentation was updated to describe `target_df` as the primary explicit backbone input.
* The package license was migrated to MIT.

## Test Environments
* Local Windows 11, R 4.5.3

## R CMD build / check status

* `R CMD build` succeeds locally.
* `devtools::check()` completed successfully with `0 errors | 0 warnings | 0 notes`.
* Check duration on the local machine was 5m 11.8s.

## Method References

There are no published references describing the methods in this package.
The package implements original functionality for taxonomic name reconciliation based on the matching logic used in the 'treemendous' package, but specifically tailored for the WCVP backbone.
