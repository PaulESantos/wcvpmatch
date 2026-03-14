## Submission Summary

This is the initial submission of 'wcvpmatch', an R package for scientific plant name reconciliation against the World Checklist of Vascular Plants (WCVP).

Since the previous draft, the package was adjusted to better align with CRAN expectations:
* `wcvpdata` was removed from declared package dependencies and is now treated as an optional companion package.
* Startup messaging and GitHub installation instructions were removed from package attachment.
* Session-wide side effects from `options(warn = 1)` were fixed by restoring user options on exit.
* Documentation was updated to describe `target_df` as the primary explicit backbone input.
* The package license was migrated to MIT.

## Test Environments
* Local Windows 11, R 4.5.2

## R CMD build / check status

* `R CMD build` succeeds locally.
* In this offline environment, `R CMD check --as-cran` could not be completed as a clean full check because required CRAN packages were not installed and repository access was unavailable.
* Before submission, this file should be updated one last time with the final clean `R CMD check --as-cran` results from a fully provisioned environment.

## Method References

There are no published references describing the methods in this package.
The package implements original functionality for taxonomic name reconciliation based on the matching logic used in the 'treemendous' package, but specifically tailored for the WCVP backbone.
