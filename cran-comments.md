## Submission Summary

This is the initial submission of 'wcvpmatch', an R package for scientific plant name reconciliation against the World Checklist of Vascular Plants (WCVP).

The package was adjusted to better align with CRAN expectations:
* `wcvpdata` is listed in `Suggests` and is available from `r-universe` at `https://paulesantos.r-universe.dev` because the full WCVP backbone exceeds CRAN size limits.
* `wcvpmatch` accepts a user-supplied backbone through `target_df`, so package functionality is available without requiring `wcvpdata` at installation time.
* When `target_df` is `NULL`, `wcvpmatch` uses `wcvpdata` as the default backbone if it is installed; otherwise it fails with an informative message explaining how to install `wcvpdata` from `r-universe` or how to pass `target_df` explicitly.
* `fozziejoin` is available on CRAN. On Windows, users installing from source may also need a working Rust toolchain because `fozziejoin` uses Rust in the fuzzy-matching backend.
* Startup messaging and GitHub installation instructions were removed from package attachment.
* Session-wide side effects from `options(warn = 1)` were fixed by restoring user options on exit.
* Documentation was updated to describe `target_df` as the explicit backbone input and to document the `r-universe` installation path for `wcvpdata`.
* Tests that require the default external backbone are skipped when `wcvpdata` is not installed, while deterministic tests continue to run against local `target_df` fixtures.
* The package license was migrated to MIT.

## Test Environments
* Local Windows 11, R 4.5.3

## R CMD build / check status

* `R CMD build` succeeds locally.
* `devtools::check(cran = TRUE)` completed successfully with `0 errors | 0 warnings | 0 notes`.
* Local `R CMD check --as-cran` used R 4.5.3 on Windows 11 x64 and completed in 1m 42.5s.

## Method References

There are no published references describing the methods in this package.
The package implements original functionality for taxonomic name reconciliation based on the matching logic used in the 'treemendous' package, but specifically tailored for the WCVP backbone.
