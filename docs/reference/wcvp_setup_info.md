# Check Default Backbone Setup

Reports whether the optional companion package `wcvpdata` is available
for use as the default WCVP backbone and, if not, explains how to
install it from `r-universe`.

## Usage

``` r
wcvp_setup_info(inform = TRUE)
```

## Arguments

- inform:

  Logical. If `TRUE` (default), print a short setup message.

## Value

Invisibly returns a named list with setup status fields:
`default_backbone_available`, `wcvpdata_installed`,
`wcvpdata_has_backbone`, `wcvpdata_version`, `repository`, and
`install_command`.

## Examples

``` r
library(wcvpmatch)
wcvp_setup_info()
#> Warning: ! wcvpdata is installed, but the expected object "wcvp_checklist_names" was not
#>   found.
#> ℹ Reinstall wcvpdata from <https://paulesantos.r-universe.dev>.
#> ℹ Or pass a backbone explicitly with `target_df`.
```
