# Check Default Backbone Setup

**\[stable\]**

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

## Details

Reports whether the optional companion package `wcvpdata` is available
for use as the default WCVP backbone and, if not, explains how to
install it from `r-universe`.

## Examples

``` r
library(wcvpmatch)
wcvp_setup_info()
#> -- Default WCVP Backbone ------------------------------------ wcvpmatch 0.0.2 --
#> v wcvpdata   0.7.0        
#> v backbone   available
#> i repository https://paulesantos.r-universe.dev
#> i Functions can use the default backbone when `target_df = NULL`.
```
