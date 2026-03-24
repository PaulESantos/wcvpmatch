# Cleaned Master Tree Species List from FIA

A cleaned dataset containing tree species recorded by the Forest
Inventory and Analysis (FIA) program of the U.S. Forest Service. This
dataset is used in the examples and README of the `wcvpmatch` package.
The data was downloaded in November 2022 from the official webpage of
the Forest Inventory and Analysis National Program, available at the
following
[link](https://research.fs.usda.gov/products/dataandtools/fia-datamart),
and was originally used during the development of the `treemendous`
package. For `wcvpmatch`, the variable names have been standardized to
`Orig.Genus` and `Orig.Species`.

## Usage

``` r
fia
```

## Format

A data frame with 2169 rows and 2 variables:

- Orig.Genus:

  Genus name of the species binomial

- Orig.Species:

  Specific epithet of the species binomial
