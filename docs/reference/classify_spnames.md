# Classify Scientific Plant Names into Taxonomic Components

**\[experimental\]**

Parse and classify scientific plant names into taxonomic components:
genus, specific epithet, infraspecific rank, infraspecific epithet, and
author.

Output is aligned to a backbone convention:

- `Orig.Genus` in Title Case (first letter uppercase, rest lowercase).

- `Orig.Species` and `Orig.Infraspecies` epithets in lowercase.

- `Infra.Rank` in lowercase (`subsp.`, `var.`, `subvar.`, `f.`,
  `subf.`).

- `Author` is recovered from the input and preserved in its original
  casing/punctuation (no forced uppercasing).

- `Orig.Name` is reconstructed as: genus + species + (rank + infra) +
  author.

Robustness rules:

- `cf.` / `aff.` are removed from parsing but preserved as flags
  (`has_cf`, `has_aff`).

- Hybrid markers (`x`/`\u00D7`) as standalone tokens are removed with
  `had_hybrid = TRUE`.

- `sp.` / `spp.` triggers genus-only classification (`Rank = 1`,
  `Orig.Species = NA`) and sets `is_sp`/`is_spp`.

- If an infraspecific rank is present but the infraspecific epithet is
  missing, sets `rank_missing_infra = TRUE` and keeps `Infra.Rank` while
  `Orig.Infraspecies = NA`.

- If rank appears "late" (after author-like tokens), parsing is
  best-effort and `rank_late = TRUE`.

- If there is no explicit rank and a third token exists, the function
  can infer an unranked infraspecific epithet when the third token looks
  epithet-like (all lowercase), and does not look like the start of an
  author. In that case `implied_infra = TRUE`, `Orig.Infraspecies` is
  filled, `Infra.Rank = NA`, and `Rank = 3`.

## Usage

``` r
classify_spnames(splist)
```

## Arguments

- splist:

  Character vector. Scientific plant names.

## Value

A tibble with one row per input name and standardized columns/flags:

- sorter:

  Numeric index of original order.

- Input.Name:

  Original input string as provided by user.

- Orig.Name:

  Reconstructed standardized name aligned to backbone + original-cased
  author.

- Orig.Genus:

  Genus in Title Case.

- Orig.Species:

  Specific epithet in lowercase, or `NA` for genus-only (`sp./spp.`).

- Author:

  Recovered author string (original casing/punctuation) or `""`.

- Orig.Infraspecies:

  Infraspecific epithet in lowercase (ranked or implied), or `NA`.

- Infra.Rank:

  Infraspecific rank in lowercase (`subsp.`, `var.`, `subvar.`, `f.`,
  `subf.`), or `NA`.

- Rank:

  Numeric level: `1` genus-only, `2` genus+species, `3` includes
  infraspecific epithet.

- has_cf,has_aff,is_sp,is_spp,had_hybrid,rank_late,rank_missing_infra,had_na_author,implied_infra:

  Logical flags.

## Examples

``` r
library(wcvpmatch)
classify_spnames(c("Opuntia sp.", "Rosa canina subsp. coriifolia (Fr.) Leffler"))
#> Warning: Undetermined species indicator detected ('sp.'/'spp.'). Classified at genus
#> level only; Orig.Species set to NA for
#> • Opuntia sp.
#> # A tibble: 2 × 18
#>   sorter Input.Name   Orig.Name Orig.Genus Orig.Species Author Orig.Infraspecies
#>    <dbl> <chr>        <chr>     <chr>      <chr>        <chr>  <chr>            
#> 1      1 Opuntia sp.  Opuntia   Opuntia    NA           ""     NA               
#> 2      2 Rosa canina… Rosa can… Rosa       canina       "(Fr.… coriifolia       
#> # ℹ 11 more variables: Infra.Rank <chr>, Rank <dbl>, has_cf <lgl>,
#> #   has_aff <lgl>, is_sp <lgl>, is_spp <lgl>, had_hybrid <lgl>,
#> #   rank_late <lgl>, rank_missing_infra <lgl>, had_na_author <lgl>,
#> #   implied_infra <lgl>
classify_spnames(c("Cydonia japonica tricolor")) # implied unranked infra epithet
#> # A tibble: 1 × 18
#>   sorter Input.Name   Orig.Name Orig.Genus Orig.Species Author Orig.Infraspecies
#>    <dbl> <chr>        <chr>     <chr>      <chr>        <chr>  <chr>            
#> 1      1 Cydonia jap… Cydonia … Cydonia    japonica     ""     tricolor         
#> # ℹ 11 more variables: Infra.Rank <chr>, Rank <dbl>, has_cf <lgl>,
#> #   has_aff <lgl>, is_sp <lgl>, is_spp <lgl>, had_hybrid <lgl>,
#> #   rank_late <lgl>, rank_missing_infra <lgl>, had_na_author <lgl>,
#> #   implied_infra <lgl>
```
