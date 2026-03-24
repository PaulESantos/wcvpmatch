# Direct Match Species within Genus

**\[experimental\]**

Tries to directly match the specific epithet within an already matched
genus in 'WCVP'.

## Usage

``` r
wcvp_direct_match_species_within_genus(df, target_df = NULL)
```

## Arguments

- df:

  `tibble` containing the species binomial split into the columns
  `Orig.Genus` and `Orig.Species`.

- target_df:

  Optional custom target table. If `NULL`, the optional `wcvpdata`
  checklist is used when available; otherwise pass a backbone
  explicitly.

## Value

Returns a `tibble` with the additional logical column
`direct_match_species_within_genus`, indicating whether the specific
epithet was successfully matched within the matched genus (`TRUE`) or
not (`FALSE`).
