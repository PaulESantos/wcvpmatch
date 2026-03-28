# Fuzzy Match Species within Genus

**\[stable\]**

Tries to fuzzy match the species epithet within a matched genus against
'WCVP' (using the optional `wcvpdata` checklist by default when
available).

## Usage

``` r
wcvp_fuzzy_match_species_within_genus(
  df,
  target_df = NULL,
  max_dist = 1,
  method = "osa"
)
```

## Arguments

- df:

  `tibble` containing the species binomial split into the columns
  `Orig.Genus` and `Orig.Species`.

- target_df:

  Optional custom target table. If `NULL`, the optional `wcvpdata`
  checklist is used when available; otherwise pass a backbone
  explicitly.

- max_dist:

  Maximum edit distance used for fuzzy species matching within genus.

- method:

  String distance method passed to `fozziejoin` (for example `"osa"`).

## Value

Returns a `tibble` with the additional logical column
`fuzzy_match_species_within_genus`, indicating whether the specific
epithet was successfully fuzzy matched within the matched genus (`TRUE`)
or not (`FALSE`).
