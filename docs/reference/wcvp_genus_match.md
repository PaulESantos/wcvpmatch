# Match Genus name

**\[experimental\]**

Tries to match the genus name to the 'WCVP' table (using the optional
`wcvpdata` checklist by default when available).

## Usage

``` r
wcvp_genus_match(df, target_df = NULL)
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

Returns a `tibble` with the additional logical column `genus_match`,
indicating whether the genus was successfully matched (`TRUE`) or not
(`FALSE`).
