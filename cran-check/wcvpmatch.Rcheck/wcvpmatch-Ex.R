pkgname <- "wcvpmatch"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
base::assign(".ExTimings", "wcvpmatch-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('wcvpmatch')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("build_genus_index")
### * build_genus_index

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: build_genus_index
### Title: Build a Genus Index for Fast Prefiltering
### Aliases: build_genus_index
### Keywords: internal

### ** Examples

## No test: 
target <- data.frame(genus = "Opuntia", species = "ficus-indica", plant_name_id = 1)
wcvpmatch:::build_genus_index(target)
## End(No test)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("build_genus_index", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("classify_spnames")
### * classify_spnames

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: classify_spnames
### Title: Classify Scientific Plant Names into Taxonomic Components
### Aliases: classify_spnames

### ** Examples

library(wcvpmatch)
classify_spnames(c("Opuntia sp.", "Rosa canina subsp. coriifolia (Fr.) Leffler"))
classify_spnames(c("Cydonia japonica tricolor")) # implied unranked infra epithet



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("classify_spnames", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("prefilter_target_by_genus")
### * prefilter_target_by_genus

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: prefilter_target_by_genus
### Title: Prefilter Target Backbone by Input Genera (Exact + Fuzzy)
### Aliases: prefilter_target_by_genus
### Keywords: internal

### ** Examples

## No test: 
df <- data.frame(Genus = "Opuntia", Species = "yanganucensis")
target <- data.frame(genus = "Opuntia", species = "yanganucensis", plant_name_id = 1)
wcvpmatch:::prefilter_target_by_genus(df, target_df = target)
## End(No test)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("prefilter_target_by_genus", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("wcvp_direct_match")
### * wcvp_direct_match

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: wcvp_direct_match
### Title: Direct Match Species & Genus Binomial or Trinomial names
### Aliases: wcvp_direct_match
### Keywords: internal

### ** Examples

## No test: 
df_parsed <- classify_spnames("Opuntia yanganucensis")
target <- data.frame(genus = "Opuntia", species = "yanganucensis", plant_name_id = 1)
wcvpmatch:::wcvp_direct_match(df_parsed, target_df = target)
## End(No test)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("wcvp_direct_match", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("wcvp_distribution")
### * wcvp_distribution

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: wcvp_distribution
### Title: Retrieve Tabular WCVP Distribution by Taxonomic Rank
### Aliases: wcvp_distribution

### ** Examples

## Don't show: 
if (rlang::is_installed("wcvpdata")) withAutoprint({ # examplesIf
## End(Don't show)
## No test: 
library(wcvpmatch)

wcvp_distribution("Opuntia ficus-indica", taxon_rank = "species")
wcvp_distribution("Opuntia", taxon_rank = "genus")
wcvp_distribution("Cactaceae", taxon_rank = "family")
# When `order` is present in a custom names table:
# \dontrun{wcvp_distribution("Caryophyllales", taxon_rank = "order", wcvp_names = custom_names)}
## End(No test)
## Don't show: 
}) # examplesIf
## End(Don't show)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("wcvp_distribution", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("wcvp_fuzzy_match_genus")
### * wcvp_fuzzy_match_genus

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: wcvp_fuzzy_match_genus
### Title: Fuzzy Match Genus Name
### Aliases: wcvp_fuzzy_match_genus
### Keywords: internal

### ** Examples

## No test: 
df <- data.frame(Orig.Genus = "Opuntiaa", Orig.Species = "yanganucensis")
target <- data.frame(genus = "Opuntia", species = "yanganucensis", plant_name_id = 1)
wcvpmatch:::wcvp_fuzzy_match_genus(df, target_df = target)
## End(No test)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("wcvp_fuzzy_match_genus", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("wcvp_matching")
### * wcvp_matching

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: wcvp_matching
### Title: Match Scientific Names Against WCVP
### Aliases: wcvp_matching

### ** Examples

## Don't show: 
if (rlang::is_installed("wcvpdata")) withAutoprint({ # examplesIf
## End(Don't show)
## No test: 
library(wcvpmatch)
# Match a single name
wcvp_matching(data.frame(Genus = "Opuntia", Species = "yanganucensis"))

# Match multiple names with snake_case output
names <- c("Aniba heterotepala", "Anthurium quipuscoae")
df <- classify_spnames(names)
wcvp_matching(df, output_name_style = "snake_case")

# Attach per-stage timings for profiling
out <- wcvp_matching(df, output_name_style = "snake_case", profile = TRUE)
attr(out, "timings")
## End(No test)
## Don't show: 
}) # examplesIf
## End(Don't show)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("wcvp_matching", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("wcvp_setup_info")
### * wcvp_setup_info

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: wcvp_setup_info
### Title: Check Default Backbone Setup
### Aliases: wcvp_setup_info

### ** Examples

library(wcvpmatch)
wcvp_setup_info()



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("wcvp_setup_info", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("wcvp_suffix_match_species_within_genus")
### * wcvp_suffix_match_species_within_genus

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: wcvp_suffix_match_species_within_genus
### Title: Suffix Match Species within Genus
### Aliases: wcvp_suffix_match_species_within_genus
### Keywords: internal

### ** Examples

## No test: 
df <- data.frame(Orig.Genus = "Opuntia", Orig.Species = "yanganucensa", Matched.Genus = "Opuntia")
target <- data.frame(genus = "Opuntia", species = "yanganucensis", plant_name_id = 1)
wcvpmatch:::wcvp_suffix_match_species_within_genus(df, target_df = target)
## End(No test)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("wcvp_suffix_match_species_within_genus", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("wcvp_synonyms")
### * wcvp_synonyms

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: wcvp_synonyms
### Title: Retrieve Synonyms Resolved Through the WCVP Backbone
### Aliases: wcvp_synonyms

### ** Examples

## Don't show: 
if (rlang::is_installed("wcvpdata")) withAutoprint({ # examplesIf
## End(Don't show)
## No test: 
wcvp_synonyms("Nopalea cochenillifera")
## End(No test)
## Don't show: 
}) # examplesIf
## End(Don't show)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("wcvp_synonyms", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
