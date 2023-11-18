library(ready4)
# Get current table of abbreviations
abbreviations_lup <- get_abbrs(gh_repo_1L_chr = "ready4-dev/ready4")
abbreviations_lup %>% head()
# Find abbreviations for a word
get_abbrs("template", abbreviations_lup = abbreviations_lup)
# Find possible whole word meanings associated with an abbreviation
get_abbrs("org", type_1L_chr = "extension", abbreviations_lup = abbreviations_lup)


