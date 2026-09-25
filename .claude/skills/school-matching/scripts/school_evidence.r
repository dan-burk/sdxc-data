# Evidence for every school name in a season that isn't yet known.
# Usage (from repo root): Rscript .claude/skills/school-matching/scripts/school_evidence.r 2025
#
# For each unknown raw school name prints:
#   runners, grades, meets      where and how often it shows up
#   shared_with                 known in-state school(s) whose runners have the same
#                               names, in ANY year's data (strongest evidence of a spelling variant)
#   similar_known               known school names with a similar spelling
#   pct_out_at_meets            share of OUT runners at the meets it ran (high = out-of-state meet)

suppressPackageStartupMessages(library(dplyr))
library(stringr)
options(width = 250)
source("R/load.r")

year <- commandArgs(trailingOnly = TRUE)[1]
if (is.na(year)) year <- "2025"
season <- load_season(year)

schools <- season$schools
aliases <- season$school_aliases
standardize <- function(raw) coalesce(aliases$school[match(raw, aliases$raw_school)], raw)

unknown <- season$races %>% filter(!school %in% c(schools$school, "OUT", "DROP"))
if (nrow(unknown) == 0) {
  cat("No unknown schools in", year, "\n")
  quit(save = "no")
}

# Every year's results, any column layout, with standardized school names.
read_any <- function(path) {
  r <- read.csv(path, colClasses = "character", check.names = FALSE)
  names(r) <- tolower(names(r))
  if (!all(c("name", "school") %in% names(r))) return(NULL)
  tibble(name = toupper(str_squish(r$name)), school = standardize(str_squish(r$school)))
}
all_years <- list.files(".", "_(boys|girls)\\.csv$", recursive = TRUE, full.names = TRUE) %>%
  str_subset("/Data/") %>%
  lapply(read_any) %>%
  bind_rows() %>%
  filter(school %in% schools$school) %>%
  distinct()

shared <- unknown %>%
  distinct(school, name) %>%
  inner_join(all_years, by = "name", suffix = c("", "_known"), relationship = "many-to-many") %>%
  count(school, school_known) %>%
  arrange(school, desc(n)) %>%
  group_by(school) %>%
  summarise(shared_with = paste0(school_known, " (", n, ")", collapse = "; "))

known_names <- c(schools$school, aliases$raw_school[aliases$school %in% schools$school])
# Close spellings, plus known names sharing a word of 4+ letters ("Winner" -> "Winner/Colome").
words <- function(x) setdiff(str_split(tolower(x), "[^a-z]+")[[1]], c("", "area", "school", "high"))
similar <- function(x) {
  d <- adist(tolower(x), tolower(known_names))[1, ]
  close <- known_names[d <= max(3, nchar(x) / 3)]
  w <- words(x)[nchar(words(x)) >= 4]
  share <- known_names[sapply(known_names, function(k) any(w %in% words(k)))]
  paste(head(unique(standardize(c(close, share))), 5), collapse = "; ")
}

pct_out <- season$races %>%
  group_by(meet) %>%
  summarise(pct_out = round(100 * mean(school == "OUT")))

unknown %>%
  left_join(pct_out, by = "meet") %>%
  group_by(raw_school = school) %>%
  summarise(
    runners          = n(),
    grades           = paste(sort(unique(grade)), collapse = ","),
    meets            = paste(head(unique(meet), 4), collapse = ";"),
    pct_out_at_meets = round(mean(pct_out))
  ) %>%
  left_join(shared, by = c("raw_school" = "school")) %>%
  mutate(similar_known = sapply(raw_school, similar)) %>%
  as.data.frame() %>%
  print(right = FALSE, row.names = FALSE)
