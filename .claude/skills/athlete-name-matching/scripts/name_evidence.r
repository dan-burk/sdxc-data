# Evidence for every "possible misspelled runner" pair in a season.
# Usage (from repo root): Rscript .claude/skills/athlete-name-matching/scripts/name_evidence.r 2025
#
# For each pair (same school + gender, names 1-2 letters apart) prints:
#   races_1 / races_2    how many races each spelling appears in
#   same_race            races where BOTH spellings ran -> two different people
#   grades_1 / grades_2  every grade listed for that spelling (one value = consistent)
#   best_1 / best_2      best time for each spelling (any distance, so only a rough guide)

suppressPackageStartupMessages(library(dplyr))
library(stringr)
options(width = 250)
source("R/load.r")
source("R/check.r")

year <- commandArgs(trailingOnly = TRUE)[1]
if (is.na(year)) year <- "2025"
season <- load_season(year)

in_state <- season$races %>% filter(school %in% season$schools$school)
pairs <- similar_names(in_state)
if (nrow(pairs) == 0) {
  cat("No similar-name pairs in", year, "\n")
  quit(save = "no")
}

profile <- function(g, s, n) {
  r <- in_state %>% filter(gender == g, school == s, name == n)
  list(races = nrow(r),
       grades = paste(sort(unique(r$grade)), collapse = ","),
       best  = if (all(is.na(r$time_sec))) NA else r$time[which.min(r$time_sec)],
       meets = r$meet)
}

rows <- lapply(seq_len(nrow(pairs)), function(k) {
  p <- pairs[k, ]
  a <- profile(p$gender, p$school, p$name_1)
  b <- profile(p$gender, p$school, p$name_2)
  both <- intersect(a$meets, b$meets)
  tibble(school = p$school,
         name_1 = p$name_1, races_1 = a$races, grades_1 = a$grades, best_1 = a$best,
         name_2 = p$name_2, races_2 = b$races, grades_2 = b$grades, best_2 = b$best,
         same_race = if (length(both)) paste(both, collapse = ";") else "")
})

bind_rows(rows) %>% as.data.frame() %>% print(right = FALSE, row.names = FALSE)
