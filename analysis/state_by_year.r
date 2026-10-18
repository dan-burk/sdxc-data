# State meet prediction error by year: PR alone, course-adjusted time, and
# the published rank (adjusted time + head-to-head swaps).
#
# Each state race is predicted from the ratings after the week before state.
# Same rules as old_vs_new.r: only runners every method can rate (ran a 5K
# before), places re-numbered 1..n in each race. MAE = mean |error| in places.
#
# 2023 and 2024 were never used to choose the method, and neither was 2025 state.
#
# Run from the repo root: Rscript analysis/state_by_year.r
# Table: analysis/output/state_by_year.csv

suppressPackageStartupMessages({ library(dplyr); library(tidyr) })
library(stringr)
source("R/load.r")
source("R/score.r")
source("analysis/ratings.r")

results <- list()
for (year in c(2023, 2024, 2025)) {
  season <- load_season(year)
  for (g in c("boys", "girls")) {
    st <- state_scores(season, g)
    results[[paste(year, g)]] <- backtest(st$races, st$scores) %>%
      filter(week == st$state_week) %>%
      mutate(year = year, gender = g)
  }
}
results <- bind_rows(results)

table <- results %>%
  group_by(year, gender, method) %>%
  summarise(mae = mean(abs(error)), runners = n(), .groups = "drop") %>%
  pivot_wider(names_from = method, values_from = mae) %>%
  mutate(improvement = 1 - `Adj + swap` / `PR alone`) %>%
  select(gender, year, runners, `PR alone`, `Adjusted time`, `Adj + swap`, improvement) %>%
  arrange(gender, year)

dir.create(file.path("analysis", "output"), showWarnings = FALSE)
write.csv(table, file.path("analysis", "output", "state_by_year.csv"), row.names = FALSE)
print(as.data.frame(table), digits = 3)
