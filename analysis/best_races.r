# Does rating runners on their best few adjusted races (instead of all of
# them) predict finish places better? A runner who wins easily or has an off
# day keeps those slow times in the plain average.
#
#   best N       average of the runner's N fastest adjusted races
#   drop worst   average of all but the slowest (runners with 3+ races)
#   adjusted     the pipeline's plain average (all races)
#
# Run from the repo root: Rscript analysis/best_races.r   (about a minute)
# Table: analysis/output/best_races/mae.csv

suppressPackageStartupMessages({ library(dplyr); library(tidyr) })
library(stringr)
options(width = 200)
source("R/load.r")
source("R/score.r")
source("analysis/ratings.r")

out <- file.path("analysis", "output", "best_races")
dir.create(out, recursive = TRUE, showWarnings = FALSE)

all_scores <- function(races) {
  bind_rows(
    pr_scores(races) %>% mutate(method = "pr"),
    adjusted_scores(races),
    bind_rows(lapply(1:4, function(n) adjusted_best_scores(races, n) %>% mutate(method = paste("best", n)))),
    adjusted_best_scores(races, drop_worst = TRUE) %>% mutate(method = "drop worst"))
}

results <- list()
for (year in c(2025, 2026)) {
  season <- load_season(year)
  for (g in c("boys", "girls")) {
    races <- season_races(season, g)
    results[[paste(year, g)]] <- backtest(races, all_scores(races)) %>% mutate(year = year, gender = g)
  }
}
mae <- bind_rows(results) %>%
  mutate(test = test_set(year, week)) %>%
  group_by(test, method) %>%
  summarise(n = n(), mae = mean(abs(error)), .groups = "drop")
write.csv(mae, file.path(out, "mae.csv"), row.names = FALSE)

cat("MAE (places), worst to best on 2025 weeks 2-7\n")
print(as.data.frame(mae %>% select(test, method, mae) %>%
                      pivot_wider(names_from = test, values_from = mae) %>%
                      arrange(desc(`2025 weeks 2-7`))), digits = 3, row.names = FALSE)

# Each method vs the plain average on the same runners in the same races:
# mean difference in |error| (negative = better), with a standard error that
# treats races as the independent units (runners in one race aren't).
paired <- bind_rows(results) %>%
  mutate(test = test_set(year, week), abs_error = abs(error)) %>%
  select(test, year, gender, meet, id, method, abs_error) %>%
  pivot_wider(names_from = method, values_from = abs_error) %>%
  pivot_longer(-c(test, year, gender, meet, id, adjusted), names_to = "method", values_to = "abs_error") %>%
  mutate(diff = abs_error - adjusted) %>%
  group_by(test, method, year, gender, meet) %>%
  summarise(diff = sum(diff), n = n(), .groups = "drop") %>%
  group_by(test, method) %>%
  summarise(races = n(),
            change = sum(diff) / sum(n),
            se     = sqrt(races / (races - 1) * sum((diff - change * n)^2)) / sum(n),
            .groups = "drop") %>%
  mutate(t = change / se) %>%
  arrange(test, change)
cat("\nChange in MAE vs the plain average (negative = better), SE clustered by race\n")
print(as.data.frame(paired), digits = 2, row.names = FALSE)
write.csv(paired, file.path(out, "paired_vs_adjusted.csv"), row.names = FALSE)

# ---- By ability: is drop worst better for slower runners? -------------------
# Decile = the runner's statewide adjusted-time rating going in, among everyone
# racing that week (1 = fastest 10%). Not the actual finish, which a bad day
# would move.

by_ability <- bind_rows(results) %>%
  mutate(test = test_set(year, week)) %>%
  filter(method %in% c("adjusted", "drop worst")) %>%
  select(test, year, gender, week, meet, id, method, score, error) %>%
  pivot_wider(names_from = method, values_from = c(score, error)) %>%
  group_by(year, gender, week) %>%
  mutate(decile = ceiling(10 * rank(-score_adjusted) / n())) %>%
  ungroup()

decile_table <- function(d) {
  d %>%
    mutate(diff = abs(`error_drop worst`) - abs(error_adjusted)) %>%
    group_by(decile, year, gender, meet) %>%
    summarise(diff = sum(diff), n = n(), mae = sum(abs(error_adjusted)), .groups = "drop") %>%
    group_by(decile) %>%
    summarise(runs = sum(n), races = n(),
              mae_adjusted = sum(mae) / runs,
              change = sum(diff) / runs,
              se = sqrt(races / (races - 1) * sum((diff - change * n)^2)) / runs,
              .groups = "drop") %>%
    mutate(t = change / se)
}
cat("\nDrop worst vs plain average by ability decile, all test sets pooled (negative = drop worst better)\n")
print(as.data.frame(decile_table(by_ability)), digits = 2, row.names = FALSE)
cat("\nState meet only\n")
print(as.data.frame(decile_table(by_ability %>% filter(test == "2025 state meet"))), digits = 2, row.names = FALSE)
write.csv(decile_table(by_ability), file.path(out, "by_ability_decile.csv"), row.names = FALSE)

# How much do runners vary race to race? SD of each runner's adjusted log
# time (x 100, about a percent), runners with 3+ races, by end-of-season decile.
spread <- bind_rows(lapply(c(2025, 2026), function(year) {
  season <- load_season(year)
  bind_rows(lapply(c("boys", "girls"), function(g) {
    d <- season_races(season, g) %>% filter(!startsWith(meet, "sdhsaa_"))
    d %>%
      mutate(adjusted = log(time_sec) - meet_effects(d)[meet]) %>%
      group_by(id) %>%
      filter(n() >= 3) %>%
      summarise(rating = mean(adjusted), sd = 100 * sd(adjusted),
                gap_worst = 100 * (max(adjusted) - mean(adjusted)), .groups = "drop") %>%
      mutate(decile = ceiling(10 * rank(rating) / n()), year = year, gender = g)
  }))
}))
cat("\nRace-to-race spread by ability decile (runners with 3+ races; % of time)\n")
print(as.data.frame(spread %>% group_by(decile) %>%
                      summarise(runners = n(), median_sd = median(sd), median_worst_gap = median(gap_worst)),
                    .groups = "drop"), digits = 2, row.names = FALSE)
