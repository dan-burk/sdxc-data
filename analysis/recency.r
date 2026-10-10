# Should recent races count more? Adjusted time with each runner's races
# weighted by age: a race `half_life` weeks old counts half as much as one this
# week. Inf = every race counts the same (the pipeline today).
# Same week-ahead backtest and test sets as old_vs_new.r.
#
# Run from the repo root: Rscript analysis/recency.r

suppressPackageStartupMessages({ library(dplyr); library(tidyr); library(ggplot2) })
library(stringr)
options(width = 200)
source("R/load.r")
source("R/score.r")
source("analysis/ratings.r")
source("analysis/plot_theme.r")

half_lives <- c(1, 1.5, 2, 3, 4, 6, 8, Inf)
out <- file.path("analysis", "output", "recency")
dir.create(out, recursive = TRUE, showWarnings = FALSE)

results <- list()
for (year in c(2025, 2026)) {
  season <- load_season(year)
  for (g in c("boys", "girls")) {
    races  <- season_races(season, g)
    scores <- bind_rows(lapply(half_lives, function(h) adjusted_recency_scores(races, h)))
    results[[paste(year, g)]] <- backtest(races, scores) %>% mutate(year = year)
  }
}
mae <- bind_rows(results) %>%
  mutate(test = test_set(year, week), half_life = as.numeric(str_remove(method, "half-life "))) %>%
  group_by(test, half_life) %>%
  summarise(n = n(), mae = mean(abs(error)), .groups = "drop") %>%
  group_by(test) %>%
  mutate(vs_equal = sprintf("%+.1f%%", 100 * (mae / mae[is.infinite(half_life)] - 1))) %>%
  ungroup()
write.csv(mae, file.path(out, "mae_by_half_life.csv"), row.names = FALSE)
print(mae %>% mutate(mae = round(mae, 2)) %>% select(-n) %>%
        pivot_wider(names_from = test, values_from = c(mae, vs_equal)), n = Inf)

labels <- ifelse(is.infinite(half_lives), "Equal\n(today)", half_lives)
p <- ggplot(mae %>% mutate(x = factor(half_life, levels = half_lives, labels = labels)), aes(x, mae, group = 1)) +
  geom_line(colour = aqua, linewidth = 0.9) +
  geom_point(colour = aqua, size = 2.4) +
  facet_wrap(~ test, scales = "free_y") +
  labs(title = "Should recent races count more?",
       subtitle = "Half-life: how many weeks old a race is when it counts half as much. Lower MAE is better.",
       x = "Half-life (weeks)", y = "Mean absolute error (places)") +
  theme_sdxc
save_fig(p, "14_recency.png", w = 11, h = 4.5)
