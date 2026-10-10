# Do runners get faster over the season? Raw 5K times by week, 2025 weeks 1-7.
#
# Each point is one runner's time that week (their average if they raced twice).
# Two views:
#   all runners       whoever raced that week
#   same runners      only runners who raced in both week 1 and week 7
# Raw times also carry course speed, so a trend here is improvement and course mixed.
#
# Run from the repo root: Rscript analysis/improvement.r

suppressPackageStartupMessages({ library(dplyr); library(tidyr); library(ggplot2) })
library(stringr)
source("R/load.r")
source("R/score.r")
source("analysis/ratings.r")
source("analysis/plot_theme.r")

season <- load_season(2025)
weekly <- bind_rows(lapply(c("boys", "girls"), function(g) season_races(season, g))) %>%
  filter(week <= 7, flg_5k == 1) %>%
  group_by(gender, id, week) %>%
  summarise(time_sec = mean(time_sec), .groups = "drop")

both_ends <- weekly %>% group_by(gender, id) %>% filter(any(week == 1), any(week == 7)) %>% ungroup()
views <- bind_rows(weekly %>% mutate(view = "All runners"),
                   both_ends %>% mutate(view = "Same runners (raced weeks 1 and 7)"))

medians <- views %>% group_by(view, gender, week) %>%
  summarise(runners = n(), median = median(time_sec), .groups = "drop") %>%
  group_by(view, gender) %>%
  mutate(change_vs_week1 = sprintf("%+.1f%%", 100 * (median / median[week == 1] - 1))) %>%
  ungroup()
print(medians %>% mutate(median = sec_to_time(median)), n = Inf)

p <- ggplot(views, aes(factor(week), time_sec / 60)) +
  geom_boxplot(outlier.shape = NA, fill = "#cde2fb", colour = blue, width = 0.6) +
  geom_line(data = medians, aes(factor(week), median / 60, group = 1), colour = orange, linewidth = 0.9) +
  geom_point(data = medians, aes(factor(week), median / 60), colour = orange, size = 2) +
  facet_grid(gender ~ view, scales = "free_y") +
  coord_cartesian(ylim = NULL) +
  scale_y_continuous(labels = function(m) sprintf("%d:%02d", floor(m), round((m %% 1) * 60))) +
  labs(title = "Raw 5K times by week, 2025",
       subtitle = "Boxes: middle 50% of runners. Orange: median. Raw times mix improvement with course speed.",
       x = "Week", y = "5K time") +
  theme_sdxc
save_fig(p, "13_raw_times_by_week.png", w = 10, h = 7)
