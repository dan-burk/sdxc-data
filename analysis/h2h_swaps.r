# Adjusted time, then head-to-head swaps within each class (h2h_scores() in
# ratings.r): a runner who has beaten someone up to 5 places above them in
# every one of their 2+ races together moves ahead of them.
#
# Two checks:
#   1. Week-ahead MAE, as in the other scripts (few runners move, so the
#      overall change is small; also shown for just the runners who moved).
#   2. The swapped pairs themselves: when they race each other the next week,
#      does the head-to-head winner finish ahead?
#
# Run from the repo root: Rscript analysis/h2h_swaps.r
# Tables: analysis/output/h2h_swaps/

suppressPackageStartupMessages({ library(dplyr); library(tidyr) })
library(stringr)
options(width = 200)
source("R/load.r")
source("R/score.r")
source("analysis/ratings.r")

out <- file.path("analysis", "output", "h2h_swaps")
dir.create(out, recursive = TRUE, showWarnings = FALSE)

settings <- tibble(window = c(5, 3, 10, 5), min_races = c(2, 2, 2, 3)) %>%
  mutate(method = sprintf("h2h window %d, %d+ races", window, min_races))

results <- list()
pairs   <- list()
moved   <- list()
for (year in c(2025, 2026)) {
  season  <- load_season(year)
  classes <- season$schools %>% select(school, class)
  for (g in c("boys", "girls")) {
    races <- season_races(season, g)
    adjusted <- adjusted_scores(races)
    swaps <- lapply(seq_len(nrow(settings)), function(k)
      h2h_scores(races, classes, settings$window[k], settings$min_races[k]) %>% mutate(method = settings$method[k]))
    scores <- bind_rows(pr_scores(races) %>% mutate(method = "pr"), adjusted,
                        bind_rows(swaps) %>% select(week, id, method, score))
    results[[paste(year, g)]] <- backtest(races, scores) %>% mutate(year = year, gender = g)

    # Pairs the main setting flipped: b was ahead on adjusted time, a is ahead now
    main <- swaps[[1]] %>% select(week, id, score_h2h = score)
    both <- adjusted %>% select(week, id, score_adj = score) %>% inner_join(main, by = c("week", "id")) %>%
      mutate(school = sub(".*\\|", "", id)) %>% left_join(classes, by = "school")
    moved[[paste(year, g)]] <- both %>% filter(score_adj != score_h2h) %>% transmute(year, week, id)
    flipped <- both %>%
      inner_join(both, by = c("week", "class"), suffix = c("_a", "_b"), relationship = "many-to-many") %>%
      filter(score_h2h_a > score_h2h_b, score_adj_a < score_adj_b) %>%
      select(week, class, a = id_a, b = id_b)
    # Their next meetings: races in the following week with both in them
    nxt <- races %>% select(week, meet, id, row)
    pairs[[paste(year, g)]] <- flipped %>%
      mutate(week = week + 1) %>%
      inner_join(nxt, by = c("week", "a" = "id"), relationship = "many-to-many") %>%
      inner_join(nxt, by = c("week", "meet", "b" = "id"), suffix = c("_a", "_b")) %>%
      mutate(year = year, gender = g, a_ahead = row_a < row_b)
    cat("Scored", year, g, "\n")
  }
}

all <- bind_rows(results) %>% mutate(test = test_set(year, week))

# ---- 1. MAE -------------------------------------------------------------------

mae <- all %>% group_by(test, method) %>% summarise(n = n(), mae = mean(abs(error)), .groups = "drop")
write.csv(mae, file.path(out, "mae.csv"), row.names = FALSE)
cat("\nMAE (places)\n")
print(as.data.frame(mae %>% select(test, method, mae) %>% pivot_wider(names_from = test, values_from = mae)),
      digits = 3, row.names = FALSE)

# Just the runners the main setting moved (the rating used for week w + 1)
moved <- bind_rows(moved) %>% mutate(week = week + 1)
mae_moved <- all %>%
  semi_join(moved, by = c("year", "week", "id")) %>%
  filter(method %in% c("adjusted", settings$method[1])) %>%
  group_by(test, method) %>%
  summarise(runs = n(), mae = mean(abs(error)), .groups = "drop")
cat("\nMAE for runners the swaps moved (window 5, 2+ races)\n")
print(as.data.frame(mae_moved %>% pivot_wider(names_from = method, values_from = mae)), digits = 3, row.names = FALSE)

# ---- 2. The swapped pairs -------------------------------------------------------

pairs <- bind_rows(pairs) %>% mutate(test = test_set(year, week))
write.csv(pairs, file.path(out, "swapped_pairs_next_week.csv"), row.names = FALSE)
cat("\nSwapped pairs that met the next week: how often the head-to-head winner finished ahead\n")
print(as.data.frame(pairs %>% group_by(test) %>%
                      summarise(meetings = n(), h2h_winner_ahead = mean(a_ahead),
                                se = sqrt(mean(a_ahead) * (1 - mean(a_ahead)) / n()), .groups = "drop")),
      digits = 3, row.names = FALSE)

cat("\nHow many runners moved per week (main setting)\n")
print(as.data.frame(moved %>% count(year, week, name = "runners_moved") %>%
                      pivot_wider(names_from = year, values_from = runners_moved)), row.names = FALSE)
