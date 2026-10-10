# How course-adjusted time was chosen: every method tried, tuned on 2025 weeks 2-7.
#
#   wager X%   the 2025 points: winners take X% of the points of everyone behind
#   elo K = X  Elo: a rating moves by how much better or worse than expected you placed
#   pr         fastest 5K so far
#   adjusted   course-adjusted time (the pipeline's method)
#   + pr       blended with the PR rank, the way the 2025 rank blended points and PR
#
# Then: does blending anything into adjusted time help? (It doesn't.)
#
# Run from the repo root: Rscript analysis/method_search.r   (a few minutes)
# Tables: analysis/output/method_search/. Figures: analysis/figures/.

suppressPackageStartupMessages({ library(dplyr); library(tidyr); library(ggplot2) })
library(stringr)
options(width = 200)
source("R/load.r")
source("R/score.r")
source("analysis/ratings.r")
source("analysis/plot_theme.r")

wagers  <- c(0.005, 0.01, 0.02, 0.03, 0.05, 0.075, 0.10, 0.15, 0.20)
Ks      <- c(16, 32, 64, 128, 256, 512, 768, 1024)
weights <- seq(0, 1, by = 0.1)  # share of adjusted time in a blend

out <- file.path("analysis", "output", "method_search")
dir.create(out, recursive = TRUE, showWarnings = FALSE)

all_scores <- function(races) {
  scores <- bind_rows(
    pr_scores(races) %>% mutate(method = "pr"),
    adjusted_scores(races),
    bind_rows(lapply(wagers, function(w) replay(races, wager_update(w), 1000) %>% mutate(method = sprintf("wager %g%%", 100 * w)))),
    bind_rows(lapply(Ks, function(K) replay(races, elo_update(K), 1500) %>% mutate(method = paste("elo K =", K)))))
  pr_blends <- lapply(setdiff(unique(scores$method), "pr"), function(m) blend(scores, m, "pr", paste(m, "+ pr")))
  adjusted_blends <- lapply(weights, function(w) bind_rows(
    blend(scores, "adjusted", "elo K = 768", paste("adjusted", w, "with elo"), weight = w),
    blend(scores, "adjusted", "pr",          paste("adjusted", w, "with pr"),  weight = w)))
  bind_rows(scores, pr_blends, adjusted_blends)
}

results <- list()
for (year in c(2025, 2026)) {
  season <- load_season(year)
  for (g in c("boys", "girls")) {
    races <- season_races(season, g)
    results[[paste(year, g)]] <- backtest(races, all_scores(races)) %>% mutate(year = year)
    cat("Scored", year, g, "\n")
  }
}
mae <- bind_rows(results) %>%
  mutate(test = test_set(year, week)) %>%
  group_by(test, method) %>%
  summarise(n = n(), mae = mean(abs(error)), .groups = "drop")
write.csv(mae, file.path(out, "mae_all_methods.csv"), row.names = FALSE)

# ---- 1. Tuning the points methods ------------------------------------------

tuning <- mae %>% filter(test == "2025 weeks 2-7")
sweep <- tuning %>%
  filter(str_detect(method, "^(wager|elo)"), !str_detect(method, "^adjusted")) %>%
  mutate(x       = as.numeric(str_extract(method, "[0-9.]+")),
         panel   = if_else(str_starts(method, "wager"), "Points: wager (% of each loser's points)", "Elo: K"),
         version = if_else(str_ends(method, "\\+ pr"), "Blended with PR", "Alone"))
cat("\nBest of each on 2025 weeks 2-7\n")
print(sweep %>% group_by(panel, version) %>% slice_min(mae, n = 1) %>% ungroup() %>% select(method, mae))
p <- ggplot(sweep, aes(x, mae, colour = version)) +
  geom_hline(yintercept = tuning$mae[tuning$method == "pr"], colour = blue, linetype = "dashed", linewidth = 0.5) +
  geom_hline(yintercept = tuning$mae[tuning$method == "adjusted"], colour = aqua, linetype = "dashed", linewidth = 0.5) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.2) +
  geom_point(data = sweep %>% filter(method %in% c("wager 5%", "wager 5% + pr")), shape = 21, size = 5,
             stroke = 1, fill = NA, show.legend = FALSE) +
  facet_wrap(~ panel, scales = "free_x") +
  scale_x_log10() +
  scale_colour_manual(values = c("Alone" = ink_2, "Blended with PR" = orange), name = NULL) +
  labs(title = "No setting of points or Elo catches adjusted time",
       subtitle = "2025 weeks 2-7. Dashed: PR alone (blue), adjusted time (green). Circled: the 2025 method (5%).",
       x = NULL, y = "Mean absolute error (places)") +
  theme_sdxc
save_fig(p, "10_search_points_and_elo.png")

# ---- 2. Everything, on every test set ----------------------------------------

finalists <- c("pr", "wager 5%", "wager 5% + pr", "wager 2% + pr", "elo K = 1024", "elo K = 768 + pr", "adjusted")
names_nice <- c("pr" = "PR alone", "wager 5%" = "Points 5% alone", "wager 5% + pr" = "Points 5% + PR (2025)",
                "wager 2% + pr" = "Points 2% + PR", "elo K = 1024" = "Elo alone (K = 1024)",
                "elo K = 768 + pr" = "Elo + PR (K = 768)", "adjusted" = "Adjusted time (new)")
worst_first <- tuning %>% filter(method %in% finalists) %>% arrange(desc(mae)) %>% pull(method)
compare <- mae %>% filter(method %in% finalists) %>%
  mutate(method = factor(names_nice[method], levels = names_nice[worst_first]),
         highlight = case_when(method == "Adjusted time (new)" ~ "new", method == "Points 5% + PR (2025)" ~ "old", TRUE ~ "other"))
p <- ggplot(compare, aes(mae, method, colour = highlight)) +
  geom_point(size = 3) +
  facet_wrap(~ test, scales = "free_x") +
  scale_colour_manual(values = c(new = aqua, old = orange, other = ink_2), guide = "none") +
  labs(title = "Every method tried, on every test set",
       subtitle = "Best setting of each, chosen on 2025 weeks 2-7. Lower is better.",
       x = "Mean absolute error (places)", y = NULL) +
  theme_sdxc
save_fig(p, "11_search_all_methods.png", w = 11, h = 4.5)

# ---- 3. Does anything add to adjusted time? --------------------------------

blends <- mae %>% filter(str_detect(method, "^adjusted [0-9.]+ with")) %>%
  mutate(weight  = as.numeric(str_extract(method, "[0-9.]+")),
         partner = if_else(str_ends(method, "elo"), "Elo (K = 768)", "PR"))
cat("\nBest blend weight on adjusted time\n")
print(blends %>% group_by(test, partner) %>% slice_min(mae, n = 1) %>% ungroup() %>% select(test, partner, weight, mae))
p <- ggplot(blends, aes(weight, mae, colour = partner)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.2) +
  facet_wrap(~ test, scales = "free_y") +
  scale_x_continuous(labels = scales::percent, breaks = c(0, 0.5, 1)) +
  scale_colour_manual(values = c("Elo (K = 768)" = ink_2, "PR" = blue), name = "Blended with") +
  labs(title = "Blending Elo or PR into adjusted time doesn't help",
       subtitle = "Weighted rank blend. 100% = adjusted time alone. Lower is better.",
       x = "Weight on adjusted time", y = "Mean absolute error (places)") +
  theme_sdxc
save_fig(p, "12_search_blends.png", w = 11, h = 4.5)
