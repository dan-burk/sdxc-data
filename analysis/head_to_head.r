# Head-to-head ratings: does Plackett-Luce (finish order only) beat or add to
# course-adjusted time?
#
#   pl sd = X           Plackett-Luce, prior mean 0, prior sd X (wins and losses only)
#   pl k = K, sd = X    Plackett-Luce, prior mean = K x adjusted-time z-score
#   adjusted W with pl  rank blend of adjusted time (weight W) and pl sd = 1
#
# Settings are chosen on 2025 weeks 2-7, as in method_search.r.
# Run from the repo root: Rscript analysis/head_to_head.r   (about 15 minutes)
# Tables: analysis/output/head_to_head/.

suppressPackageStartupMessages({ library(dplyr); library(tidyr) })
library(stringr)
options(width = 200)
source("R/load.r")
source("R/score.r")
source("analysis/ratings.r")

sds     <- c(0.5, 1, 2, 4)  # prior sd, wins and losses only
ks      <- c(1, 2, 4)       # weight of adjusted time in the prior mean
k_sds   <- c(0.5, 1, 2)     # prior sd with the adjusted-time prior
weights <- c(0.5, 0.75, 0.9)

out <- file.path("analysis", "output", "head_to_head")
dir.create(out, recursive = TRUE, showWarnings = FALSE)

all_scores <- function(races) {
  scores <- bind_rows(
    pr_scores(races) %>% mutate(method = "pr"),
    adjusted_scores(races),
    bind_rows(lapply(sds, function(s) pl_scores(races, s) %>% mutate(method = paste("pl sd =", s)))),
    bind_rows(lapply(ks, function(k) bind_rows(lapply(k_sds, function(s)
      pl_scores(races, s, k) %>% mutate(method = sprintf("pl k = %g, sd = %g", k, s)))))))
  blends <- lapply(weights, function(w) blend(scores, "adjusted", "pl sd = 1", paste("adjusted", w, "with pl"), weight = w))
  bind_rows(scores, blends)
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
write.csv(mae, file.path(out, "mae.csv"), row.names = FALSE)

# One row per method, worst to best on the tuning set
wide <- mae %>%
  select(test, method, mae) %>%
  pivot_wider(names_from = test, values_from = mae) %>%
  arrange(desc(`2025 weeks 2-7`))
cat("\nMAE (places), worst to best on 2025 weeks 2-7\n")
print(as.data.frame(wide), digits = 3, row.names = FALSE)
