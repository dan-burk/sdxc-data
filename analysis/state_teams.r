# Team places at the state meet: actual vs predicted by PR alone, adjusted
# time, and the published rank (adjusted time + head-to-head swaps).
#
# Team scoring is team_places() in R/score.r (as on the site).
# Predicted: the same runners, ordered by each method's rating going into
# state. Runners with no rating (no 5K for PR) are tied at the back.
#
# Run from the repo root: Rscript analysis/state_teams.r
# Tables: analysis/output/state_teams.csv (every team),
#         analysis/output/state_teams_mae.csv (team MAE by race and year)

suppressPackageStartupMessages({ library(dplyr); library(tidyr) })
library(stringr)
source("R/load.r")
source("R/score.r")
source("analysis/ratings.r")

teams <- list()
for (year in c(2023, 2024, 2025)) {
  season <- load_season(year)
  for (g in c("boys", "girls")) {
    st <- state_scores(season, g)
    state <- st$races %>%
      filter(week == st$state_week)
    for (m in unique(state$meet)) {
      race <- state %>% filter(meet == m)
      n <- team_scorers[[toupper(str_remove(m, "sdhsaa_"))]]
      actual <- team_places(race$school, race$row, n)
      actual <- tibble(school = names(actual), Actual = unname(actual))
      predicted <- lapply(unique(st$scores$method), function(meth) {
        s <- st$scores %>% filter(method == meth) %>% select(id, score)
        x <- race %>% left_join(s, by = "id")
        p <- team_places(x$school, -x$score, n)
        tibble(school = names(p), team_place = unname(p), method = meth)
      })
      teams[[paste(year, g, m)]] <- bind_rows(predicted) %>%
        pivot_wider(names_from = method, values_from = team_place) %>%
        left_join(actual, by = "school") %>%
        mutate(gender = g, year = year, race = toupper(str_remove(m, "sdhsaa_")))
    }
  }
}
teams <- bind_rows(teams) %>%
  select(gender, year, race, school, Actual, `PR alone`, `Adjusted time`, `Adj + swap`) %>%
  arrange(gender, year, race, Actual)

# MAE = mean |predicted - actual| team place, per state race
mae <- teams %>%
  group_by(gender, race, year) %>%
  summarise(teams           = n(),
            `PR alone`      = mean(abs(`PR alone` - Actual)),
            `Adjusted time` = mean(abs(`Adjusted time` - Actual)),
            `Adj + swap`    = mean(abs(`Adj + swap` - Actual)),
            .groups = "drop") %>%
  arrange(gender, factor(race, levels = c("AA", "A", "B")), year)

dir.create(file.path("analysis", "output"), showWarnings = FALSE)
write.csv(teams, file.path("analysis", "output", "state_teams.csv"), row.names = FALSE)
write.csv(mae, file.path("analysis", "output", "state_teams_mae.csv"), row.names = FALSE)
print(as.data.frame(mae), digits = 2)
