# How much better is course-adjusted time than the 2025 method?
#
# Three methods, each predicting next week's finish places from this week's ratings:
#   pr    fastest 5K so far
#   old   the 2025 published rank: 5% points blended with PR
#   new   course-adjusted time (the pipeline's method from 2026)
#
# Error = predicted place - actual place (+ = finished better than predicted).
# MAE = mean |error| in places. Only runners every method can rate are
# compared (they had run a 5K before), re-numbered 1..n in each race.
#
# Run from the repo root: Rscript analysis/old_vs_new.r
# Tables: analysis/output/old_vs_new/. Figures: analysis/figures/.

suppressPackageStartupMessages({ library(dplyr); library(tidyr); library(ggplot2) })
library(stringr)
options(width = 200)
source("R/load.r")
source("R/score.r")
source("analysis/ratings.r")
source("analysis/plot_theme.r")
set.seed(651)
n_boot <- 1000

out <- file.path("analysis", "output", "old_vs_new")
dir.create(out, recursive = TRUE, showWarnings = FALSE)

# ---- Backtest --------------------------------------------------------------

results <- list()
effects <- list()
for (year in c(2025, 2026)) {
  season <- load_season(year)
  for (g in c("boys", "girls")) {
    races  <- season_races(season, g)
    scores <- bind_rows(pr_scores(races) %>% mutate(method = "pr"),
                        replay(races, wager_update(0.05), 1000) %>% mutate(method = "points"),
                        adjusted_scores(races) %>% mutate(method = "new"))
    scores <- bind_rows(scores %>% filter(method != "points"),
                        blend(scores, "points", "pr", "old"))
    results[[paste(year, g)]] <- backtest(races, scores) %>%
      mutate(year = year, gender = g, school = str_remove(id, ".*\\|")) %>%
      left_join(season$schools, by = "school")
    if (year == 2025) {
      # Course effects going into state (weeks 1-7), for the explainer figure
      pre_state <- races %>% filter(week < 8)
      effects[[g]] <- tibble(meet = names(meet_effects(pre_state)), effect = meet_effects(pre_state), gender = g) %>%
        left_join(season$meet_list %>% select(meet, official_meet_name, flg_5k), by = "meet")
    }
  }
}
results <- bind_rows(results) %>%
  mutate(test   = test_set(year, week),
         region = if_else(region == "", "AA", region),
         method = factor(method_labels[method], levels = method_labels),
         decile = ceiling(10 * actual / n_race),  # decile of the actual finish, 1 = front
         race   = paste(toupper(str_remove(meet, "sdhsaa_")), gender))
state <- results %>% filter(test == "2025 state meet")

mae_by <- function(d, ...) {
  d %>% group_by(..., method) %>%
    summarise(n = n(), mae = mean(abs(error)), bias = mean(error), .groups = "drop")
}

# ---- Headline: MAE and % improvement with bootstrap intervals --------------

# Resample runners within each race, keeping each runner's three errors together
boot <- function(d) {
  wide <- d %>% select(year, gender, week, meet, id, method, error) %>%
    pivot_wider(names_from = method, values_from = error) %>%
    group_by(year, gender, week, meet)
  one <- function(w) {
    m <- colMeans(abs(w[, method_labels]))
    c(m, better_than_old = 1 - m[[3]] / m[[2]], better_than_pr = 1 - m[[3]] / m[[1]])
  }
  est  <- one(wide)
  sims <- replicate(n_boot, one(slice_sample(wide, prop = 1, replace = TRUE)))
  tibble(stat = names(est), estimate = est,
         lower = apply(sims, 1, quantile, 0.025), upper = apply(sims, 1, quantile, 0.975))
}
headline <- results %>% group_by(test) %>% group_modify(~ boot(.x)) %>% ungroup()
write.csv(headline, file.path(out, "headline.csv"), row.names = FALSE)

cat("\nMAE (places) and improvement, 95% bootstrap intervals\n")
print(headline %>% mutate(across(where(is.numeric), ~ if_else(str_starts(stat, "better"), round(100 * .x, 1), round(.x, 2)))) %>%
        mutate(stat = str_replace(stat, "better_than_", "% better than ")), n = Inf)

mae <- headline %>% filter(stat %in% method_labels) %>% mutate(method = factor(stat, levels = method_labels))
p <- ggplot(mae, aes(estimate, method, colour = method)) +
  geom_errorbar(aes(xmin = lower, xmax = upper), width = 0, linewidth = 0.9, orientation = "y") +
  geom_point(size = 3) +
  geom_text(aes(label = sprintf("%.1f", estimate)), vjust = -1.1, size = 3.5, colour = ink) +
  facet_wrap(~ test, scales = "free_x") +
  scale_colour_manual(values = method_colours, guide = "none") +
  scale_y_discrete(limits = rev(unname(method_labels))) +
  scale_x_continuous(expand = expansion(mult = 0.25)) +
  labs(title = "Adjusted time predicts finish places best on every test",
       subtitle = "Average places off (MAE) with 95% bootstrap intervals. Lower is better.",
       caption = "Each week's races predicted from the ratings after the week before. 2025 weeks 2-7 were used to pick the method; the state meet and 2026 were not.",
       x = "Mean absolute error (places)", y = NULL) +
  theme_sdxc
save_fig(p, "1_headline_mae.png", w = 11, h = 4)

improvement <- headline %>% filter(str_starts(stat, "better")) %>%
  mutate(vs = if_else(stat == "better_than_old", "vs Points + PR (2025)", "vs PR alone"))
p <- ggplot(improvement, aes(estimate, test, fill = vs)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.7) +
  geom_errorbar(aes(xmin = lower, xmax = upper), position = position_dodge(width = 0.75),
                width = 0, linewidth = 0.6, colour = ink_2, orientation = "y") +
  geom_text(aes(x = upper, label = scales::percent(estimate, accuracy = 1)),
            position = position_dodge(width = 0.75), hjust = -0.3, size = 3.5, colour = ink) +
  scale_fill_manual(values = c("vs Points + PR (2025)" = orange, "vs PR alone" = blue), name = NULL) +
  scale_x_continuous(labels = scales::percent, expand = expansion(mult = c(0, 0.15))) +
  scale_y_discrete(limits = rev(levels(results$test))) +
  labs(title = "How many fewer places off adjusted time is",
       subtitle = "Percent reduction in MAE, with 95% bootstrap intervals",
       x = NULL, y = NULL) +
  theme_sdxc
save_fig(p, "2_improvement.png", w = 9, h = 4)

# ---- Week by week ----------------------------------------------------------

by_week <- mae_by(results, year, week)
write.csv(by_week, file.path(out, "by_week.csv"), row.names = FALSE)
p <- ggplot(by_week, aes(week, mae, colour = method)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.2) +
  facet_wrap(~ year, scales = "free_x") +
  scale_x_continuous(breaks = 2:8, labels = c(2:7, "State")) +
  scale_colour_manual(values = method_colours, name = NULL) +
  labs(title = "Better every week, including early season",
       subtitle = "Each week predicted from the ratings after the week before",
       caption = "State MAE is higher because state fields are about three times bigger than a regular meet.",
       x = "Week predicted", y = "Mean absolute error (places)") +
  theme_sdxc
save_fig(p, "3_by_week.png")

# ---- Deciles ---------------------------------------------------------------

by_decile <- mae_by(results, test, decile)
write.csv(by_decile, file.path(out, "by_decile.csv"), row.names = FALSE)
p <- ggplot(by_decile, aes(decile, mae, colour = method)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.2) +
  facet_wrap(~ test, scales = "free_y") +
  scale_x_continuous(breaks = 1:10) +
  scale_colour_manual(values = method_colours, name = NULL) +
  labs(title = "Where in the field the predictions miss",
       subtitle = "MAE by decile of actual finish (1 = front 10% of the race)",
       caption = "Every method is best at the front and back; the middle of the field has room to miss both ways.",
       x = "Decile of actual finish", y = "Mean absolute error (places)") +
  theme_sdxc
save_fig(p, "4_by_decile.png", w = 11, h = 4.5)

# ---- Regions ---------------------------------------------------------------

by_region <- mae_by(results %>% filter(test != "2026 so far"), test, region)
write.csv(by_region, file.path(out, "by_region.csv"), row.names = FALSE)
cat("\nRegion bias: average |mean error| over regions (places)\n")
print(by_region %>% filter(region != "AA") %>% group_by(test, method) %>%
        summarise(region_bias = round(mean(abs(bias)), 2), .groups = "drop") %>%
        pivot_wider(names_from = method, values_from = region_bias))
region_order <- c("AA", paste0(1:5, "A"), paste0(1:5, "B"))
region_plot <- function(d, value, title, subtitle, xlab, zero = FALSE) {
  d <- d %>% mutate(value = .data[[value]], region = factor(region, levels = rev(region_order)))
  segments <- d %>% select(test, region, method, value) %>%
    pivot_wider(names_from = method, values_from = value)
  p <- ggplot(d, aes(value, region))
  if (zero) p <- p + geom_vline(xintercept = 0, colour = ink_2, linewidth = 0.4)
  p + geom_segment(data = segments, aes(x = .data[[method_labels[["old"]]]], xend = .data[[method_labels[["new"]]]],
                                        y = region, yend = region), colour = "#c3c2b7", linewidth = 1.2) +
    geom_point(aes(colour = method), size = 2.8) +
    facet_wrap(~ test, scales = "free_x") +
    scale_colour_manual(values = method_colours, name = NULL) +
    labs(title = title, subtitle = subtitle, x = xlab, y = NULL) +
    theme_sdxc
}
p <- region_plot(by_region, "mae", "Adjusted time is more accurate in almost every region",
                 "MAE by the runner's region. Grey bar = change from the 2025 method to adjusted time.",
                 "Mean absolute error (places)")
save_fig(p, "5_by_region_mae.png", w = 10, h = 5.5)
p <- region_plot(by_region, "bias", "Region bias shrinks",
                 "Average signed error. + = the region's runners finished better than predicted (underrated).",
                 "Mean error (places)", zero = TRUE) +
  labs(caption = "Errors sum to zero within a race, so bias is relative to the other regions in the same class. AA at state is one race, so always 0.")
save_fig(p, "6_by_region_bias.png", w = 10, h = 5.5)

# ---- State races -----------------------------------------------------------

by_race <- mae_by(state, race)
write.csv(by_race, file.path(out, "state_by_race.csv"), row.names = FALSE)
p <- ggplot(by_race, aes(mae, factor(race, levels = rev(sort(unique(race)))), colour = method)) +
  geom_point(size = 3) +
  scale_colour_manual(values = method_colours, name = NULL) +
  labs(title = "2025 state meet, race by race",
       x = "Mean absolute error (places)", y = NULL) +
  theme_sdxc
save_fig(p, "7_state_by_race.png", w = 8, h = 4)

# ---- Predicted vs actual at state -------------------------------------------

scatter <- state %>% filter(method != method_labels[["pr"]])
p <- ggplot(scatter, aes(predicted / n_race, actual / n_race)) +
  geom_abline(colour = ink_2, linewidth = 0.4) +
  geom_point(aes(colour = method), alpha = 0.35, size = 1.4, show.legend = FALSE) +
  facet_wrap(~ method) +
  scale_colour_manual(values = method_colours) +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  coord_equal() +
  labs(title = "2025 state meet: predicted vs actual finish",
       subtitle = "Position in the race (0% = winner). Closer to the line = better prediction.",
       x = "Predicted", y = "Actual") +
  theme_sdxc
save_fig(p, "8_state_predicted_vs_actual.png", w = 9, h = 5)

# ---- How fast was each course? ---------------------------------------------

effects <- bind_rows(effects) %>%
  group_by(meet, official_meet_name, flg_5k) %>%
  summarise(effect = mean(effect), .groups = "drop") %>%  # boys and girls averaged
  mutate(pct  = exp(effect) - 1,
         kind = if_else(flg_5k == 1, "5K", "Not a 5K"),
         official_meet_name = reorder(official_meet_name, pct))
write.csv(effects, file.path(out, "course_effects_2025.csv"), row.names = FALSE)
p <- ggplot(effects, aes(pct, official_meet_name, colour = kind)) +
  geom_vline(xintercept = 0, colour = ink_2, linewidth = 0.4) +
  geom_point(size = 1.8) +
  scale_colour_manual(values = c("5K" = blue, "Not a 5K" = orange), name = NULL) +
  scale_x_continuous(labels = function(x) sprintf("%+.0f%%", 100 * x)) +
  labs(title = "How fast was each 2025 meet?",
       subtitle = "How much slower (+) or faster (-) runners ran than usual,\ncompared with a typical 5K. Weeks 1-7, boys and girls averaged.",
       x = NULL, y = NULL) +
  theme_sdxc + theme(axis.text.y = element_text(size = 6.5))
save_fig(p, "9_course_effects_2025.png", w = 8, h = 14)
