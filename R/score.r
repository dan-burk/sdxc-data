# Score a season one gender at a time.
#
# Every runner starts at 1000 points. In each race, every runner beats every
# runner behind them, and takes `wager` (5%) of the loser's current points.
# Meets run in order (week, date, meet list row); rankings are snapshotted
# at the end of each week.

score_season <- function(season, g, wager = 0.05) {
  races <- scorable(season$races, season$schools) %>% filter(gender == g)
  meets <- season$meet_list %>% filter(missing == 0) %>% arrange(week, date, list_row)

  athletes <- tibble(name = character(), school = character(),
                     points = numeric(), pr_sec = numeric())
  weekly <- list()

  for (w in unique(meets$week)) {
    for (m in meets$meet[meets$week == w]) {
      race <- races %>% filter(meet == m) %>% arrange(row)
      if (nrow(race) == 0) next
      is_5k <- meets$flg_5k[meets$meet == m] == 1
      athletes <- run_meet(athletes, race, is_5k, wager)
    }
    weekly[[length(weekly) + 1]] <- rank_athletes(athletes) %>% mutate(week = w)
  }

  bind_rows(weekly) %>%
    left_join(season$schools, by = "school") %>%
    mutate(pr = sec_to_time(pr_sec)) %>%
    select(week, rank, name, school, class, points, pr, rank_points, rank_time)
}

# Apply one race to the running athlete table.
run_meet <- function(athletes, race, is_5k, wager) {
  newcomers <- race %>%
    distinct(name, school) %>%
    anti_join(athletes, by = c("name", "school")) %>%
    mutate(points = 1000, pr_sec = NA_real_)
  athletes <- bind_rows(athletes, newcomers)

  # i = each finisher's row in the athlete table, in finish order
  i <- match(paste(race$name, race$school), paste(athletes$name, athletes$school))
  athletes$points[i] <- score_race(athletes$points[i], wager)
  if (is_5k) athletes$pr_sec[i] <- pmin(athletes$pr_sec[i], race$time_sec, na.rm = TRUE)
  athletes
}

# `points` in finish order (winner first). Returns points after the race.
score_race <- function(points, wager) {
  n <- length(points)
  if (n < 2) return(points)
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      transfer  <- wager * points[j]
      points[i] <- points[i] + transfer
      points[j] <- points[j] - transfer
    }
  }
  points
}

# Final rank = average of points rank and 5K PR rank (points rank alone if no PR).
rank_athletes <- function(athletes) {
  athletes %>%
    mutate(
      rank_points = min_rank(desc(points)),
      rank_time   = min_rank(pr_sec),
      blend       = if_else(is.na(rank_time), rank_points, (rank_points + rank_time) / 2)
    ) %>%
    arrange(blend, rank_time) %>%
    mutate(rank = row_number()) %>%
    select(-blend)
}

sec_to_time <- function(sec) {
  ifelse(is.na(sec), NA, sprintf("%d:%05.2f", sec %/% 60, sec %% 60))
}
