# Rating methods for the analysis, and a week-ahead backtest to compare them.
#
# Each method returns one row per athlete per week: week, id, score, where
# score is the athlete's rating after that week's meets (higher = better).

# One gender's scorable results, in the order the pipeline runs them.
season_races <- function(season, g) {
  meets <- season$meet_list %>%
    filter(missing == 0) %>%
    arrange(week, date, list_row) %>%
    mutate(order = row_number())
  scorable(season$races, season$schools) %>%
    filter(gender == g) %>%
    inner_join(meets %>% select(meet, week, flg_5k, order), by = "meet") %>%
    mutate(id = paste(name, school, sep = "|")) %>%
    arrange(order, row)
}

# Replay every race in order. `update` takes the ratings of one race's runners
# in finish order and returns their new ratings. New runners start at `start`.
replay <- function(races, update, start) {
  rating <- numeric()
  out <- list()
  for (w in sort(unique(races$week))) {
    week_races <- races[races$week == w, ]
    for (m in unique(week_races$meet)) {
      race <- week_races[week_races$meet == m, ]
      rating[setdiff(race$id, names(rating))] <- start
      rating[race$id] <- update(rating[race$id])
    }
    out[[length(out) + 1]] <- tibble(week = w, id = names(rating), score = unname(rating))
  }
  bind_rows(out)
}

# The 2025 points method: each runner takes `wager` of the points of every
# runner behind them (checked to match the 2025 pipeline exactly).
wager_update <- function(wager) {
  function(points) {
    n <- length(points)
    for (i in seq_len(n - 1)) {
      behind <- (i + 1):n
      points[i] <- points[i] + wager * sum(points[behind])
      points[behind] <- points[behind] * (1 - wager)
    }
    points
  }
}

# Elo for a race with many runners: each runner is scored against every other
# runner in the race. actual = share of the field beaten, expected = average
# chance of beating each opponent given the ratings. Rating moves K * (actual - expected).
elo_update <- function(K) {
  function(rating) {
    n <- length(rating)
    if (n < 2) return(rating)
    beat_chance <- 1 / (1 + 10^((outer(rating, rating, "-") * -1) / 400))  # [i, j] = P(i beats j)
    diag(beat_chance) <- 0
    expected <- rowSums(beat_chance) / (n - 1)
    actual   <- (n - seq_len(n)) / (n - 1)
    rating + K * (actual - expected)
  }
}

# Fastest 5K so far (score = minus seconds)
pr_scores <- function(races) {
  bind_rows(lapply(sort(unique(races$week)), function(w) {
    races %>%
      filter(week <= w, flg_5k == 1) %>%
      group_by(id) %>%
      summarise(score = -min(time_sec), .groups = "drop") %>%
      mutate(week = w, .before = 1)
  }))
}

# Course-adjusted time: the pipeline's method (rank_athletes() in R/score.r),
# refitted on all results up to each week
adjusted_scores <- function(races) {
  bind_rows(lapply(sort(unique(races$week)), function(w) {
    rank_athletes(races %>% filter(week <= w)) %>%
      transmute(week = w, id = paste(name, school, sep = "|"), method = "adjusted", score = -adj_sec)
  }))
}

# The 2025 method's blend: average of the two statewide ranks. `weight` is a's
# share (0.5 = the plain average).
blend <- function(scores, a, b, name, weight = 0.5) {
  scores %>%
    filter(method %in% c(a, b)) %>%
    group_by(week, method) %>%
    mutate(r = min_rank(desc(score))) %>%
    ungroup() %>%
    select(week, id, method, r) %>%
    pivot_wider(names_from = method, values_from = r) %>%
    filter(!is.na(.data[[a]]), !is.na(.data[[b]])) %>%
    transmute(week, id, method = name, score = -(weight * .data[[a]] + (1 - weight) * .data[[b]]))
}

# Week-ahead backtest: each race in week w is predicted from the ratings after
# week w - 1. Only runners every method can rate are kept (in practice: ran a
# 5K before), and places are re-numbered 1..n among them in each race.
# Returns one row per runner, race and method.
backtest <- function(races, scores) {
  n_methods <- n_distinct(scores$method)
  scores %>%
    mutate(week = week + 1) %>%  # ratings after week w - 1 predict week w
    inner_join(races %>% select(week, meet, row, id), by = c("week", "id"),
               relationship = "many-to-many") %>%
    group_by(week, meet, id) %>%
    filter(n() == n_methods) %>%
    group_by(week, meet, method) %>%
    mutate(n_race    = n(),
           actual    = rank(row),
           predicted = rank(-score),
           error     = predicted - actual) %>%
    ungroup()
}

# Which test set a predicted week belongs to. 2025 weeks 2-7 were used to tune
# methods; the 2025 state meet and 2026 were not.
test_set <- function(year, week) {
  factor(case_when(year == 2025 & week == 8 ~ "2025 state meet",
                   year == 2025             ~ "2025 weeks 2-7",
                   TRUE                     ~ paste(year, "so far")),
         levels = c("2025 weeks 2-7", "2025 state meet", "2026 so far"))
}

# Adjusted time with recent races counting more. Meet effects are fitted as in
# the pipeline; then each runner's rating is a weighted average of their
# adjusted races, where a race `half_life` weeks old counts half as much as
# one this week. half_life = Inf is the pipeline's plain average.
adjusted_recency_scores <- function(races, half_life) {
  bind_rows(lapply(sort(unique(races$week)), function(w) {
    d <- races %>% filter(week <= w)
    meet_effect <- meet_effects(d)
    d %>%
      mutate(adjusted = log(time_sec) - meet_effect[meet],
             weight   = 0.5^((w - week) / half_life)) %>%
      group_by(id) %>%
      summarise(score = -weighted.mean(adjusted, weight), .groups = "drop") %>%
      mutate(week = w, method = paste("half-life", half_life), .before = 1)
  }))
}

# Plackett-Luce: each race's finish order is a ranking. The winner is picked
# from the field with chance proportional to exp(strength), 2nd from the
# runners left, and so on. Strengths are fitted by maximum likelihood with a
# normal prior on each runner (mean `prior_mean`, sd `prior_sd`), which keeps
# unbeaten runners finite and pulls runners with few races toward the prior.
# `races` must be in finish order within each meet (season_races() is).
pl_fit <- function(races, prior_mean, prior_sd) {
  ids    <- names(prior_mean)
  groups <- split(match(races$id, ids), races$meet)  # runner positions, in finish order

  # minus the log posterior, and its gradient
  value_and_grad <- function(theta) {
    ll   <- 0
    grad <- numeric(length(theta))
    for (g in groups) {
      m <- max(theta[g])
      e <- exp(theta[g] - m)
      S <- rev(cumsum(rev(e)))  # S[k] = sum of e over the runners who finished k-th or later
      ll <- ll + sum(theta[g] - m) - sum(log(S))
      grad[g] <- grad[g] + 1 - e * cumsum(1 / S)
    }
    z <- (theta - prior_mean) / prior_sd
    list(value = -(ll - sum(z^2) / 2), grad = -(grad - z / prior_sd))
  }
  fit <- optim(prior_mean, function(t) value_and_grad(t)$value, function(t) value_and_grad(t)$grad,
               method = "L-BFGS-B", control = list(maxit = 1000))
  setNames(fit$par, ids)
}

# Plackett-Luce refitted on all results up to each week. k = 0: every runner's
# prior mean is 0 (wins and losses only). k > 0: the prior mean is k times the
# runner's adjusted time as a z-score (faster = higher), so runners start from
# their times and head-to-head results move them from there.
pl_scores <- function(races, prior_sd, k = 0) {
  bind_rows(lapply(sort(unique(races$week)), function(w) {
    d   <- races %>% filter(week <= w)
    adj <- rank_athletes(d) %>% transmute(id = paste(name, school, sep = "|"), x = -log(adj_sec))
    prior_mean <- setNames(k * (adj$x - mean(adj$x)) / sd(adj$x), adj$id)
    theta <- pl_fit(d, prior_mean, prior_sd)
    tibble(week = w, id = names(theta), score = unname(theta))
  }))
}

# Adjusted time from each runner's best races only. Meet effects are fitted as
# in the pipeline; then the rating is the average of the runner's `n_best`
# fastest adjusted races (all of them if they have fewer). drop_worst = TRUE
# instead averages all but the slowest, for runners with 3 or more races.
adjusted_best_scores <- function(races, n_best = Inf, drop_worst = FALSE) {
  bind_rows(lapply(sort(unique(races$week)), function(w) {
    d <- races %>% filter(week <= w)
    meet_effect <- meet_effects(d)
    d %>%
      mutate(adjusted = log(time_sec) - meet_effect[meet]) %>%
      group_by(id) %>%
      arrange(adjusted, .by_group = TRUE) %>%
      mutate(n = n(), k = if (drop_worst) ifelse(n >= 3, n - 1, n) else pmin(n, n_best)) %>%
      filter(row_number() <= k) %>%
      summarise(score = -mean(adjusted), .groups = "drop") %>%
      mutate(week = w, .before = 1)
  }))
}

# Head-to-head swaps on top of adjusted time, as in the pipeline (h2h_order()
# in R/score.r) but with the window and minimum races as settings. Within each
# class, the moved runner takes the adjusted time of the place they land in, so
# mixed-class races still compare across classes.
h2h_scores <- function(races, classes, window = 5, min_races = 2) {
  bind_rows(lapply(sort(unique(races$week)), function(w) {
    d <- races %>% filter(week <= w)
    finish <- lapply(split(d, d$id), function(r) setNames(r$row, r$meet))
    base <- rank_athletes(d) %>%
      transmute(id = paste(name, school, sep = "|"), school, adj_sec) %>%
      left_join(classes, by = "school")
    bind_rows(lapply(split(base, base$class), function(b) {
      b <- b %>% arrange(adj_sec)
      o <- h2h_order(b$id, finish, window, min_races)
      tibble(id = o$ids, score = -b$adj_sec, moves = o$moves, capped = o$capped)
    })) %>% mutate(week = w, .before = 1)
  }))
}
