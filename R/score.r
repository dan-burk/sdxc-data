# Score a season one gender at a time.
#
# Runners are ranked by course-adjusted time. Every meet has a meet effect:
# how much faster or slower everyone ran there than they usually do (course,
# distance, weather). A runner's adjusted time is their average time with the
# meet effects taken out, on the scale of a typical 5K course.
#
# Then head-to-head: within a class, a runner who is 2-0 or better against
# someone up to 5 places above them is ranked ahead of them.
#
# At the end of each week the whole season so far is refitted and ranked.
# The state meet week isn't ranked. Its snapshot is the state finishers, each
# with their place and the places predicted by the rankings going in and by PRs.
# (analysis/ compares this method with the old points method.)

score_season <- function(season, g) {
  meets <- season$meet_list %>% filter(missing == 0) %>% arrange(week, date, list_row)
  races <- scorable(season$races, season$schools) %>%
    filter(gender == g) %>%
    inner_join(meets %>% select(meet, week, date, flg_5k), by = "meet")
  state_week <- unique(meets$week[startsWith(meets$meet, "sdhsaa_")])  # State AA, A and B

  weekly <- list()
  for (w in sort(unique(races$week))) {
    if (w %in% state_week) {
      weekly[[length(weekly) + 1]] <- state_meet(ranking, races %>% filter(week == w)) %>% mutate(week = w)
      next
    }
    ranking <- rank_athletes(races %>% filter(week <= w)) %>%
      head_to_head(races %>% filter(week <= w), season$schools) %>%
      left_join(athlete_grades(races %>% filter(week <= w)), by = c("name", "school"))
    weekly[[length(weekly) + 1]] <- ranking %>% mutate(week = w)
  }

  bind_rows(weekly) %>%
    left_join(season$schools, by = "school") %>%
    mutate(adj_time = sec_to_time(adj_sec), pr = sec_to_time(pr_sec)) %>%
    select(week, rank, name, school, class, grade, adj_time, pr, races,
           any_of(c("meet", "place", "predicted", "predicted_pr", "time")))
}

# Each runner's grade: the one listed in most of their races so far, ties to
# the latest race. Not used in scoring. Grades outside 7-12 are roster errors
# (a 6th grader can't run varsity) and are left blank.
athlete_grades <- function(results) {
  results %>%
    mutate(grade = suppressWarnings(as.numeric(grade))) %>%
    filter(grade %in% 7:12) %>%
    group_by(name, school, grade) %>%
    summarise(n = n(), last = max(date), .groups = "drop") %>%
    arrange(desc(n), desc(last)) %>%
    distinct(name, school, .keep_all = TRUE) %>%
    select(name, school, grade)
}

# Rank the runners by course-adjusted time, using every result so far.
rank_athletes <- function(results) {
  meet_effect <- meet_effects(results)
  results %>%
    mutate(adjusted = time_sec / exp(meet_effect[meet])) %>%
    group_by(name, school) %>%
    summarise(adj_sec = exp(mean(log(adjusted))),
              pr_sec  = suppressWarnings(min(time_sec[flg_5k == 1])),
              races   = n(), .groups = "drop") %>%
    mutate(pr_sec = if_else(is.infinite(pr_sec), NA_real_, pr_sec)) %>%
    arrange(adj_sec, pr_sec) %>%
    mutate(rank = row_number())
}

# Head-to-head swaps on top of adjusted time, one class at a time. A runner
# moves ahead of anyone up to `window` places above them in their class whom
# they've beaten in every race together, with at least `min_races` races.
# The class keeps its statewide rank numbers; only who holds them changes.
# adj_time stays the runner's own, so a runner can rank ahead of a faster time.
# (analysis/h2h_swaps.r: this neither helps nor hurts predictions.)
head_to_head <- function(ranking, results, schools, window = 5, min_races = 2) {
  id <- paste(ranking$name, ranking$school, sep = "|")
  finish <- lapply(split(results, paste(results$name, results$school, sep = "|")),
                   function(r) setNames(r$row, r$meet))  # finish order in each meet
  class <- schools$class[match(ranking$school, schools$school)]
  for (cl in unique(class)) {
    rows <- which(class == cl)
    rows <- rows[order(ranking$rank[rows])]
    new  <- h2h_order(id[rows], finish, window, min_races)$ids
    ranking$rank[match(new, id)] <- ranking$rank[rows]
  }
  ranking %>% arrange(rank)
}

# Reorder `ids` (best first): each runner moves ahead of the highest of the
# `window` runners above them they own (beaten in every race together,
# `min_races` or more), but never past someone who owns them. Records can form
# cycles (A > B > C > A), so the number of moves is capped.
h2h_order <- function(ids, finish, window = 5, min_races = 2) {
  owns <- function(a, b) {
    common <- intersect(names(finish[[a]]), names(finish[[b]]))
    length(common) >= min_races && all(finish[[a]][common] < finish[[b]][common])
  }
  moves <- 0
  max_moves <- 10 * length(ids)
  i <- 2
  while (i <= length(ids) && moves < max_moves) {
    target <- NA
    for (j in rev(max(1, i - window):(i - 1))) {  # from just above, upward
      if (owns(ids[j], ids[i])) break
      if (owns(ids[i], ids[j])) target <- j
    }
    if (is.na(target)) {
      i <- i + 1
    } else {
      ids <- append(ids[-i], ids[i], after = target - 1)
      moves <- moves + 1
      i <- target + 1
    }
  }
  list(ids = ids, moves = moves, capped = moves >= max_moves)
}

# Meet effects on the log scale, named by meet (0.05 = about 5% slow).
#
# Model: log(time) = runner + meet. Fitted by alternating two steps until
# they stop changing:
#   runner = average of (log time - meet effect) over the runner's races
#   meet   = median of (log time - runner) over the meet's linked runners
# A linked runner has also raced another meet so far; first-timers can't
# measure a course. A meet needs `min_linked` linked runners to be measured;
# otherwise it counts as a typical course (0) until more of its runners race
# again. Log time makes a meet effect a percentage, so races of any distance
# fit the same model. 0 = the typical (median) measured 5K meet.
meet_effects <- function(results, min_linked = 5) {
  id <- paste(results$name, results$school)
  y  <- log(results$time_sec)
  meets_run <- tapply(results$meet, id, n_distinct)
  linked    <- meets_run[id] > 1
  measured  <- names(which(tapply(linked, results$meet, sum) >= min_linked))
  is_5k     <- intersect(measured, unique(results$meet[results$flg_5k == 1]))
  baseline  <- if (length(is_5k)) is_5k else measured

  meet_effect <- setNames(rep(0, n_distinct(results$meet)), unique(results$meet))
  for (i in 1:1000) {
    runner <- tapply(y - meet_effect[results$meet], id, mean)
    new    <- meet_effect
    keep   <- linked & results$meet %in% measured
    new[measured] <- tapply((y - runner[id])[keep], results$meet[keep], median)[measured]
    if (length(baseline)) new[measured] <- new[measured] - median(new[baseline])
    change <- max(abs(new - meet_effect))
    meet_effect <- new
    if (change < 1e-9) break
  }
  attr(meet_effect, "measured") <- measured  # the rest are 0 because they couldn't be measured
  meet_effect
}

# State finishers with their ranking going in. place = finish order among
# ranked (in-state, timed) runners; predicted = order in the race by rank going
# in (blank for runners with no race before state).
state_meet <- function(ranking, state) {
  ranking %>%
    right_join(state %>% select(meet, row, name, school, time), by = c("name", "school")) %>%
    group_by(meet) %>%
    arrange(row, .by_group = TRUE) %>%
    # predicted / predicted_pr: NA for no rank / no 5K PR going in
    mutate(place = row_number(), predicted = min_rank(rank), predicted_pr = min_rank(pr_sec)) %>%
    ungroup() %>%
    select(-row)
}

# State team places. SDHSAA: AA runs 7 and scores 5, A runs 6 and scores 4,
# B runs 5 and scores 3. Only schools with enough finishers to score are
# placed; their runners are placed 1..n among themselves (by `order`, lower is
# better, NA last; tied runners share the better place), a team's score is its
# scorers' places, and ties go to the next runner. Returns one row per team,
# best first: school, place, score, and the team places of its scorers and of
# the rest (displacers).
team_scorers <- c(AA = 5, A = 4, B = 3)
team_scores <- function(school, order, n) {
  full <- school %in% names(which(table(school) >= n))
  place <- rank(replace(order, is.na(order), Inf)[full], ties.method = "min")
  teams <- lapply(split(place, school[full]), sort)
  d <- data.frame(school      = names(teams),
                  score       = sapply(teams, function(p) sum(p[1:n])),
                  next_runner = sapply(teams, function(p) if (length(p) > n) p[n + 1] else Inf))
  d$scorers    <- lapply(teams, function(p) p[1:n])
  d$displacers <- lapply(teams, function(p) p[-(1:n)])
  d <- d[order(d$score, d$next_runner), ]
  d$place <- seq_len(nrow(d))
  rownames(d) <- NULL
  d
}

# Each team's place only, named by school, best first
team_places <- function(school, order, n) {
  d <- team_scores(school, order, n)
  setNames(d$place, d$school)
}

sec_to_time <- function(sec) {
  sec <- round(sec, 2)  # so 1019.999 prints 17:00.00, not 16:60.00
  ifelse(is.na(sec), NA, sprintf("%d:%05.2f", sec %/% 60, sec %% 60))
}
