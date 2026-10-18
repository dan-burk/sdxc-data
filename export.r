# Write a season's rankings as JSON for the website (the sister repo ../sdxc).
# One file per gender and week: ../sdxc/data/{gender}_{year}_week{N}.json,
# The state meet week holds state results instead of a ranking (see R/score.r).
# plus the season's schools (../sdxc/data/schools_{year}.json) and the year's latest week in manifest.json,
# and every ranked runner's races (../sdxc/data/results_{gender}_{year}.json),
# and state team places once state is run (../sdxc/data/state_teams_{gender}_{year}.json)
# Run after run.r:  Rscript export.r 2026

year <- 2025  # or: Rscript export.r 2026
if (length(commandArgs(trailingOnly = TRUE))) year <- as.numeric(commandArgs(trailingOnly = TRUE)[1])
out_dir <- "../sdxc/data"

suppressPackageStartupMessages(library(dplyr))
library(stringr)
source("R/load.r")
source("R/score.r")

if (!dir.exists(out_dir)) stop("Can't find ", out_dir, ": the sdxc repo should sit next to this one.")

# "16:31.78" -> 991.78 seconds (the site calls this time_min but it's seconds)
to_seconds <- function(time) {
  parts <- strsplit(as.character(time), ":")  # an all-blank column reads as logical
  sapply(parts, function(p) if (length(p) == 2) as.numeric(p[1]) * 60 + as.numeric(p[2]) else NA)
}

# Every race of every ranked runner, for the runner's page on the site:
# ../sdxc/data/results_{gender}_{year}.json, one row per runner per race.
# Names and schools come after the aliases (load_season), so they match the rankings' id.
# place is the finish place in the whole race (out-of-state runners included);
# field is the number of finishers. DQ/DNF rows have place, time and adj_time null.
write_results <- function(season, gender, rankings) {
  meets <- season$meet_list %>% filter(missing == 0)
  races <- season$races %>%
    filter(gender == !!gender, meet %in% meets$meet) %>%
    group_by(meet) %>%
    mutate(field = sum(!is.na(time_sec)),
           place = ifelse(is.na(time_sec), NA, cumsum(!is.na(time_sec)))) %>%
    ungroup() %>%
    mutate(id = match(paste(name, school), sort(unique(paste(rankings$name, rankings$school))))) %>%
    filter(!is.na(id)) %>%  # out of state, not a school, or never ranked
    inner_join(meets %>% select(meet, meet_name = official_meet_name, date, week, flg_5k), by = "meet") %>%
    arrange(id, date)

  # Each race's adjusted time, from the latest week's fit. The meet effects are
  # refitted every week, so a race's adjusted time can shift a little from week
  # to week. null for a meet with too few linked runners to measure, and for the state meet.
  state_week <- unique(meets$week[startsWith(meets$meet, "sdhsaa_")])
  fit <- scorable(season$races, season$schools) %>%
    filter(gender == !!gender) %>%
    inner_join(meets %>% select(meet, week, flg_5k), by = "meet") %>%
    filter(!week %in% state_week)
  effect <- meet_effects(fit)
  effect[!names(effect) %in% attr(effect, "measured")] <- NA

  site <- data.frame(id       = races$id,
                     meet     = races$meet_name,
                     date     = format(races$date),
                     week     = races$week,
                     flg_5k   = races$flg_5k,
                     place    = races$place,
                     field    = races$field,
                     time     = round(races$time_sec, 2),
                     adj_time = round(races$time_sec / exp(effect[races$meet]), 2),
                     grade    = suppressWarnings(as.numeric(races$grade)))
  file <- file.path(out_dir, paste0("results_", gender, "_", year, ".json"))
  jsonlite::write_json(site, file, pretty = TRUE, digits = NA, na = "null")
  cat("Wrote", file, "(", nrow(site), "results )\n")
}

# State team places (team_places() in R/score.r) for the State Meet page:
# ../sdxc/data/state_teams_{gender}_{year}.json, one row per team per race.
# score, scorers and displacers are from the actual race, in team places
# (places among the scoring teams' runners). predicted is from the rankings
# going in (adjusted time + head-to-head), predicted_pr from each runner's PR going in.
# Before state is run (entries only), place, score, scorers and displacers are null.
write_state_teams <- function(r, gender) {
  site <- bind_rows(lapply(split(r, r$meet), function(x) {
    race <- toupper(sub("sdhsaa_", "", x$meet[1]))
    n <- team_scorers[[race]]
    ran <- any(!is.na(x$place))
    actual <- team_scores(x$school, if (ran) x$place else x$rank, n)  # before state: in predicted order
    site <- data.frame(race         = race,
                       School       = actual$school,
                       place        = if (ran) actual$place else NA,
                       predicted    = unname(team_places(x$school, x$rank, n)[actual$school]),
                       predicted_pr = unname(team_places(x$school, to_seconds(x$pr), n)[actual$school]),
                       score        = if (ran) actual$score else NA)
    site$scorers    <- if (ran) actual$scorers else NA
    site$displacers <- if (ran) actual$displacers else NA
    site
  }))
  file <- file.path(out_dir, paste0("state_teams_", gender, "_", year, ".json"))
  jsonlite::write_json(site, file, pretty = TRUE, na = "null")
  cat("Wrote", file, "(", nrow(site), "teams )\n")
}

season <- load_season(year)

for (gender in c("boys", "girls")) {
  rankings <- read.csv(file.path(year, "output", paste0("rankings_", gender, ".csv")))
  # The state week's rows have a meet (place is blank before state is run)
  state_week <- if ("meet" %in% names(rankings)) unique(rankings$week[!is.na(rankings$meet)]) else NULL

  # id: one number per athlete for the whole season, so it's the same every week
  athlete <- paste(rankings$name, rankings$school)
  rankings$id <- match(athlete, sort(unique(athlete)))

  write_results(season, gender, rankings)

  for (wk in sort(unique(rankings$week))) {
    r <- rankings[rankings$week == wk, ]
    if (wk %in% state_week) {
      # One row per state finisher: their race, place and predicted places (rankings, PRs),
      # plus their ranking going in (rnk_blnd, adj_time, time_min; null if unranked)
      r <- r[order(r$meet, r$place, r$predicted), ]  # before state: predicted order
      site <- data.frame(Name         = r$name,
                         School       = r$school,
                         id           = r$id,
                         school_class = r$class,
                         race         = toupper(sub("sdhsaa_", "", r$meet)),
                         place        = r$place,
                         predicted    = r$predicted,
                         predicted_pr = r$predicted_pr,
                         state_time   = round(to_seconds(r$time), 2),
                         rnk_blnd     = r$rank,
                         adj_time     = round(to_seconds(r$adj_time), 2),
                         time_min     = round(to_seconds(r$pr), 2))
      write_state_teams(r, gender)
    } else {
      r <- r[order(r$rank), ]
      site <- data.frame(Name         = r$name,
                         School       = r$school,
                         id           = r$id,
                         adj_time     = round(to_seconds(r$adj_time), 2),
                         time_min     = round(to_seconds(r$pr), 2),
                         rnk_blnd     = r$rank,
                         school_class = r$class,
                         grade        = r$grade)  # null if unknown
    }
    file <- file.path(out_dir, paste0(gender, "_", year, "_week", wk, ".json"))
    # runners with no PR get time_min: null
    jsonlite::write_json(site, file, pretty = TRUE, digits = NA, na = "null")
    cat("Wrote", file, "(", nrow(site), "runners )\n")
  }
}

# The season's in-state schools, classes and regions, for the Teams tab
schools <- read.csv(file.path(year, "schools.csv"))
site <- data.frame(School = schools$school, school_class = schools$class,
                   region = ifelse(schools$region == "", NA, schools$region))  # AA has no regions
site <- site[order(site$School), ]
file <- file.path(out_dir, paste0("schools_", year, ".json"))
jsonlite::write_json(site, file, pretty = TRUE, na = "null")
cat("Wrote", file, "(", nrow(site), "schools )\n")

# manifest.json tells the site which years exist, each year's latest week,
# and which week is the state meet (for years that have run it)
file <- file.path(out_dir, "manifest.json")
manifest <- jsonlite::read_json(file)
manifest$years[[as.character(year)]] <- max(rankings$week)
manifest$years <- manifest$years[order(names(manifest$years))]
if (is.null(manifest$state)) manifest$state <- list()
manifest$state[[as.character(year)]] <- if (length(state_week)) state_week else NULL
manifest$current <- max(as.numeric(names(manifest$years)))
jsonlite::write_json(manifest, file, pretty = TRUE, auto_unbox = TRUE)
cat("Wrote", file, "(", year, "has", max(rankings$week), "weeks )\n")
