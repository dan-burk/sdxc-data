# Look for problems in a loaded season before scoring. Changes nothing.
#
# ERRORS stop the run: fix the curated files or data, then rerun.
# WARNINGS are worth a look but don't stop anything.

check_season <- function(season) {
  errors <- 0

  report <- function(level, title, rows) {
    if (nrow(rows) == 0) return(invisible())
    cat("\n", level, ": ", title, " (", nrow(rows), ")\n", sep = "")
    print(as.data.frame(rows), row.names = FALSE, right = FALSE)
    if (level == "ERROR") errors <<- errors + 1
  }

  meets   <- season$meet_list
  races   <- season$races
  schools <- season$schools
  aliases <- season$school_aliases
  year    <- season$year

  # --- Meet list -------------------------------------------------------------
  report("ERROR", "Meet listed more than once",
         meets %>% count(meet) %>% filter(n > 1))
  report("ERROR", "Meet list: date not in this season, or week, flg_5k or missing not filled in",
         meets %>%
           filter(is.na(date) | format(date, "%Y") != as.character(year) |
                  is.na(week) | !missing %in% c(0, 1) | !flg_5k %in% c(0, 1)) %>%
           select(meet, date, week, flg_5k, missing))

  # --- Curated lists ---------------------------------------------------------
  report("ERROR", "School listed more than once in schools.csv",
         schools %>% count(school) %>% filter(n > 1))
  report("ERROR", "School class is not AA, A or B",
         schools %>% filter(!class %in% c("AA", "A", "B")))
  report("ERROR", "Raw school listed more than once in school_aliases.csv",
         aliases %>% count(raw_school) %>% filter(n > 1))
  report("ERROR", "Runner listed more than once in athlete_aliases.csv",
         season$athlete_aliases %>% count(name, school) %>% filter(n > 1))
  report("WARNING", "Athlete alias matches no runner (use the standardized school name)",
         season$athlete_aliases %>%
           filter(!paste(correct_name, school) %in% paste(races$name, races$school)) %>%
           select(name, school))
  report("ERROR", "Alias points to a school not in schools.csv",
         aliases %>%
           filter(raw_school %in% races$raw_school,
                  !school %in% c(schools$school, "OUT", "DROP")) %>%
           select(raw_school, school))

  # --- Files -----------------------------------------------------------------
  expected <- tidyr::expand_grid(meet = meets$meet[meets$missing == 0],
                                 gender = c("boys", "girls")) %>%
    mutate(found = file.exists(race_path(year, meet, gender)))
  report("ERROR", "Meet not marked missing but has no result files",
         expected %>% group_by(meet) %>% filter(!any(found)) %>% distinct(meet))
  report("WARNING", "Meet has only one gender's results",
         expected %>% group_by(meet) %>% filter(any(found), !found) %>% select(meet, gender))

  both <- expected %>% group_by(meet) %>% filter(all(found)) %>% distinct(meet)
  same <- both %>% filter(sapply(meet, function(m)
    identical(readLines(race_path(year, m, "boys")), readLines(race_path(year, m, "girls")))))
  report("ERROR", "Boys and girls results are the same file (re-save the right one's TXT)", same)

  files <- list.files(file.path(year, "Data"), "_(boys|girls)\\.csv$")
  report("WARNING", "Result file not in the meet list",
         tibble(file = files) %>%
           filter(!sub("_(boys|girls)\\.csv$", "", file) %in% meets$meet))

  # --- Result rows -----------------------------------------------------------
  report("ERROR", "Blank name or school (check the file's header: Place,Name,School,Time,Grade)",
         races %>% filter(is.na(name) | name == "" | is.na(school) | school == "") %>%
           select(meet, gender, row, name, school))
  report("ERROR", "Unknown school (add to schools.csv or school_aliases.csv)",
         races %>%
           filter(!school %in% c(schools$school, "OUT", "DROP")) %>%
           group_by(school) %>%
           summarise(runners = n(), example_meet = first(meet)))
  report("ERROR", "Time can't be read (DQ/DNF/DNS are fine and are dropped)",
         races %>%
           filter(is.na(time_sec), !toupper(time) %in% c("DQ", "DNF", "DNS", "")) %>%
           select(meet, gender, row, name, time))
  report("WARNING", "Grade outside 7-12 (shown blank; fix the TXT if the real grade is known)",
         races %>%
           filter(!suppressWarnings(as.numeric(grade)) %in% 7:12, !is.na(grade), grade != "") %>%
           select(meet, gender, row, name, school, grade))

  report("ERROR", "Rows not in finish order (a runner is faster than the row above)",
         races %>%
           filter(!is.na(time_sec)) %>%
           group_by(meet, gender) %>%
           filter(time_sec < lag(time_sec)) %>%
           select(meet, gender, row, name, time))

  timed <- races %>% filter(!is.na(time_sec)) %>% distinct(meet, gender, name, time_sec)
  report("ERROR", "Same race entered as two meets (3+ runners with identical times; merge into one meet)",
         inner_join(timed, timed, by = c("gender", "name", "time_sec"), suffix = c("_1", "_2"),
                    relationship = "many-to-many") %>%
           filter(meet_1 < meet_2) %>%
           count(meet_1, meet_2, gender, name = "shared_runners") %>%
           filter(shared_runners >= 3))

  in_state <- races %>% filter(school %in% schools$school)
  report("ERROR", "Same runner twice in one race",
         in_state %>% count(meet, gender, name, school) %>% filter(n > 1))
  report("WARNING", "Possible misspelled runner (same school, similar name; add to athlete_aliases.csv if same person, different_athletes.csv if not)",
         similar_names(in_state) %>%
           anti_join(season$different_athletes, by = c("school", "name_1", "name_2")) %>%
           anti_join(season$different_athletes, by = c("school", "name_1" = "name_2", "name_2" = "name_1")))

  # --- State entries (who's running state) -----------------------------------
  if (!is.null(season$state_entries)) {
    report("WARNING", "State entry not in this season's results (add a school or athlete alias if misspelled)",
           season$state_entries %>% anti_join(in_state, by = c("gender", "name", "school")) %>%
             select(meet, gender, name, school))
    # The seed is the runner's 5K season best on the entry site, so it should match our 5K PR
    # going into state (state itself is left out once its results are in)
    pr <- in_state %>%
      inner_join(meets %>% filter(flg_5k == 1, !startsWith(meet, "sdhsaa_")) %>% select(meet), by = "meet") %>%
      filter(!is.na(time_sec)) %>%
      group_by(gender, name, school) %>%
      summarise(pr = min(time_sec), .groups = "drop")
    report("WARNING", "State entry's seed doesn't match our 5K PR (faster: a race we don't have; slower: check our time)",
           season$state_entries %>% inner_join(pr, by = c("gender", "name", "school")) %>%
             filter(abs(seed_sec - pr) > 0.15) %>%
             transmute(meet, gender, name, school, seed_minus_pr = round(seed_sec - pr, 1)) %>%
             arrange(meet, gender, school))
  }

  cat("\n", errors, " error type(s) found.\n", sep = "")
  invisible(errors)
}

# Pairs of names at the same school and gender that differ by 1-2 letters,
# e.g. "JON SMITH" / "JOHN SMITH".
similar_names <- function(races) {
  runners <- races %>% distinct(gender, school, name)
  pairs <- list()
  for (grp in split(runners, list(runners$gender, runners$school), drop = TRUE)) {
    if (nrow(grp) < 2) next
    d <- adist(grp$name)
    close <- which(d > 0 & d <= 2 & upper.tri(d), arr.ind = TRUE)
    if (nrow(close) == 0) next
    pairs[[length(pairs) + 1]] <- tibble(
      gender = grp$gender[1], school = grp$school[1],
      name_1 = grp$name[close[, 1]], name_2 = grp$name[close[, 2]]
    )
  }
  bind_rows(pairs, tibble(gender = character(), school = character(),
                          name_1 = character(), name_2 = character()))
}
