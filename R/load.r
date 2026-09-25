# Read one season's curated files and race results into a single list.
#
# Curated by hand:
#   school_aliases.csv          raw_school -> school, OUT (out of state) or DROP (not a school)
#   {year}/meet_list.xlsx       every meet: meet, date, week, flg_5k, missing
#   {year}/schools.csv          in-state schools and their class this year
#   {year}/athlete_aliases.csv  name + school -> correct_name, for misspelled runners
#   {year}/Data/{meet}_{boys|girls}.csv   Place, Name, School, Time, Grade

load_season <- function(year) {
  meet_list <- readxl::read_xlsx(file.path(year, "meet_list.xlsx")) %>%
    mutate(date = as.Date(date), list_row = row_number())

  season <- list(
    year            = year,
    meet_list       = meet_list,
    schools         = read_curated(file.path(year, "schools.csv")),
    school_aliases  = read_curated("school_aliases.csv"),
    athlete_aliases = read_curated(file.path(year, "athlete_aliases.csv")) %>%
      mutate(name = toupper(str_squish(name)), correct_name = toupper(str_squish(correct_name)))
  )
  season$races <- read_races(year, meet_list) %>% clean_races(season)
  season
}

read_curated <- function(path) {
  read.csv(path, colClasses = "character", strip.white = TRUE)
}

# All result files for meets not marked missing, stacked into one table.
# Row order within a file is finish order.
read_races <- function(year, meet_list) {
  races <- list()
  for (m in meet_list$meet[meet_list$missing == 0]) {
    for (g in c("boys", "girls")) {
      path <- race_path(year, m, g)
      if (!file.exists(path)) next  # check_season() reports these
      races[[path]] <- read.csv(path, colClasses = "character") %>%
        mutate(meet = m, gender = g, row = row_number())
    }
  }
  bind_rows(races)
}

race_path <- function(year, meet, gender) {
  file.path(year, "Data", paste0(meet, "_", gender, ".csv"))
}

# Standardize names, schools and times. Keeps every row; scoring decides
# which ones count (see scorable()).
clean_races <- function(races, season) {
  races %>%
    mutate(
      name       = toupper(str_squish(Name)),
      raw_school = str_squish(School),
      time       = str_squish(Time),
      time_sec   = time_to_sec(time)
    ) %>%
    left_join(season$school_aliases %>% select(raw_school, school), by = "raw_school") %>%
    mutate(school = coalesce(school, raw_school)) %>%
    left_join(season$athlete_aliases, by = c("name", "school")) %>%
    mutate(name = coalesce(correct_name, name)) %>%
    select(meet, gender, row, name, school, raw_school, time, time_sec, grade = Grade)
}

# "16:38.48" -> 998.48 seconds. Anything else (DQ, DNF, blank) -> NA.
time_to_sec <- function(time) {
  parts <- str_match(time, "^(\\d{1,2}):(\\d{2}(?:\\.\\d+)?)$")
  as.numeric(parts[, 2]) * 60 + as.numeric(parts[, 3])
}

# Rows that take part in scoring: in-state runners with a finish time.
scorable <- function(races, schools) {
  races %>% filter(school %in% schools$school, !is.na(time_sec))
}
