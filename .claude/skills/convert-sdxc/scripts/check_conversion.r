# Check that meet TXT files were converted into CSVs correctly.
# Usage (from repo root):
#   Rscript .claude/skills/convert-sdxc/scripts/check_conversion.r 2025              # every meet
#   Rscript .claude/skills/convert-sdxc/scripts/check_conversion.r 2025 wall howard  # only these meets
#
# For each meet/gender with both a TXT and a CSV it reports:
#   COUNT     TXT finishers (place-number lines) vs CSV rows differ -> runners lost or invented
#             (e.g. a runner listed with no time in the source is skipped by the parser)
#   PROBLEM   CSV rows that look mis-parsed (initials as name, "(" or blank school,
#             unreadable time, finishers out of order)
#   NOT SCORED  DQ / DNF / DNS / no-time rows: kept in the CSV, dropped by run.r before scoring

suppressPackageStartupMessages(library(dplyr))
library(stringr)
options(width = 200)

args  <- commandArgs(trailingOnly = TRUE)
year  <- if (length(args) >= 1) args[1] else "2025"
only  <- args[-1]
meets <- readxl::read_xlsx(file.path(year, "meet_list.xlsx")) %>% filter(missing == 0)
if (length(only)) meets <- meets %>% filter(meet %in% only)

status <- c("DQ", "DNF", "DNS", "")  # "" = listed with no time
problems <- list()
not_scored <- list()

for (m in meets$meet) {
  for (g in c("boys", "girls")) {
    txt <- file.path(year, "Data", paste0(m, "_", g, ".txt"))
    csv <- file.path(year, "Data", paste0(m, "_", g, ".csv"))
    if (!file.exists(csv)) { cat("MISSING CSV ", basename(csv), "\n"); next }
    r <- read.csv(csv, colClasses = "character")

    if (file.exists(txt)) {
      lines <- trimws(readLines(txt, warn = FALSE))
      entries <- sum(grepl("^\\d+$", lines))
      if (entries != nrow(r)) {
        cat(sprintf("COUNT       %-28s TXT %3d entries, CSV %3d rows\n", paste0(m, "_", g), entries, nrow(r)))
      }
    }

    time_ok <- grepl("^\\d{1,2}:\\d{2}(\\.\\d+)?$", r$Time)
    secs <- rep(NA_real_, nrow(r))
    secs[time_ok] <- as.numeric(sub(":.*", "", r$Time[time_ok])) * 60 + as.numeric(sub(".*:", "", r$Time[time_ok]))
    why <- case_when(
      grepl("^[A-Z]{1,3}\\(?$", r$Name) | r$Name == ""           ~ "name looks like initials/blank",
      grepl("\\(", r$School) | r$School == ""                     ~ "school has '(' or is blank",
      !time_ok & !toupper(r$Time) %in% status                     ~ "time can't be read",
      !is.na(secs) & secs < cummax(ifelse(is.na(secs), 0, secs))  ~ "faster than the row above",
      TRUE ~ NA_character_
    )
    bad <- which(!is.na(why))
    if (length(bad)) problems[[length(problems) + 1]] <-
      tibble(file = paste0(m, "_", g), row = bad, why = why[bad], r[bad, c("Place", "Name", "School", "Time", "Grade")])

    dq <- which(toupper(r$Time) %in% status)
    if (length(dq)) not_scored[[length(not_scored) + 1]] <-
      tibble(file = paste0(m, "_", g), r[dq, c("Name", "School", "Time")])
  }
}

cat("\nPROBLEM rows:\n")
if (length(problems)) print(as.data.frame(bind_rows(problems)), row.names = FALSE, right = FALSE) else cat("  none\n")
cat("\nNOT SCORED (DQ/DNF/DNS/no time, dropped by run.r):\n")
if (length(not_scored)) print(as.data.frame(bind_rows(not_scored)), row.names = FALSE, right = FALSE) else cat("  none\n")
