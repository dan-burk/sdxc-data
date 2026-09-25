# Rescore a whole season from the curated files. Safe to rerun any time,
# e.g. after late results arrive: add the CSVs, set missing = 0, rerun.

suppressPackageStartupMessages(library(dplyr))
library(stringr)
options(width = 200)
source("R/load.r")
source("R/check.r")
source("R/score.r")

year  <- 2025  # or: Rscript run.r 2026
if (length(commandArgs(trailingOnly = TRUE))) year <- as.numeric(commandArgs(trailingOnly = TRUE)[1])
wager <- 0.05

season <- load_season(year)
if (check_season(season) > 0) stop("Fix the errors above, then rerun.")
if (nrow(season$races) == 0) stop("No results for ", year, " yet: add meets to the meet list and run convert.r.")

dir.create(file.path(year, "output"), showWarnings = FALSE)
for (gender in c("boys", "girls")) {
  rankings <- score_season(season, gender, wager)
  write.csv(rankings, file.path(year, "output", paste0("rankings_", gender, ".csv")),
            row.names = FALSE)
}
