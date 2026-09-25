# Rebuild every Data CSV (Place, Name, School, Time, Grade) from its source.
# Always reconverts, so a fix made in a source file always reaches the CSV.
# Fix data problems in the SOURCE files, never in the CSVs (they're overwritten):
#   {year}/Data/{meet}_{boys|girls}.txt            timing-company results
#   {year}/Data/{meet}_{boys|girls}_milesplit.txt  saved MileSplit page (alternative = 1;
#                                                  downloaded from the meet list URL once)
#   {year}/merged_meets.csv                        meets published as two files but run as one race

suppressPackageStartupMessages(library(dplyr))
source("R/convert.r")

year <- 2025

meets  <- readxl::read_xlsx(file.path(year, "meet_list.xlsx")) %>% filter(missing == 0)
merges <- read.csv(file.path(year, "merged_meets.csv"), colClasses = "character")

for (i in seq_len(nrow(meets))) {
  m <- meets$meet[i]
  for (g in c("boys", "girls")) {
    url <- if (isTRUE(meets$alternative[i] == 1)) meets[[paste0("alternative_", g)]][i] else NA
    results <- read_meet_source(year, m, g, url)
    if (is.null(results)) { message("No source file for ", m, "_", g); next }

    for (other in merges$merge_from[merges$meet == m]) {
      results <- merge_races(rbind(results, read_meet_source(year, other, g)))
    }
    write.csv(results, file.path(year, "Data", paste0(m, "_", g, ".csv")), row.names = FALSE)
  }
}
message("Converted ", nrow(meets), " meets.")
