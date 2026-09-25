# Make Data CSVs for new meets. Only meets with missing = 0 and no CSV yet
# are converted, so CSVs that have been hand-fixed are never overwritten.
# Source per meet: {meet}_{boys|girls}.txt in Data/, or, if the meet list has
# alternative = 1, the MileSplit URLs in alternative_boys / alternative_girls.

suppressPackageStartupMessages(library(dplyr))
source("R/convert.r")

year <- 2025

meets <- readxl::read_xlsx(file.path(year, "meet_list.xlsx")) %>% filter(missing == 0)

for (i in seq_len(nrow(meets))) {
  for (g in c("boys", "girls")) {
    csv <- file.path(year, "Data", paste0(meets$meet[i], "_", g, ".csv"))
    if (file.exists(csv)) next

    if (isTRUE(meets$alternative[i] == 1)) {
      results <- fetch_milesplit_raw(meets[[paste0("alternative_", g)]][i])
    } else {
      txt <- sub("\\.csv$", ".txt", csv)
      if (!file.exists(txt)) { message("No TXT for ", basename(txt)); next }
      results <- txt_to_csv(readLines(txt, warn = FALSE))
    }
    write.csv(results, csv, row.names = FALSE)
    message("Wrote ", csv, " (", nrow(results), " runners)")
  }
}
