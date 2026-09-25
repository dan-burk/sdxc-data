# Is this meet a 5K? Suggest flg_5k for meets whose flg_5k is blank in the meet list.
# Usage (from repo root):
#   Rscript .claude/skills/convert-sdxc/scripts/check_5k.r 2026            # meets with blank flg_5k
#   Rscript .claude/skills/convert-sdxc/scripts/check_5k.r 2026 all        # every meet (audit)
#   Rscript .claude/skills/convert-sdxc/scripts/check_5k.r 2026 mobridge   # just these meets
#
# How: each runner's time at the meet is compared with their own typical 5K time
# (median at other flg_5k = 1 meets this season; if they have none yet, last season's).
# The meet's pct = median of those ratios. Tested on 2025:
#   known non-5Ks (4K-ish)  75-84%      short 5Ks (Castlewood, Wall)  87-90%
#   all other 5Ks           92-111%
# So: pct < 85 -> 0 (not a 5K), pct >= 92 -> 1 (5K), 85-91 -> ask the user.

suppressPackageStartupMessages(library(dplyr))
library(stringr)
options(width = 200)
source("R/load.r")

args <- commandArgs(trailingOnly = TRUE)
year <- as.numeric(if (length(args) >= 1) args[1] else 2025)
pick <- args[-1]

season <- load_season(year)
runs <- scorable(season$races, season$schools) %>%
  left_join(season$meet_list %>% select(meet, flg_5k), by = "meet")

# Last season's 5K times, for runners with no 5K yet this season (early meets).
last <- tryCatch({
  p <- load_season(year - 1)
  scorable(p$races, p$schools) %>%
    semi_join(p$meet_list %>% filter(flg_5k == 1), by = "meet") %>%
    group_by(gender, name, school) %>%
    summarise(ref_last = median(time_sec), .groups = "drop")
}, error = function(e) tibble(gender = character(), name = character(), school = character(), ref_last = numeric()))

meets <- season$meet_list %>% filter(missing == 0, meet %in% runs$meet)
meets <- if (identical(pick, "all")) meets else if (length(pick)) filter(meets, meet %in% pick) else filter(meets, is.na(flg_5k))
if (nrow(meets) == 0) { cat("No meets to check (none with results and a blank flg_5k).\n"); quit(save = "no") }

rows <- lapply(meets$meet, function(m) {
  ref_now <- runs %>%
    filter(meet != m, flg_5k == 1) %>%
    group_by(gender, name, school) %>%
    summarise(ref_now = median(time_sec), .groups = "drop")
  x <- runs %>%
    filter(meet == m) %>%
    left_join(ref_now, by = c("gender", "name", "school")) %>%
    left_join(last, by = c("gender", "name", "school")) %>%
    mutate(ref = coalesce(ref_now, ref_last)) %>%
    filter(!is.na(ref))
  pct <- if (nrow(x) >= 5) round(100 * median(x$time_sec / x$ref)) else NA
  tibble(
    meet        = m,
    flg_5k_now  = meets$flg_5k[meets$meet == m],
    compared    = paste0(nrow(x), " (", sum(!is.na(x$ref_now)), " this yr)"),
    pct         = pct,
    suggest     = case_when(is.na(pct) ~ "ask: too few runners to compare",
                            pct < 85   ~ "0  (not a 5K)",
                            pct >= 92  ~ "1  (5K)",
                            TRUE       ~ "ask: short 5K or long course?"),
    boys_winner = runs %>% filter(meet == m, gender == "boys") %>% slice_min(time_sec, n = 1, with_ties = FALSE) %>% pull(time) %>% first()
  )
})

bind_rows(rows) %>% as.data.frame() %>% print(row.names = FALSE, right = FALSE)
