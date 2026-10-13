# ISSUES.md

Open issues. The 2025 pipeline's data checks live in `R/check.r`, and past data problems are listed in CLAUDE.md.

## 2025

- **`humboldt` meet has no South Dakota runners** (all Iowa schools). Check whether it belongs in the meet list.
- **`sisseton_{boys,girls}.txt` have no CSV and aren't in the meet list.**
- **Some 5K meets look short** (runners 7–13% faster than their usual times: Castlewood, Wall, Region 4B, Howard). Revisit together with the PR part of the ranking.

## 2023

- No results for Potter County, Flandreau, Gettysburg Pre-Region. Platte-Geddes was cancelled. Howard boys set aside (`howard_boys_unverified.txt`).
- 40 meets are still hand-typed (`2023/Data/manual/`) and carry small typos. Faith, Madison and Mitchell fit runners' other races worst; replace with TXTs if they turn up.
- Old pipeline files: `2023/Data/df_*.csv`, `2023/Simulation/`, `read_dakotatiming_v2.r`, `read_other_results.R`, `meet_list_data_ready.xlsx`, `SD_XC_Meet_List.xlsx` (has coach contact details).
- PNG, PDF, DOCX and XLSX source files in `2023/Data/` could move to an archive folder.
