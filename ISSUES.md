# ISSUES.md

Open issues. The 2025 pipeline's data checks live in `R/check.r`, and past data problems are listed in CLAUDE.md.

## 2025

- **`humboldt` meet has no South Dakota runners** (all Iowa schools). Check whether it belongs in the meet list.
- **`sisseton_{boys,girls}.txt` have no CSV and aren't in the meet list.**
- **Some 5K meets look short** (runners 7–13% faster than their usual times: Castlewood, Wall, Region 4B, Howard). Revisit together with the PR part of the ranking.

## 2023 (not yet in the new pipeline)

- Result CSVs have mixed column layouts; they need `Place,Name,School,Time,Grade` before `run.r` can score 2023.
- `sf_christian_boys` and `sioux_falls_christian_boys` are identical (CSV and TXT).
- `colman_egan_girls.csv` vs `coleman_egan_*` naming.
- `eldon_{boys,girls}.txt` have no CSV and aren't in the meet list.
- Old pipeline files: `2023/Data/df_*.csv`, `2023/Simulation/`, `read_dakotatiming_v2.r`, `read_other_results.R`.
- 37 PNG, 39 PDF and 2 DOCX source files in `2023/Data/` could move to an archive folder.
- One Faith result row has a blank school.
