---
name: state-entries
description: Load the South Dakota XC state meet entry lists (state_entries_*.txt) so the site can show predicted state places before state is run, and use them as a data check. Use when the user adds state entry TXT files, asks who's running state, wants pre-state predictions, or asks why a state entry's seed time doesn't match our data.
---

# State meet entries

The entries say **who is in each state race**. That is all the pipeline needs from them; the seed time is only a data check.

1. The owner saves six tab-separated files: `{year}/Data/state_entries_{aa|a|b}_{boys|girls}.txt` (grade, name, status, seed, blank, school, region qualification).
2. `Rscript convert.r {year}` writes `{year}/Data/state_entries.csv` (`meet,gender,Name,School,Seed`). Nicknames in parentheses are dropped, as in results.
3. `Rscript run.r {year}`. Aliases apply to entries as to results. Two WARNINGs:
   - **State entry not in this season's results**: a spelling difference (add a row to `athlete_aliases.csv`, e.g. `Muzyk Grayeagle` -> `Muzyk Gray Eagle`, or to `school_aliases.csv`), or a runner with no races we have (leave it; they get no prediction).
   - **Seed doesn't match our 5K PR**: the seed is the runner's 5K season best on the entry site. Faster than ours = a race we don't have (look for a missing meet). Slower = check our time. Report these to the owner; don't change data to make them match.
4. `Rscript export.r {year}`. Until state results exist, the state week JSON holds the entrants with `predicted` and `predicted_pr` and null `place`/`state_time`, and `state_teams_*` has predicted team places with null `place`, `score`, `scorers`, `displacers`. Once the state results are added (set `missing = 0`), they replace this automatically.

Note: export.r then sets that year's latest week to the state week in `manifest.json`, so the live site opens on the state page once the data is pushed.
