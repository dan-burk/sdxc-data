# CLAUDE.md

Ranks South Dakota high school cross country runners from meet results. Written in R. The owner is a statistician; keep code plain and easy to read.

## How it works

Every run rescores the whole season from the curated files. There is no saved state, so late results only need adding and a rerun.

```
convert.r   source TXT / saved MileSplit page -> {year}/Data/{meet}_{boys|girls}.csv   (rebuilds all)
run.r       load -> check -> score -> {year}/output/rankings_{boys|girls}.csv (one ranking per week)
export.r    rankings CSVs -> ../sdxc/data/{boys|girls}_{year}_week{N}.json + schools_{year}.json for the website (sister repo)
```

- `R/load.r`: reads the files and applies school and athlete aliases.
- `R/check.r`: data checks. ERRORs stop the run; WARNINGs don't.
- `R/score.r`: scoring and weekly ranking.
- `R/convert.r`: TXT and MileSplit parsers.

Run from the repo root with Windows R: `"/mnt/c/Program Files/R/R-4.5.3/bin/x64/Rscript.exe" run.r`. `/run-sdxc` runs it, fixes what the check reports, then runs export.r.

## Curated files (the owner maintains these)

| File | Scope | Contents |
|---|---|---|
| `{year}/meet_list.csv` | per year | Every meet: `meet` (file stem), `date`, `week`, `flg_5k`, `missing`, `alternative` (1 = results from the MileSplit URLs in `alternative_boys/girls`) |
| `{year}/schools.csv` | per year | In-state schools, their class (AA/A/B) and region (1A–5A, 1B–5B; blank for AA); classes, regions and co-ops change yearly. Regions come from GoBound's teams page (`gobound.com/sd/sdhsaa/boyscrosscountry/{2026-27}/Teams?sortby=Region`, and girls), checked by hand once per season. |
| `school_aliases.csv` | all years | raw school name -> standard name, `OUT` (out of state) or `DROP` (not a school). Never delete rows. |
| `{year}/athlete_aliases.csv` | per year | `name,school,correct_name` for misspelled runners (school = standardized name) |
| `{year}/different_athletes.csv` | per year | Similar-name pairs confirmed to be different people |
| `school-name-notes.md` | all years | Co-ops, renames, closures |

Source results are `{year}/Data/{meet}_{g}.txt` (or `_milesplit.txt`), plus `{year}/merged_meets.csv` for one race published as two meets. **The Data CSVs are generated; never hand-edit them.** Fix the source TXT or the parser, then run convert.r.

Don't edit `meet_list.csv` without asking. Don't change the scoring code to work around bad data.

## Scoring

- Athlete = name + school, after aliases. `OUT`/`DROP` runners and DQs are removed before scoring. Row order in a CSV is finish order.
- Everyone starts at 1000. In each race every runner takes 5% of the current points of every runner behind them, one pair at a time in finish order.
- PRs only come from `flg_5k = 1` meets. Meets run in order of week, date, then meet-list row. A snapshot is taken at the end of each week.
- Rank = average of the points rank and the PR rank (points rank alone if no PR).

## Skills

- `convert-sdxc`: convert new TXT results to CSV and verify the parse.
- `run-sdxc`: run and fix until clean.
- `school-matching`: unknown schools.
- `athlete-name-matching`: misspelled runners.

## Data quirks seen before (check for these again)

- **One race published as two meets.** 2025 ECC and LCC Conference ran as one combined race on 10/6. Each conference published its own results, and Deuel and Clark/Willow Lake appeared in both files with identical times. Fix: row `ecc,lcc` in `2025/merged_meets.csv` (convert.r merges, dedupes, sorts by time), LCC set to `missing = 1` in the meet list. check_season() now errors on "Same race entered as two meets".
- **Rows out of finish order.** 2025 Viborg: two sorted blocks glued together. Fixed by reordering the runner blocks in the TXT.
- **Time typed with a colon.** 2025 Wagner: `16:38:48` means `16:38.48`. The parser now converts this.
- **Name and school merged in one field.** The initials line was `A(` for "Addison (Addi) Muth". The parser now recognises it and drops nicknames.
- **Varsity runners labelled "MS".** Dupree MS, Highmore MS: alias to the school, don't drop.
- **TXT runner with no `Yr:` line.** The old parser swallowed the next runner (2025 Murdo and Todd County girls, fixed in R/convert.r). A runner with no time is kept with a blank time and not scored.
- **Typo dates in the meet list.** 2025 State AA was entered as 2525.
- **Two spellings, two grades, one runner.** Timing companies copy roster errors to every meet they time (Jonathan Walters gr 10 / Watters gr 8). Trust the postseason race.

## Other folders

- `2023/`: the older, messier season. Not scored by this pipeline. Its data is used as evidence when matching schools.
- `Research/`: 2023 STAT 651 class project on the ranking method.
