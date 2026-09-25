# CLAUDE.md

Ranks South Dakota high school cross country runners from meet results. Written in R. The owner is a statistician; keep code plain and easy to read.

## How it works

Every run rescores the whole season from the curated files. There is no saved state, so late results only need adding and a rerun.

```
convert.r   raw TXT / MileSplit URL -> {year}/Data/{meet}_{boys|girls}.csv   (only meets with no CSV yet)
run.r       load -> check -> score -> {year}/output/rankings_{boys|girls}.csv (one ranking per week)
```

- `R/load.r`: reads the files and applies school and athlete aliases.
- `R/check.r`: data checks. ERRORs stop the run; WARNINGs don't.
- `R/score.r`: scoring and weekly ranking.
- `R/convert.r`: TXT and MileSplit parsers.

Run from the repo root with Windows R: `"/mnt/c/Program Files/R/R-4.5.3/bin/x64/Rscript.exe" run.r`. `/run-sdxc` runs it and fixes what the check reports.

## Curated files (the owner maintains these)

| File | Scope | Contents |
|---|---|---|
| `{year}/meet_list.xlsx` | per year | Every meet: `meet` (file stem), `date`, `week`, `flg_5k`, `missing`, `alternative` (1 = results from the MileSplit URLs in `alternative_boys/girls`) |
| `{year}/schools.csv` | per year | In-state schools and their class (AA/A/B); classes and co-ops change yearly |
| `school_aliases.csv` | all years | raw school name -> standard name, `OUT` (out of state) or `DROP` (not a school). Never delete rows. |
| `{year}/athlete_aliases.csv` | per year | `name,school,correct_name` for misspelled runners (school = standardized name) |
| `{year}/different_athletes.csv` | per year | Similar-name pairs confirmed to be different people |
| `school-name-notes.md` | all years | Co-ops, renames, closures |

Don't edit `meet_list.xlsx` without asking. Fix data problems in these files or the Data CSVs, not in the R code.

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

- **One race published as two meets.** 2025 ECC and LCC Conference ran as one combined race on 10/6. Each conference published its own results, and Deuel and Clark/Willow Lake appeared in both files with identical times. Fix: merge into one file (dedupe, sort by time, renumber Place), keep it under one meet (`ecc`), set the other to `missing = 1` in the meet list. check_season() now errors on "Same race entered as two meets".
- **Rows out of finish order.** 2025 Viborg: two sorted blocks glued together. Sort by Place if Place is right.
- **Time typed with a colon.** 2025 Wagner: `16:38:48` means `16:38.48`.
- **Name and school merged in one field.** e.g. `"A(","Addison (Addi) Muth Yankton"`: split into Name/School.
- **Varsity runners labelled "MS".** Dupree MS, Highmore MS: alias to the school, don't drop.
- **TXT runner with no `Yr:` line.** The old parser swallowed the next runner (2025 Murdo and Todd County girls, fixed in R/convert.r). A runner with no time is kept with a blank time and not scored.
- **Typo dates in the meet list.** 2025 State AA was entered as 2525.
- **Two spellings, two grades, one runner.** Timing companies copy roster errors to every meet they time (Jonathan Walters gr 10 / Watters gr 8). Trust the postseason race.

## Other folders

- `2023/`: the older, messier season. Not scored by this pipeline. Its data is used as evidence when matching schools.
- `Research/`: 2023 STAT 651 class project on the ranking method.
