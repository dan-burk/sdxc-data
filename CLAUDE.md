# CLAUDE.md

Ranks South Dakota high school cross country runners from meet results. Written in R. The owner is a statistician; keep code plain and easy to read.

## How it works

Every run rescores the whole season from the curated files. There is no saved state, so late results only need adding and a rerun.

```
convert.r   source TXT / saved MileSplit page -> {year}/Data/{meet}_{boys|girls}.csv   (rebuilds all)
run.r       load -> check -> score -> {year}/output/rankings_{boys|girls}.csv (one ranking per week)
export.r    rankings CSVs -> ../sdxc/data/{boys|girls}_{year}_week{N}.json + results_{boys|girls}_{year}.json (every race of each ranked runner) + state_teams_{boys|girls}_{year}.json (state team places) + schools_{year}.json for the website (sister repo)
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
- Rank = course-adjusted time. Model: log(time) = runner + meet. The meet effect is how much faster or slower runners ran there than their usual (course, distance, weather), so every meet counts, 5K or not. It's the median over the meet's linked runners (those who also raced another meet so far); a meet with fewer than 5 linked runners isn't adjusted (0) until more of its runners race again. Adjusted time = average time with meet effects removed, on the scale of the median 5K meet.
- Head-to-head (`head_to_head()` in R/score.r): within a class, a runner who has beaten someone up to 5 places above them every time they raced, at least twice, is ranked ahead of them. adj_time is unchanged, so a runner can rank ahead of a faster adjusted time. Added in 2026; `analysis/h2h_swaps.r` showed it neither helps nor hurts predictions.
- At the end of each week the whole season so far is refitted and ranked. PR (fastest `flg_5k = 1` time) is shown but not used in the rank.
- Replaced the 2025 points method (5% wager blended with PR) in 2026 after `analysis/` showed it predicts finish places better; see `analysis/README.md`.
- The state meet week (the `sdhsaa_*` meets) isn't scored. Its snapshot is the state finishers: `meet`, `place`, `predicted` (order in the race by rank going in; blank if no race before state) and state `time`. export.r writes it to that week's JSON with different fields, and records the week in `manifest.json` under `state`.

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
- **Same race again, ECC and LCC.** 2023 too (10/2): same fix, `ecc,lcc` in `2023/merged_meets.csv`.
- **Lowercase initials.** `Sv` for Sep van Oosterhout (2023 Gettysburg, Region 4B) merged name and school. Fixed in the TXT as `SV`.
- **Wrong or copied race.** 2023 hand-typed Scotland boys was the JV race, and Howard was a copy of Cornbelt Conference. Check a meet that looks fast or slow against its source.

## Other folders

- `2023/`: the older, messier season, scored by this pipeline since 2026 (see below). Its PDFs, photos, `df_*.csv`, `Simulation/` and the two `read_*.r` scripts are from the old pipeline and aren't used.

## 2023

About 40 meets have no TXT; they were typed by hand from PDFs and photos into `2023/Data/manual/{meet}_{g}.csv` (old layout: row number, Place, Name, Time, School, Class). Those files are the source for those meets. `2023/convert_2023.r` turns them into Data CSVs, so the clean scripts stay 2023-free:

```
Rscript convert.r 2023          TXT meets
Rscript 2023/convert_2023.r     hand-typed meets
Rscript run.r 2023              score
```

- The 2023 timing-company TXTs put the grade on the school line (`Dakota Valley [12]`) and heat/points after the time; the parser reads both.
- Hand-typed files have typos the checks can't see (Clark boys had 6 wrong times out of 38). When the owner can get a meet's TXT, it replaces the hand-typed file: delete `manual/{meet}_*.csv` or convert_2023.r overwrites the TXT's CSV.
- Howard boys isn't scored: its file looked like the Cornbelt race, so it's kept as `howard_boys_unverified.txt` (not read).
- Non-5Ks (4K-ish, 12–21% fast): Pierre, O'Gorman, Mack Butler, Milbank Breathe Easy, Deadwood, Mitchell.
- `Research/`: 2023 STAT 651 class project on the ranking method.
