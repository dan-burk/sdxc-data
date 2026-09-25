---
name: run-sdxc
description: Score the South Dakota XC season by running run.r, then fix whatever the data check reports (unknown schools, misspelled runners, bad rows) and rerun until it's clean. Use when the user types /run-sdxc, or asks to run, rerun, score or rescore the meets/season/rankings.
---

# Run SDXC scoring

## 1. Run it

From the repo root:
```
"/mnt/c/Program Files/R/R-4.5.3/bin/x64/Rscript.exe" run.r
```
This is the same as `source("run.r")` in R. It checks the data first. If any ERROR is found it stops without scoring. Otherwise it writes `{year}/output/rankings_{boys,girls}.csv` (one ranking per week). The season is set by `year <- ...` in run.r.

## 2. Handle what it reports

Handle everything you can without asking, then rerun. Repeat until it prints `0 error type(s) found.` and has no warnings you can act on.

| Message | What to do |
|---|---|
| `Unknown school` | Follow the **school-matching** skill |
| `Possible misspelled runner` | Follow the **athlete-name-matching** skill |
| `Same runner twice in one race` | Usually a wrong athlete alias; check `{year}/athlete_aliases.csv` first (athlete-name-matching skill). Otherwise it's a duplicated row in that meet's CSV. |
| `Athlete alias matches no runner` | The alias uses a raw school name or a typo. Fix the row (school must be the standardized name). |
| `Rows not in finish order` | The CSV rows are shuffled (e.g. two sorted blocks glued together). If `Place` is correct, sort the file by `Place`; otherwise ask. |
| `Time can't be read` | Fix obvious format slips in the CSV (e.g. `16:38:48` → `16:38.48`). Ask if the right time isn't clear. |
| `Blank name or school` | Check the file's header is `Place,Name,School,Time,Grade`, and check for merged fields like `"A(","Addison (Addi) Muth Yankton"`. |
| `Meet not marked missing but has no result files` | Ask the user: either the files are coming, or set `missing = 1` in `{year}/meet_list.xlsx`. Don't edit the xlsx yourself. |
| Meet list / schools.csv errors (duplicates, bad dates, bad class) | These are the user's curated files; propose the fix and ask before changing class or meet data. |
| `Result file not in the meet list`, `only one gender's results` | Just mention these; no action needed. |

Rules that apply throughout:
- Fix data problems in the curated files or the CSVs, never by changing the R code.
- Never delete rows from `school_aliases.csv`; only add or correct them.
- Don't guess on uncertain calls (a school's class, whether two runners are the same person). Leave them for the user.

## 3. Report

Keep it short:
- Clean run: say so, give the latest week, and list the top 5 boys and girls from the output files.
- What you changed, as a short list (file → change), for the user to review before committing.
- Anything you left for the user to decide.

Don't commit; the user reviews first.
