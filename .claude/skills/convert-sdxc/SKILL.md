---
name: convert-sdxc
description: Convert new South Dakota XC meet results (TXT files or MileSplit URLs) into Data CSVs with convert.r, then verify every runner parsed correctly, that DQ/DNF/no-time runners won't be scored, and whether each new meet is a 5K (flg_5k). Use when the user types /convert-sdxc, adds new meet TXT files, pastes a meet schedule to add to the meet list, asks whether a meet is a 5K, or asks whether results parsed/converted correctly. Run this before run-sdxc whenever there are new results.
---

# Convert meet results to CSV

`convert.r` rebuilds **every** `{year}/Data/{meet}_{boys|girls}.csv` (`Place,Name,School,Time,Grade`) for meets with `missing = 0`, from the source files:
- `{meet}_{g}.txt`: timing-company results (most meets)
- `{meet}_{g}_milesplit.txt`: saved MileSplit page, for meets with `alternative = 1`. It's downloaded from the meet list URL the first time, then read locally.
- `{year}/merged_meets.csv` (`meet,merge_from,note`): one race published as two meets. The sources are combined, duplicates dropped, rows sorted by time and places renumbered (ECC + LCC 2025).

**The source files are the truth; the CSVs are output.** Never hand-edit a CSV. The next conversion overwrites it. Fix the TXT (or add a merge row, or fix the parser) and reconvert.

## Adding meets to the meet list

When the user pastes a schedule, add rows to `{year}/meet_list.csv` with exactly the 2025 columns: `official_meet_name,meet,date,week,flg_5k,missing,alternative,alternative_boys,alternative_girls`.
- **`meet`**: if the meet ran last season, reuse last season's `meet` name exactly (same host/town, e.g. John Collignon → `madison`, North Central → `bowdle`), so files line up across years. Otherwise use a short lowercase name.
- **`date`**: `YYYY-MM-DD`.
- **`week`**: same scheme as 2025. Week 1 runs from the first meets through the *second* Saturday (2025: Thu Aug 28 to Sat Sep 6; 2026: Thu Aug 27 to Sat Sep 5). After that, each week runs Monday to Saturday.
- **`flg_5k`**: last season's value if it's the same meet; otherwise blank (see step 3). Pierre is never a 5K.
- **`missing`** = 1 until its TXT files arrive; **`alternative`** = 0.

## 1. Convert

From the repo root:
```
"/mnt/c/Program Files/R/R-4.5.3/bin/x64/Rscript.exe" convert.r
```
It prints "No source file for …" for a listed meet without its TXT: tell the user.

## 2. Verify

```
"/mnt/c/Program Files/R/R-4.5.3/bin/x64/Rscript.exe" .claude/skills/convert-sdxc/scripts/check_conversion.r <year> [meet ...]
```
Pass the meets you just converted (or nothing, for all meets). It reports:

- **COUNT**: the TXT has a different number of finishers (place-number lines) than the CSV. Runners were lost or invented. Find which place is missing and why (compare the TXT block with its neighbours). Merged meets are skipped.
- **PROBLEM**: rows that look mis-parsed:
  - *name looks like initials*: the initials line wasn't recognised, so name and school shifted into the wrong columns. Fix the parser if it's a new pattern, otherwise fix the TXT.
  - *school has "("*: a runner's name leaked into the school (same fix).
  - *time can't be read*: fix the time in the TXT, or teach the parser if the pattern could come up again.
  - *faster than the row above*: the TXT's runner blocks are out of order. Reorder the blocks in the TXT (each block starts with the place-number line).
- **NOT SCORED**: DQ / DNF / DNS / blank-time rows. These stay in the CSV (the placing is real information) and `run.r` drops them before scoring, so they neither gain nor lose points. Glance at the list: a real runner with a time should never be here.

## 3. Is it a 5K? (`flg_5k`)

`flg_5k` decides whether a meet's times count as PRs. The user usually says which meets are 5Ks. Fill in only the obvious ones:
- **Same meet as last season:** use last season's `flg_5k`. Meets that weren't 5Ks in 2025 are almost certainly not 5Ks now (Milbank Breathe Easy, North Central/Bowdle, O'Gorman, Mile High Preview/Deadwood, Pierre).
- **Boys winner well under 14:00:** not a 5K. The 2025 non-5K winners ran 13:00–13:42.
- **Otherwise:** leave it blank and ask. `run.r` won't score while any `flg_5k` is blank, so nothing slips through.

## 4. Parser notes (`R/convert.r`)

The TXT format per runner is: place, optional initials (`AB`, or `A(` when a nickname follows), name, school, time (or `DQ`, or nothing), then `Yr: N …` (sometimes missing). Known cases the parser handles:
- The initials can themselves be `DQ` (Dean Quiett). Only a `DQ` in the time slot is a disqualification.
- Nicknames: "Addison (Addi) Muth" becomes "Addison Muth".
- A missing `Yr:` line, which used to swallow the next runner (Molly Nix, Adrienne White Lance in 2025).
- A runner with no time: kept with a blank time and not scored (Cooper Jarding, Cornbelt 2025).
- Grades written as words: `Yr: Jr` becomes 11 (Fr/So/Jr/Sr = 9–12).
- Times typed `16:38:48`: read as `16:38.48` (Wagner 2025).

If you change the parser, run convert.r and look at `git diff` on `{year}/Data/*.csv` to be sure only the intended rows changed.

## 5. Then

Run the **run-sdxc** skill to check and score. Report what was converted, what you fixed, and anything left for the user.
