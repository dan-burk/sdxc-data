---
name: convert-sdxc
description: Convert new South Dakota XC meet results (TXT files or MileSplit URLs) into Data CSVs with convert.r, then verify every runner parsed correctly and that DQ/DNF/no-time runners won't be scored. Use when the user types /convert-sdxc, adds new meet TXT files, adds meets to the meet list, or asks whether results parsed/converted correctly. Run this before run-sdxc whenever there are new results.
---

# Convert meet results to CSV

`convert.r` turns `{year}/Data/{meet}_{boys|girls}.txt` (or, for meets with `alternative = 1`, the MileSplit URLs in the meet list) into `{meet}_{boys|girls}.csv` with columns `Place,Name,School,Time,Grade`. It **only converts meets with `missing = 0` that have no CSV yet**. Existing CSVs are never overwritten, because many were fixed by hand (see "Data quirks" in CLAUDE.md).

## 1. Convert

From the repo root:
```
"/mnt/c/Program Files/R/R-4.5.3/bin/x64/Rscript.exe" convert.r
```
It prints one line per CSV it writes. If it writes nothing, every listed meet already has a CSV. If it says "No TXT for …", the meet is in the list without its TXT file: tell the user.

## 2. Verify

```
"/mnt/c/Program Files/R/R-4.5.3/bin/x64/Rscript.exe" .claude/skills/convert-sdxc/scripts/check_conversion.r <year> [meet ...]
```
Pass the meets you just converted (or nothing, for all meets). It reports:

- **COUNT**: the TXT has a different number of finishers (place-number lines) than the CSV. Runners were lost or invented. Find which place is missing and why (compare the TXT block with its neighbours). Expected exceptions: `ecc` (ECC and LCC merged into one race by hand).
- **PROBLEM**: rows that look mis-parsed:
  - *name looks like initials*: the initials line wasn't recognised, so name and school shifted into the wrong columns. Fix the row by hand, and fix the parser if it's a new pattern.
  - *school has "("*: a runner's name leaked into the school.
  - *time can't be read*: fix obvious slips like `16:38:48` → `16:38.48`.
  - *faster than the row above*: rows out of finish order.
- **NOT SCORED**: DQ / DNF / DNS / blank-time rows. These stay in the CSV (the placing is real information) and `run.r` drops them before scoring, so they neither gain nor lose points. Glance at the list: a real runner with a time should never be here.

A CSV is fine to hand-edit after conversion. Keep the header, and keep the rows in finish order.

## 3. Parser notes (`R/convert.r`)

The TXT format per runner is: place, optional initials (`AB`, or `A(` when a nickname follows), name, school, time (or `DQ`, or nothing), then `Yr: N …` (sometimes missing). Known cases the parser handles:
- The initials can themselves be `DQ` (Dean Quiett). Only a `DQ` in the time slot is a disqualification.
- Nicknames: "Addison (Addi) Muth" becomes "Addison Muth".
- A missing `Yr:` line, which used to swallow the next runner (Molly Nix, Adrienne White Lance in 2025).
- A runner with no time: kept with a blank time and not scored (Cooper Jarding, Cornbelt 2025).

If you change the parser, re-parse every TXT and compare with the existing CSVs to be sure nothing else changes unexpectedly. Hand-fixed files (viborg, ecc, wagner) will differ, and that's expected.

## 4. Then

Run the **run-sdxc** skill to check and score. Report what was converted, what you fixed, and anything left for the user.
