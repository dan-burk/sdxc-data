---
name: school-matching
description: Resolve unknown or misspelled school names in South Dakota XC meet results by curating school_aliases.csv and {year}/schools.csv. Use this whenever run.r / check_season() reports "Unknown school", when new meet CSVs are added, when the user asks whether a school is in-state or out-of-state, asks about co-ops, renamed or merged schools, school classes (AA/A/B), or wants to clean up school names — even if they don't mention the alias file by name.
---

# School matching

Every result row's `School` must resolve to one of:
- an in-state school in `{year}/schools.csv` (columns `school,class`), or
- `OUT` (out-of-state; runners dropped before scoring), or
- `DROP` (not a school: clubs, unattached, "Removed", etc.; runners dropped).

The mapping from raw spellings lives in `school_aliases.csv` at the repo root (`raw_school,school,note`), shared across all years. `{year}/schools.csv` is per year because classes and co-ops change year to year.

Getting this right matters because an unrecognised in-state school silently loses all its runners from the rankings (the old pipeline dropped ~400 SF Lincoln/Washington/Christian/Roosevelt runs this way), and a wrong alias merges two teams' athletes.

## Workflow

1. **List the unknowns with evidence.** From the repo root:
   ```
   Rscript .claude/skills/school-matching/scripts/school_evidence.r <year>
   ```
   On this machine Rscript is `"/mnt/c/Program Files/R/R-4.5.3/bin/x64/Rscript.exe"` (Windows R, run from the repo directory).
   Columns: `athletes`, `results`, `grades`, `postseason` (region/state meets; the file name gives the class), `meets`, `pct_out_at_meets`, `shared_with` (known in-state schools with same-named runners: count and years), `similar_known` (similar known names and aliases, shown as `raw -> target`, which catches truncations of existing aliases).

2. **Decide each one** using the evidence, in this order of strength:
   - **Shared runners** (`shared_with`) — the strongest signal. Several shared names with one school means it's a spelling of that school. One shared name can be a coincidence (common names like "Aiden Johnson").
   - **Where it ran.** Out-of-state teams cluster at border/big invites (augie, heartland_preview, humboldt, rapid_city, belle, sturgis, milbank_bank, lemmon); high `pct_out_at_meets` supports OUT. Region/state meet files (`region2b`, `sdhsaa_a`) only have in-state teams, and the file name tells you the class.
   - **Spelling** (`similar_known`) — the weakest signal. Similar names can be different schools: "Waverly" at Augie is Waverly, Nebraska, not Waverly-South Shore; "Clear Lake" at Augie is Iowa; "Campbell County" at Rapid City is Gillette, WY. Only trust spelling when the other evidence agrees or doesn't contradict it.
   - **`school-name-notes.md`** (repo root) — the owner's notes on co-ops, renames (Potter County → Gettysburg), closures and non-schools. The SDHSAA co-op list (https://sdhsaa.com/athletics-cooperatives/) is the outside reference.

3. **Apply these rules:**
   - An alias means *the same team, spelled differently* (truncations like "McCook Centr", abbreviations like "CEB", "Sioux Falls X" ↔ "SF X", renames like Potter County → Gettysburg).
   - A co-op change makes a *different team*: don't alias "Parker" to "Parker/Marion". If a team is legitimately new for this year, add it to `{year}/schools.csv` instead.
   - Labels like "Dupree MS" or "Highmore MS" on runners who also run for the varsity school are the varsity school — alias them, don't DROP. Check `shared_with` and grades.
   - An alias target must be `OUT`, `DROP`, or a school in that year's `schools.csv`. Aliases for past years can point at a past-year school name.
   - Don't guess a class. Use the region/state meet the school ran in; if it ran none, ask the user.
   - If raw school text contains a runner's name (e.g. "Addison (Addi) Muth Yankton"), that's a parsing error in the data file. Fix the CSV row (Name/School) rather than adding an alias.

4. **Write the changes.** Only add or correct rows; never delete alias rows, even ones that don't appear in the current year's data. Old spellings come back in future results and must keep mapping to the standard names.
   - Append rows to `school_aliases.csv`. Quote values containing commas. Put a short `note` when the reason isn't obvious (e.g. "same runners as BHCA", "Waverly NE at Augie").
   - Keep the file sorted: in-state targets A–Z, then DROP, then OUT (sort by target, then raw name).
   - New in-state schools go in `{year}/schools.csv` as `school,class`.

5. **Verify.** Run `Rscript run.r` (with `year` set in run.r) and confirm there are no "Unknown school" or "Alias points to a school not in schools.csv" errors.

6. **Report back** as a short table: raw name → decision, evidence, confidence. List anything you were unsure about separately so the user can make the call. The user curates these files, so they need to see and agree with anything that isn't clear-cut.
