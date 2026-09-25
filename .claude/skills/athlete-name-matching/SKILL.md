---
name: athlete-name-matching
description: Resolve misspelled or inconsistent runner names in South Dakota XC results by curating {year}/athlete_aliases.csv. Use this whenever run.r / check_season() warns "Possible misspelled runner" or errors "Same runner twice in one race", when the user asks whether two runner names are the same person, sees a duplicate athlete in the rankings, or wants to clean up athlete names — even if they don't mention the alias file.
---

# Athlete name matching

An athlete is identified by **name + school**, after school names are standardized (see the `school-matching` skill; fix schools first, because a school misspelling makes the same runner look like two people).

When the same runner appears under two spellings ("JAXON TOLLEFSON" / "JAXSON TOLLEFSON"), they're scored as two athletes, each with part of the season's points. `{year}/athlete_aliases.csv` (`name,school,correct_name`) maps the wrong spelling to the right one. Matching ignores case and extra spaces; `school` must be the *standardized* school name.

Merging two real people is worse than leaving a duplicate: it mixes their points and PRs, and if they're in the same race it corrupts the point totals (check_season() stops with "Same runner twice in one race"). So when in doubt, don't merge; ask.

## Workflow

1. **Get the pairs with evidence.** From the repo root:
   ```
   Rscript .claude/skills/athlete-name-matching/scripts/name_evidence.r <year>
   ```
   On this machine Rscript is `"/mnt/c/Program Files/R/R-4.5.3/bin/x64/Rscript.exe"`.
   For each similar pair it shows race counts, every grade seen, best times, and `same_race`.

2. **Decide each pair:**
   - **`same_race` is not empty → different people.** One person can't finish a race twice. Siblings often differ by one or two letters (Amia/Amya Ward, Keira/Kendra Thorp). Record them in `{year}/different_athletes.csv` (see step 4) so the warning stops.
   - **Grades differ** (e.g. 9 vs 12) → probably different people. If each spelling has its own *consistent* grade across several races, treat it as two people, or ask the user, rather than merging. A single odd grade among many matching ones is just a typo.
   - **Otherwise, same person** if the spellings are plausible typos or variants: a dropped/doubled letter, a truncation ("SCHLOTMA"), a stray digit ("MCCUNE2"), apostrophe/space differences ("DE'SERSA", "THUNDERHAWK"), or nickname versions ("Addison (Addi) Muth"). Similar best times support this.

3. **Pick the correct spelling:** the one used in more races, unless it's obviously broken (digits, truncation, parenthetical nicknames), then use the clean one. Apostrophe/space variants ("DE'SERSA" vs "DESERSA") are both valid; just go with the more frequent. If it's a tie with no clue, pick one and mention it to the user.

4. **Write rows** to `{year}/athlete_aliases.csv`, one per wrong spelling:
   ```
   Lane McCune2,Freeman,Lane McCune
   ```
   Don't add a row twice for the same `name,school`. That's an error, because it duplicates race rows.

   For pairs that are **different people**, add a row to `{year}/different_athletes.csv` (`school,name_1,name_2,note`) so check_season() stops warning about them:
   ```
   Custer,Keira Thorp,Kendra Thorp,ran in the same races; grades 12 and 9
   ```
   Pairs you're unsure about go in neither file. Leave them warning and ask the user.

5. **Verify.** Run `Rscript run.r` (this also rewrites `{year}/output/rankings_*.csv`, which is expected). The merged pairs should disappear from the warnings, and there should be no "Same runner twice in one race" error and no "Athlete alias matches no runner" warning. If "Same runner twice" appears for a pair you merged, they're two people: remove that alias.

6. **Report back** as a short table: pair → merged into / kept separate, with the reason. List uncertain calls separately for the user to decide.

## Limits

- Only names at the same school are compared. A runner who transferred schools becomes a new athlete. That's by design, so don't try to alias across schools.
- Names 3+ letters apart (e.g. "Jon" vs "Jonathan") aren't flagged automatically. If the user points one out, the same evidence rules apply; you can filter the loaded season's races by school to compare.
