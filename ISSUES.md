# ISSUES.md

Known issues, unused files, and cleanup opportunities in this repository.

## Empty Directory

- **`2024/`** - Completely empty folder (no files)

## Duplicate/Redundant Files

- **`2023/Data/sf_christian_boys.csv`** and **`2023/Data/sioux_falls_christian_boys.csv`** - Identical content
- **`2023/Data/sf_christian_boys.txt`** and **`2023/Data/sioux_falls_christian_boys.txt`** - Identical content
- Note: `sf_christian` has no matching girls file, while `sioux_falls_christian` does

## Misspelled File Names

- **`2023/Data/colman_egan_girls.csv`** - Should be "coleman" (other files use "coleman_egan")
- **`2025/Data/duel_boys.txt`** / **`duel_girls.txt`** - Should be "deuel" (the school is Deuel)
- **`Research/ranking_algoithm_notes.docx`** - "algorithm" is misspelled

## Orphaned TXT Files (no CSV equivalent)

- **`2023/Data/eldon_boys.txt`** / **`eldon_girls.txt`** - Have no matching CSV, not referenced in meet list

## Files in Wrong Location

The following `df_*` files are in `2023/Data/` but belong in `2023/Simulation/`:
- `df_name.csv`
- `df_points.csv`
- `df_schools.csv`
- `df_points_alcester.csv`
- `df_points_augie.csv`
- `df_points_beresford.csv`
- `df_points_big_east.csv`
- `df_points_canton.csv`
- `df_points_chamberlain.csv`
- `df_points_dakota12.csv`
- `df_points_dakota_valley.csv`
- `df_points_lennox.csv`
- `df_points_mccook.csv`
- `df_points_scotland.csv`
- `df_points_sf_christian.csv`
- `df_points_viborg.csv`

## Source Files (Could Be Archived)

### PNG Screenshots (37 files in 2023/Data/)
Used for manual data entry, could be moved to an archive folder:
- `2023 Pierre Invite Results_1.png` through `_6.png`
- `chamberlain_girls_pg1.PNG`, `chamberlain_girls_pg2.PNG`
- `clark_boys.png`, `clark_girls.png`
- `esd_boys1.PNG`, `esd_boys2.PNG`, `esd_girls1.PNG`, `esd_girls2.PNG`
- `ethan_parkston_boys1.PNG`, `ethan_parkston_boys2.PNG`, `ethan_parkston_girls.PNG`
- `freeman_flyer_boys.PNG`, `freeman_flyer_girls.PNG`
- `howard_cornbelt_conference.PNG`
- `jones_county_boys.PNG`, `jones_county_girls.PNG`
- `lake_region_conference_boys.PNG`, `lake_region_conference_girls.PNG`
- `miller_boys.PNG`, `miller_girls.PNG`
- `nec_girls.PNG`
- `plankinton_boys.PNG`, `plankinton_girls.PNG`
- `sesd_boys.PNG`, `sesd_girls.PNG`
- `sioux_valley_boys.PNG`, `sioux_valley_girls.PNG`
- `todd_county_boys.PNG`, `todd_county_girls.PNG`
- `viborg_girls.PNG`

### PDF Source Documents (39 files in 2023/Data/)
Original meet results in PDF format, could be moved to an archive folder.

### DOCX Files in Data Folder
- `2023/Data/coleman_egan.docx`
- `2023/Data/Groton Invite Results.docx`

## Generated Reports (Potentially Temporary)

- **`2025/Report_High_School_Cross_Country_Results_Analysis_09-14-2025-09_25PM.html`** (632KB)
- **`2025/RMD_09-14-2025-09_19PM.Rmd`** - R Markdown source for above

## Old Research Documents

The `Research/` folder contains 2023 STAT 651 class project documents:
- `Final_Proposal_XC_Rankings.docx` / `.pdf`
- `XC_Rankings_Write_Up.docx` / `.pdf`
- `STAT651_XC_Rankings.pptx`
- `ranking_algoithm_notes.docx` (misspelled)

These may no longer be needed for active development.

## Code Issues

### Hardcoded/Stale Code
- **`2023/read_other_results.R`** - Contains hardcoded OneDrive paths and non-working PDF parsing code (lines 38-50 wrapped in `if(TRUE == FALSE)`)
- **`accesory.r`** - Uses undefined `path` variable (line 3)

### Unused Reference File
- **`out_of_state_schools.txt`** - Simple comma-separated list not referenced by any script (filtering done via `list_schools.csv` instead)

## Archived Files

- **`2023/Simulation/Archive/list_schools.rds`** - Old version of school list
