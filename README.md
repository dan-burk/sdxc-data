# SD XC Data Processing

Weekly rankings for South Dakota high school cross country.

1. Add results: put `{meet}_{boys|girls}.txt` in `2025/Data/`, add the meet to `2025/meet_list.csv`, then `source("convert.r")`. It rebuilds every CSV from the TXTs, so fix problems in the TXT, never the CSV.
2. Score: `source("run.r")`. It checks the data first and tells you what to fix.
3. Rankings: `2025/output/rankings_boys.csv` and `rankings_girls.csv`. They're generated, so not in git; rerun `run.r` to recreate them.
4. Website: `Rscript export.r 2025` writes the JSON the site reads into `../sdxc/data/`.

See `CLAUDE.md` for how it works.
