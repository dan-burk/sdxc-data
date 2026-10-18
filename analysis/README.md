# Analysis

Research on the ranking method. Separate from the pipeline: scripts here read
`R/load.r` and `R/score.r` but never change them or the season outputs.
Run from the repo root. Tables go to `analysis/output/`, figures to `analysis/figures/`.

## The test

Each week's races are predicted from the ratings after the week before. Within
each race, runners are ordered by rating to get a predicted place.

- **Error** = predicted place - actual place (+ = finished better than predicted)
- **MAE** = mean |error| = on average, how many places off
- Only runners every method can rate are compared (they had run a 5K before),
  re-numbered 1..n in each race.
- **Test sets:** 2025 weeks 2-7 were used to choose the method. The 2025 state
  meet and 2026 were not, so they are the honest check.

## Result

Course-adjusted time replaced the 2025 points method in 2026.

| MAE (places) | 2025 weeks 2-7 | 2025 state | 2026 so far |
|---|---|---|---|
| PR alone | 4.4 | 12.0 | 4.5 |
| Points 5% + PR (2025 method) | 4.9 | 11.9 | 6.8 |
| **Adjusted time** | **3.4** | **9.4** | **3.9** |
| Improvement over the 2025 method | 31% | 21% | 43% |

2026 = weeks 2-4. Meet speed = median over the meet's linked runners (raced another
meet too), with at least 5 linked runners to be adjusted. Median vs mean and the
minimum (1-5) made no real difference; a minimum of 10 hurt.

It is better every week, in every decile of the field, in 10 of 11 regions at
state, and region bias is about half what it was.

## Scripts

- `old_vs_new.r` (about a minute): PR vs the 2025 method vs adjusted time.
  Figures 1-9: headline MAE, % improvement, by week, by decile, by region (MAE
  and bias), by state race, predicted vs actual at state, 2025 course effects.
- `method_search.r` (a few minutes): how the method was chosen. Points at
  wager sizes 0.5-20%, Elo at K 16-1024, each alone and blended with PR, and
  whether blending Elo or PR into adjusted time helps (it doesn't).
  Figures 10-12.
- `state_by_year.r`: individual state MAE for 2023-2025 (PR, adjusted, adjusted
  + swaps). Adjusted beats PR every year; boys gain 16-22%, girls 4-5% in 2023
  and 2024 but 21% in 2025 (the tuning year).
- `state_teams.r`: state team places (`team_places()` in R/score.r, as on the
  site) predicted by PR vs adjusted. Team MAE by race and year in
  `output/state_teams_mae.csv`; adjusted + swaps beats PR in 14 of 18 races.
- `h2h_swaps.r`: head-to-head swaps on top of adjusted time. MAE moves by
  0.01 either way: neither helps nor hurts, kept because it reads right.
- `best_races.r`: rate on the best N races instead of all. Best 1-3 is worse;
  dropping the slowest race (3+ races) is a little better (2025 state 9.44 to
  9.20). Not adopted.
- `head_to_head.r` (about 15 minutes): Plackett-Luce ratings from finish order,
  alone and blended with adjusted time. Never beats adjusted time. Rejected.
- `recency.r`: recent races counting more. A 2-week half-life cut 2025 state
  MAE 5% (9.35 to 8.86). Parked, not in the pipeline yet.
- `improvement.r`: raw times by week, 2025. Runners get faster over the
  season, which is why early meet effects drift.
- `ratings.r`: the rating methods (2025 points, Elo, PR, adjusted time via the
  pipeline's `rank_athletes()`), the rank blend and the backtest.
- `plot_theme.r`: shared figure style and method colours.
