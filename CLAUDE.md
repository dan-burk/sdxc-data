# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

This repository processes South Dakota high school cross country (XC) race results and generates rankings using an ELO-like scoring system. The system:
- Converts raw meet results (TXT/XLSX) to CSV format
- Tracks athletes across the season with unique IDs
- Calculates cumulative point scores based on pairwise race comparisons
- Maintains standardized school naming conventions
- Separates boys and girls divisions

## Project Structure

```
/{year}/
  Data/                    # Raw and processed meet results
    *_boys.csv/.txt       # Boys race results
    *_girls.csv/.txt      # Girls race results
  Simulation/
    df_points_{boys,girls}.{csv,rds}  # Cumulative scores
    df_name_{boys,girls}.rds          # Athlete ID mappings
    list_schools.csv                  # Standardized school names
  meet_list_data_ready.xlsx           # Meet schedule metadata
  txt_to_csv_2025.r                   # 2025 TXT parser
  convert_to_csv_script.r             # Batch conversion

Root scripts:
  scoring_script.r        # Main scoring pipeline
  functions.r             # Core scoring functions
  accesory.r              # School list initialization
```

## Data Flow

1. **Data Ingestion**: Meet results arrive as TXT or XLSX files in `{year}/Data/`
2. **Conversion**: `convert_to_csv_script.r` converts TXT → CSV using year-specific parsers
3. **School Standardization**: All school names validated against `list_schools.csv`
4. **Scoring**: `scoring_script.r` processes meets sequentially by week
5. **Output**: Updates RDS/CSV files in `Simulation/` with cumulative rankings

## Key Data Structures

### Meet Results CSV
Columns: `Place`, `Name`, `School`, `Time`, `Grade`

### Points Dataframe
Columns: `Name`, `School`, `id`, `points`, `time_min`
- `id`: Unique athlete identifier (persists across season)
- `points`: ELO-like score (starts at 1000)
- `time_min`: Personal record in seconds

### School List
Columns: `School`, `school_class`
- `school_class`: Classification (AA, A, or B)
- Schools must exist in this list before processing meets

## Scoring Algorithm

The `do_scoring()` function in `functions.r`:
1. Takes all athletes in a race and generates pairwise comparisons
2. For each comparison where athlete i beats athlete j:
   - Winner gains: `points_wager * loser_points` (default: 5%)
   - Loser loses: `points_wager * loser_points`
3. Points transfer occurs for all n(n-1)/2 comparisons
4. Updates cumulative points and personal records

## Common Workflows

### Processing New Meet Results

```r
# Set year and week in convert_to_csv_script.r
week_i <- 6
source("2025/convert_to_csv_script.r")  # Convert TXT to CSV

# Set week_i in scoring_script.r
week_i <- 1  # Process specific week
source("scoring_script.r")
```

### Adding Missing Schools

When the script encounters unknown schools (browser() breakpoint at line 202 of `scoring_script.r`):
1. Check if school name is misspelled → no action needed (filtered out)
2. Check if school is from out-of-state → no action needed (filtered out)
3. If legitimate in-state school:
   ```r
   list_schools <- rbind(list_schools,
     data.frame(School = "New School", school_class = "A"))
   write.csv(list_schools, "2025/Simulation/list_schools.csv")
   ```
4. Type `c` in debugger to continue

### TXT File Format Expectations

**2025 Format** (parsed by `txt_to_csv_2025.r`):
```
1
[optional initials]
Athlete Name
School Name
MM:SS.ss
Yr: 12
```

**2023 Format** (parsed by `txt_to_csv` in `read_dakotatiming_v2.r`):
Tab-delimited with name, school, and time on separate lines

## Important Variables

- `points_wager`: Point transfer rate per comparison (default: 0.05 = 5%)
- `year`: Processing year (2023, 2025)
- `week_i`: Current week being processed
- `flg_5k`: Flag in meet_list indicating if meet is 5K (vs 4K or other)
- `run_augie`: Boolean to include/exclude Augie meet (large, slow to process)

## File Naming Conventions

Meet files follow pattern: `{meet_name}_{boys|girls}.{csv|txt}`
- Example: `beresford_boys.csv`, `region3a_girls.txt`
- Meet names must match entries in `meet_list_data_ready.xlsx`

## State Management

The system maintains state across meets using RDS files:
- **First meet of season** (k=1): Initializes all athletes at 1000 points
- **Subsequent meets**: Loads previous points, assigns new athletes 1000 points
- **Athlete tracking**: Uses name matching to identify returning athletes
- **School tracking**: Only processes in-state schools from `list_schools.csv`

## Debugging Notes

- `browser()` breakpoint at line 202 catches missing schools
- Lines 179-197, 377-397, 459-480: Commented debug/visualization code
- The script processes meets sequentially - cannot skip weeks
- Out-of-state athletes are filtered before place reassignment
- Name matching is case-sensitive (all names converted to uppercase)

## Known Issues

See `ISSUES.md` for a list of known issues including:
- Duplicate and misspelled files
- Orphaned files without CSV equivalents
- Files in wrong locations
- Source files (PNG/PDF) that could be archived
- Code with hardcoded paths
