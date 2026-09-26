# Write a season's rankings as JSON for the website (the sister repo ../sdxc).
# One file per gender and week: ../sdxc/data/{gender}_{year}_week{N}.json,
# plus the season's schools (../sdxc/data/schools_{year}.json) and the year's latest week in manifest.json
# Run after run.r:  Rscript export.r 2026

year <- 2025  # or: Rscript export.r 2026
if (length(commandArgs(trailingOnly = TRUE))) year <- as.numeric(commandArgs(trailingOnly = TRUE)[1])
out_dir <- "../sdxc/data"

if (!dir.exists(out_dir)) stop("Can't find ", out_dir, ": the sdxc repo should sit next to this one.")

# "16:31.78" -> 991.78 seconds (the site calls this time_min but it's seconds)
to_seconds <- function(time) {
  parts <- strsplit(time, ":")
  sapply(parts, function(p) if (length(p) == 2) as.numeric(p[1]) * 60 + as.numeric(p[2]) else NA)
}

for (gender in c("boys", "girls")) {
  rankings <- read.csv(file.path(year, "output", paste0("rankings_", gender, ".csv")))

  # id: one number per athlete for the whole season, so it's the same every week
  athlete <- paste(rankings$name, rankings$school)
  rankings$id <- match(athlete, sort(unique(athlete)))

  for (wk in sort(unique(rankings$week))) {
    r <- rankings[rankings$week == wk, ]
    r <- r[order(r$rank), ]
    site <- data.frame(Name         = r$name,
                       School       = r$school,
                       id           = r$id,
                       points       = round(r$points, 4),
                       time_min     = round(to_seconds(r$pr), 2),
                       rnk_blnd     = r$rank,
                       school_class = r$class)
    file <- file.path(out_dir, paste0(gender, "_", year, "_week", wk, ".json"))
    # runners with no PR get time_min: null
    jsonlite::write_json(site, file, pretty = TRUE, digits = NA, na = "null")
    cat("Wrote", file, "(", nrow(site), "runners )\n")
  }
}

# The season's in-state schools, classes and regions, for the Teams tab
schools <- read.csv(file.path(year, "schools.csv"))
site <- data.frame(School = schools$school, school_class = schools$class,
                   region = ifelse(schools$region == "", NA, schools$region))  # AA has no regions
site <- site[order(site$School), ]
file <- file.path(out_dir, paste0("schools_", year, ".json"))
jsonlite::write_json(site, file, pretty = TRUE, na = "null")
cat("Wrote", file, "(", nrow(site), "schools )\n")

# manifest.json tells the site which years exist and each year's latest week
file <- file.path(out_dir, "manifest.json")
manifest <- jsonlite::read_json(file)
manifest$years[[as.character(year)]] <- max(rankings$week)
manifest$years <- manifest$years[order(names(manifest$years))]
manifest$current <- max(as.numeric(names(manifest$years)))
jsonlite::write_json(manifest, file, pretty = TRUE, auto_unbox = TRUE)
cat("Wrote", file, "(", year, "has", max(rankings$week), "weeks )\n")
