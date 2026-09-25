# Convert raw meet results (TXT, or a MileSplit raw-results URL) into
# Data CSVs with columns Place, Name, School, Time, Grade.

txt_to_csv <- function(lines) {
  L <- trimws(lines)
  # keep empties to detect block structure, but we’ll skip them as needed

  results <- data.frame(
    Place = integer(), Name = character(), School = character(),
    Time = character(), Grade = character(), stringsAsFactors = FALSE
  )

  is_blank <- function(x) identical(x, "") || nchar(x) == 0
  looks_place <- function(x) grepl("^\\d+$", x)
  looks_initials <- function(x) grepl("^[A-Z]{1,3}\\(?$", x)  # "AB", or "A(" when a nickname follows
  looks_time <- function(x) grepl("^\\d{1,2}:\\d{2}(?:\\.\\d{1,2})?$", x)

  i <- 1; n <- length(L)
  while (i <= n) {
    if (!looks_place(L[i])) { i <- i + 1; next }
    place <- as.integer(L[i]); i <- i + 1

    # optional blank after place
    while (i <= n && is_blank(L[i])) i <- i + 1

    # optional initials line
    if (i <= n && looks_initials(L[i])) i <- i + 1

    # skip any extra blanks
    while (i <= n && is_blank(L[i])) i <- i + 1

    if (i > n) break
    name <- trimws(gsub("\\s*\\([^)]*\\)", "", L[i])); i <- i + 1  # drop "(Nickname)"

    # possible blank between name and school
    while (i <= n && is_blank(L[i])) i <- i + 1

    if (i > n) break
    school <- L[i]; i <- i + 1

    # possible blank between school and time
    while (i <= n && is_blank(L[i])) i <- i + 1

    if (i > n) break
    time <- L[i]
    if (grepl("Yr:", time)) time <- ""  # no time listed; leave the grade line for below

    # if what we thought was time doesn’t look like a time,
    # assume school/time shifted by one
    if (!looks_time(time) && i + 1 <= n && looks_time(L[i + 1])) {
      school <- paste(school, L[i], sep = " ")
      i <- i + 1
      time <- L[i]
    }
    if (time != "") i <- i + 1

    # grade line ("Yr: 12 ...") is sometimes missing; don't eat the next runner's place
    grade <- NA_character_
    if (i <= n && grepl("Yr:", L[i])) {
      grade <- sub(".*Yr:\\s*(\\d+).*", "\\1", L[i])
      i <- i + 1
    }

    results <- rbind(
      results,
      data.frame(Place = place, Name = name, School = school,
                 Time = time, Grade = grade, stringsAsFactors = FALSE)
    )
  }
  results
}

fetch_milesplit_raw <- function(url) {
  lines <- readLines(url, warn = FALSE)

  # The raw page is a fixed-width table like:
  #   1 Gage Beverly              10 Vermillion                                16:47.30
  # Pattern: place, name, grade (1-2 digits), school, time (MM:SS.ss)
  pattern <- "^\\s*(\\d+)\\s+(.+?)\\s+(\\d{1,2})\\s+(.+?)\\s+(\\d{1,2}:\\d{2}\\.\\d{2})\\s*$"

  matched <- grepl(pattern, lines)

  if (sum(matched) == 0) {
    warning("No lines matched the expected MileSplit format. Check the URL.")
    return(data.frame(Place = integer(), Name = character(), School = character(),
                      Time = character(), Grade = character(), stringsAsFactors = FALSE))
  }

  results <- do.call(rbind, lapply(lines[matched], function(line) {
    m <- regmatches(line, regexec(pattern, line))[[1]]
    data.frame(Place = as.integer(m[2]),
               Name  = trimws(m[3]),
               School = trimws(m[5]),
               Time  = m[6],
               Grade = m[4],
               stringsAsFactors = FALSE)
  }))

  results
}
