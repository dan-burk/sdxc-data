
#Input: A TXT file containing results from a 2025 XC meet
#Output: A dataframe with columns: Place, Name, School, Time, & Grade
txt_to_csv_2025 <- function(lines) {
  L <- trimws(lines)
  # keep empties to detect block structure, but we’ll skip them as needed

  results <- data.frame(
    Place = integer(), Name = character(), School = character(),
    Time = character(), Grade = character(), stringsAsFactors = FALSE
  )

  is_blank <- function(x) identical(x, "") || nchar(x) == 0
  looks_place <- function(x) grepl("^\\d+$", x)
  looks_initials <- function(x) grepl("^[A-Z]{1,3}$", x)
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
    name <- L[i]; i <- i + 1

    # possible blank between name and school
    while (i <= n && is_blank(L[i])) i <- i + 1

    if (i > n) break
    school <- L[i]; i <- i + 1

    # possible blank between school and time
    while (i <= n && is_blank(L[i])) i <- i + 1

    if (i > n) break
    time <- L[i]

    # if what we thought was time doesn’t look like a time,
    # assume school/time shifted by one
    if (!looks_time(time) && i + 1 <= n && looks_time(L[i + 1])) {
      school <- paste(school, L[i], sep = " ")
      i <- i + 1
      time <- L[i]
    }
    i <- i + 1

    if (i > n) break
    grade_line <- L[i]; i <- i + 1
    grade <- sub(".*Yr:\\s*(\\d+).*", "\\1", grade_line)
    if (identical(grade, grade_line)) grade <- NA_character_

    results <- rbind(
      results,
      data.frame(Place = place, Name = name, School = school,
                 Time = time, Grade = grade, stringsAsFactors = FALSE)
    )
  }
  results
}
