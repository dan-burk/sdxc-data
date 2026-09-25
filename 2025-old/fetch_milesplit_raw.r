
#Input: A MileSplit raw results URL
#Output: A dataframe with columns: Place, Name, School, Time, & Grade
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
