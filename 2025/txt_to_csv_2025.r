
#Input: A TXT file containing results from a 2025 XC meet
#Output: A dataframe with columns: Place, Name, School, Time, & Grade
txt_to_csv_2025 <- function(lines) {
  results <- data.frame(Place = integer(),
                        Name = character(),
                        School = character(),
                        Time = character(),
                        Grade = character(),
                        stringsAsFactors = FALSE)
  
  i <- 1
  while (i <= length(lines)) {
    if (grepl("^\\d+$", lines[i])) {
      place <- as.integer(lines[i])
      name <- lines[i + 2]
      school <- lines[i + 3]
      time <- lines[i + 4]
      grade_line <- lines[i + 5]
      grade <- sub(".*Yr:\\s*(\\d+).*", "\\1", grade_line)
      results <- rbind(results, data.frame(Place = place,
                                           Name = name,
                                           School = school,
                                           Time = time,
                                           Grade = grade,
                                           stringsAsFactors = FALSE))
      i <- i + 6
    } else {
      i <- i + 1
    }
  }
  return(results)
}