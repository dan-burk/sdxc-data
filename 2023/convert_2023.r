# 2023 only: converts the hand-typed results in 2023/Data/manual/ into Data CSVs.
#
# In 2023 about half the meets were typed by hand from PDFs, photos and
# spreadsheets, so there's no TXT for them. Those hand-typed files are the
# source (fix problems there); this writes 2023/Data/{meet}_{g}.csv in the
# standard layout (Place, Name, School, Time, Grade). The TXT meets still go
# through convert.r. Run from the repo root:
#
#   Rscript convert.r 2023          TXT meets
#   Rscript 2023/convert_2023.r     hand-typed meets
#   Rscript run.r 2023              score
#
# Quirks of the hand-typed files handled here:
# - Columns are: row number, Place, Name, Time, School, Class (gender), no grade.
# - Place isn't reliable (Plankinton girls has grades in it), so place = row order.
# - Times typed "24:06:00" mean 24:06 (Winner girls).
# - "DNF" in Place with no time (Winner boys).
# - Blank rows (end of Faith boys).

library(stringr)

for (f in list.files("2023/Data/manual", pattern = "_(boys|girls)\\.csv$")) {
  d <- read.csv(file.path("2023/Data/manual", f), colClasses = "character", check.names = FALSE)
  d <- d[str_squish(d$Name) != "", ]

  time <- str_squish(d$Time)
  time[is.na(time)] <- ""
  time <- sub("^(\\d{1,2}:\\d{2}):00$", "\\1", time)
  time[d$Place == "DNF"] <- "DNF"

  out <- data.frame(Place = seq_len(nrow(d)), Name = str_squish(d$Name), School = str_squish(d$School),
                    Time = time, Grade = NA)
  write.csv(out, file.path("2023/Data", f), row.names = FALSE, na = "")
}
message("Converted ", length(list.files("2023/Data/manual")), " hand-typed files.")
