



library(tidyverse)
library(readxl)
library(janitor)
library(pdftools)
library(tesseract)

#Format Winner XC Meet
path <- "C:/Users/Daniel Burkhalter/OneDrive - South Dakota State University - SDSU/Documents/School/STAT 651/XC Ranking/Data/"
read_in <- readxl::read_xlsx(paste(path,"Winner Cross Country Results - 2023.xlsx",sep=""),
                                 sheet = "V Boys", col_names = FALSE) %>% clean_names()
winner_boys <- read_in %>%
  mutate(Name = paste(x6, x7,sep=" "),
         Time = format(x9, format = "%M:%S"),
         Class = "M") %>% 
  select(Place= x8, Name, Time, School=x4, Class)

write.csv(winner_boys, paste(path, "winner_boys.csv",sep="")) 


#Format Winner XC Meet
path <- "C:/Users/Daniel Burkhalter/OneDrive - South Dakota State University - SDSU/Documents/School/STAT 651/XC Ranking/Data/"
read_in <- readxl::read_xlsx(paste(path,"Winner Cross Country Results - 2023.xlsx",sep=""),
                             sheet = "V Girls", col_names = FALSE) %>% clean_names()
winner_girls <- read_in[-c(1,23),] %>%
  mutate(Name = paste(x6, x7,sep=" "),
         Time = format(x9, format = "%M:%S"),
         Class = "M") %>% 
  select(Place= x8, Name, Time, School=x4, Class)

write.csv(winner_girls, paste(path, "winner_girls.csv",sep=""))


#DOES NOT WORK: Format Pierre XC Meet (Flag as non-5k)
if(TRUE == FALSE){
read_in <- pdf_text(paste(path,"2023 Pierre Invite Results.pdf",sep=""))
read_in <- pdf_info(paste(path,"2023 Pierre Invite Results.pdf",sep=""))
read_in <- ocr(paste(path,"2023 Pierre Invite Results.pdf",sep=""))

txt_girls <- read_in[[5]]
txt_boys <- read_in[[6]]

split_boys1 <- str_split(txt_boys, pattern="\n")[[1]][7:47]
lapply(1:length(split_boys1), function(i) str_split(split_boys1[i]," "))
read_in %>% class()
cat(read_in)
}

