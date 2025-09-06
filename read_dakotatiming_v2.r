
library(dplyr)
library(readxl)


run_augie <- TRUE
points_wager <- 0.05 #Was 0.005
setwd("C:/Users/Daniel Burkhalter/OneDrive - South Dakota State University - SDSU/Documents/School/STAT 651/XC Ranking/Data")

meet_list <- readxl::read_xlsx("../meet_list_data_ready.xlsx") %>% 
    mutate(date = as.Date(date))

get_dt <- meet_list %>% filter(source == "DT" & missing == 0)

vector_boys_dt <- paste0(get_dt$meet, "_boys")
vector_girls_dt <- paste0(get_dt$meet, "_girls")

txt_to_csv <- function(file){
  
  get_test <- readLines(paste(file,".txt",sep=""))
  df <- data.frame(get_test)
  head_comb <- paste(df$get_test[1:3], collapse= " ")
  head_split <- strsplit(head_comb,split="\t")
  head <- trimws(head_split[[1]][2:4], "left")
  
  name <- df[seq(5,nrow(df)-2,by=4),]
  school <- df[seq(6,nrow(df)-1,by=4),]
  time <-  df[seq(7,nrow(df),by=4),]
  
  
  temp1 <- strsplit(school,split=" \\[")
  school_list <- lapply(1:length(school), function(i) temp1[[i]][1])
  school2 <- unlist(school_list)
  
  temp2 <- strsplit(time,split="\t")
  time_list <- lapply(1:length(time), function(i) temp2[[i]][1])
  time2 <- unlist(time_list)
  
  if(grepl("boys",file)){
    
    output <- data.frame(
      Place = c(1:length(name)),
      Name = name,
      Time = time2,
      School = school2,
      Class = rep("M", times=length(name))
    )
    
  }else if(grepl("girls",file)){
    output <- data.frame(
      Place = c(1:length(name)),
      Name = name,
      Time = time2,
      School = school2,
      Class = rep("F", times=length(name))
    )
    
  }else{
    print(paste("WARNING! File", file, "is corrupted."))
  }
  

  write.csv(output, file=paste("C:/Users/Daniel Burkhalter/OneDrive - South Dakota State University - SDSU/Documents/School/STAT 651/XC Ranking/Data/",file,".csv",sep=""))
  
  return(nrow(output))
}




lapply(1:length(vector_boys_dt), function(i) txt_to_csv(vector_boys_dt[i]))
lapply(1:length(vector_girls_dt), function(i) txt_to_csv(vector_girls_dt[i]))

#For corrupted files
vector_boys_dt <- "heartland_preview_boys"
vector_girls_dt <- "heartland_preview_girls"
lapply(1:length(vector_boys_dt), function(i) txt_to_csv(vector_boys_dt[i]))
lapply(1:length(vector_girls_dt), function(i) txt_to_csv(vector_girls_dt[i]))
