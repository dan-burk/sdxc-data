
# Import Libraries
library(dplyr)
library(readxl)
library(stringr)

source("2025/txt_to_csv_2025.r")
source("2025/fetch_milesplit_raw.r")
week_i = 8

# Import XC Meet Lists
meet_list <- readxl::read_xlsx(paste0(2025, "/meet_list_data_ready.xlsx")) %>% 
    mutate(date = as.Date(date)) %>% 
    filter(week <= week_i)

vector_boys <- paste0(meet_list$meet, "_boys")
vector_girls <- paste0(meet_list$meet, "_girls")

# i=10
for(i in 1:length(vector_boys)){
    if(meet_list$missing[i] == 0 && meet_list$alternative[i] == 0){
        lines_boys <- readLines(paste0("2025/Data/",vector_boys[i], ".txt"))
        lines_girls <- readLines(paste0("2025/Data/",vector_girls[i], ".txt"))
        get_csv_boys <- txt_to_csv_2025(lines_boys)
        get_csv_girls <- txt_to_csv_2025(lines_girls)
        print(get_csv_boys[1:2,])
        print(get_csv_girls[1:2,])
        write.csv(get_csv_boys, paste0("2025/Data/",vector_boys[i], ".csv"), row.names = FALSE)
        write.csv(get_csv_girls, paste0("2025/Data/",vector_girls[i], ".csv"), row.names = FALSE)
    }else if (meet_list$missing[i] == 0 && meet_list$alternative[i] == 1){
        get_csv_boys <- fetch_milesplit_raw(meet_list$alternative_boys[i])
        get_csv_girls <- fetch_milesplit_raw(meet_list$alternative_girls[i])
        write.csv(get_csv_boys, paste0("2025/Data/", vector_boys[i], ".csv"), row.names = FALSE)
        write.csv(get_csv_girls, paste0("2025/Data/", vector_girls[i], ".csv"), row.names = FALSE)    
    }
}
