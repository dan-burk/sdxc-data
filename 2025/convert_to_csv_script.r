
# Import Libraries
library(dplyr)
library(readxl)
library(stringr)

source("2025/txt_to_csv_2025.r")
week_i = 3

# Import XC Meet Lists
meet_list <- readxl::read_xlsx(paste0(2025, "/meet_list_data_ready.xlsx")) %>% 
    mutate(date = as.Date(date)) %>% 
    filter(week <= week_i & missing == 0)

vector_boys <- paste0(meet_list$meet, "_boys")
vector_girls <- paste0(meet_list$meet, "_girls")

# i=10
for(i in 1:length(vector_boys)){
    lines_boys <- readLines(paste0("2025/Data/",vector_boys[i], ".txt"))
    lines_girls <- readLines(paste0("2025/Data/",vector_girls[i], ".txt"))
    get_csv_boys <- txt_to_csv_2025(lines_boys)
    get_csv_girls <- txt_to_csv_2025(lines_girls)
    print(get_csv_boys[1:5,])
    print(get_csv_girls[1:5,])
    write.csv(get_csv_boys, paste0("2025/Data/",vector_boys[i], ".csv"), row.names = FALSE)
    write.csv(get_csv_girls, paste0("2025/Data/",vector_girls[i], ".csv"), row.names = FALSE)
}
