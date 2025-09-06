
library(dplyr)
df_points_boys <- read.csv("df_points_boys.csv")
df_points_girls <- read.csv("df_points_girls.csv")

b <- df_points_boys %>%  select(Name, School)
g <- df_points_girls %>%  select(Name, School)
b %>% head()
g %>% head()

write.csv(b, file = "boys_names.csv")
write.csv(g, file = "girls_names.csv")

#Send .csv files to Claude &/or OpenAI