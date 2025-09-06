
# Create Initial List of Schools
if(!file.exists(paste0(path, "Simulation/list_schools.rds"))){
  get_state <- meet_list %>% filter(meet %in% c("sdhsaa_b", "sdhsaa_a", "sdhsaa_aa"))

  state_boys <- list()
  state_girls <- list()
  class2 <- c("B", "A", "AA")
  for(i in 1:nrow(get_state)){ #2x is for both boys and girls
      tboys = paste0(get_state$meet[i], "_boys")
      tgirls = paste0(get_state$meet[i], "_girls")
      state_boys[[i]] <- read.csv(file=paste0(path, "Data/", tboys, ".csv")) %>% 
                            mutate(school_class = class2[i])
      state_girls[[i]] <- read.csv(file=paste0(path, "Data/", tgirls, ".csv")) %>% 
                            mutate(school_class = class2[i])
  }

  state_boys_comb <- do.call(rbind, state_boys)
  state_girls_comb <- do.call(rbind, state_girls)

  list_schools_boys_pre <- state_boys_comb %>% group_by(School, school_class) %>% summarise(n=n()) %>% select(School, school_class)
  list_schools_girls_pre <- state_girls_comb %>% group_by(School, school_class) %>% summarise(n=n()) %>% select(School, school_class)

  #Getting a complete list of schools (from state)
  list_schools_pre <- list_schools_boys_pre %>%
    full_join(list_schools_girls_pre, by=c("School","school_class"))

  #Appending Additional Schools that didn't make state
  list_schools <- rbind(
    list_schools_pre,
    data.frame(
      School= c("Alcester-Hudson", "Elk Point-Jefferson", "McCook-Central","Parker",
                "Wagner","Iroquois/Lak","Tri-Valley", "CEB","Garretson","Kadoka", "Colman-Egan",
                "Elkton-Lake Benton", "Oldham-Ramona-Rutland"),
      school_class = c("B", "A", "A", "A",
                       "A", "B", "A", "A", "A", "B", "B",
                       "A", "B")
    )
    )

  saveRDS(list_schools, paste0(path, "Simulation/list_schools.rds"))
}