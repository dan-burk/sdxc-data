
# Import Libraries
library(dplyr)
library(readxl)
library(stringr)

run_augie <- TRUE #Set to TRUE to run Augie meet (Takes a while to run)
points_wager <- 0.05 #Was 0.005
year <- 2025
source("functions.r")

# Import XC Meet Lists
meet_list <- readxl::read_xlsx(paste0(year, "/meet_list_data_ready.xlsx")) %>% 
    mutate(date = as.Date(date)) %>% 
    filter(missing == 0)

# Import Standardized School List
list_schools <- read.csv(paste0(year, "/Simulation/list_schools.csv"), stringsAsFactors = FALSE)

    # Read in Cumulative Scoring Throughout Season
  t <- readRDS(paste0(year, "/Simulation/df_points_boys.rds"))
  class(t)
  names(t)
  length(t)
  rm(t)
  # week_i = 7 #Important Parameter
for(week_i in 1:7){ #Maybe turn this into a function?
  
    if(week_i == 1){
      meet_list_i <- meet_list %>% filter(week <= week_i)
      n_i <- nrow(meet_list_i) #Number of meets in week_i
      meet_list_i
      n_0 <- 1
    }else{
      meet_list_im1 <- meet_list %>% filter(week <= week_i-1)
      meet_list_i <- meet_list %>% filter(week <= week_i)
      n_i <- nrow(meet_list_i) #There are DEPENDENCIES on n_i in the code!!
      n_0 <- nrow(meet_list_im1) + 1
      meet_list_i[c(n_0:n_i),]
    }
    ### Loop through remaining meets
    # Finished through k=11
    for(k in n_0:n_i){

      if(k == 1){
        #### If First Meet (i.e. Beresford), k=1
        df_name_boys <- list()
        df_points_boys <- list()
        df_name_girls <- list()
        df_points_girls <- list()

        tboys = paste0(meet_list$meet[k], "_boys")
        tgirls = paste0(meet_list$meet[k], "_girls")
        readin_boys <- read.csv(file=paste0(year,"/Data/",tboys,".csv")) %>% 
          mutate(Name = toupper(Name))
        readin_girls <- read.csv(file=paste0(year,"/Data/",tgirls,".csv")) %>% 
          mutate(Name = toupper(Name))

        #Find Missing Schools (and print)
        readin_boys %>% anti_join(list_schools, by="School") %>% group_by(School) %>% summarise(n=n())

        #Convert School Names to Standardized Naming
        result_boys_pre <- readin_boys %>%
            mutate(School = case_when(
                                School == "DR Smary" ~ "DR St. Mary",
                                School == "Freeman Acad" ~ "Freeman Acad/Mar",
                                School == "McCook Centr" ~ "McCook-Central",
                                School == "SF Lincoln" ~ "Sioux Falls Lincoln",
                                School %in% c("Mount Vernon/Plankinton","Mount Vernon") ~ "Mount Vernon/Pla",
                                School == "Potter county" ~ "Potter County",
                                School == "SBA" ~ "Sunshine Bible A",
                                School %in% c("Sanborn Central/Woonsocket","Sanborn Cent") ~ "Sanborn Cent/Woo",
                                TRUE ~ School)) %>% 
          select(-X) %>% 
          mutate(points_pre = 1000,
                points_post = 1000) %>%
          filter(School %in% list_schools$School) #Filter out of state people

        result_boys_pre$Place <- c(1:nrow(result_boys_pre)) #Reording places after removing out of state people
        result_boys_pre$id <- c(1:nrow(result_boys_pre)) #Giving each person an ID

        df_name_boys[[k]] <- result_boys_pre %>% select(id, Name) #school_id
        saveRDS(df_name_boys, paste0(year, "/Simulation/df_name_boys.rds"))

        #Create Function to do Scoring
        x <- nrow(result_boys_pre) #Number of individuals
        n <- 0.5*(x-1)*x #Total number of comparisons.
        #Make function that creates all pairwise comparisons
        comparisons <- matrix(0, nrow = x * (x - 1) / 2, ncol = 2)
        counter <- 1
        for (i in 1:(x - 1)) {
          for (j in (i + 1):x) {
            comparisons[counter, ] <- c(i, j)
            counter <- counter + 1
          }
        }

        result_final <- result_boys_pre
        for(i in 1:n){
          t <- comparisons[i,] #Take the ith comparison
          sub <- result_final[t,] #and subset the results to that comparison
          result_final <- result_final %>% 
            mutate(points_post = case_when(
              Place == t[1] ~ points_post + points_wager*sub$points_post[2], #Add x%  of the points from the looser to winner
              Place == t[2] ~ points_post - points_wager*sub$points_post[2], #Take x%  of the points from the looser and subtract
              TRUE ~ points_post
            ))
          
        }

        split_time <- str_split(result_final$Time, ":")
        get_time_sec <- unlist(lapply(1:nrow(result_final), function(i) as.numeric(split_time[[i]][1])*60 + 
                                        as.numeric(split_time[[i]][2])))


        df_points_boys[[k]] <- result_final %>%
          select(Name, School, id, points = points_post) %>% 
          mutate(time_min = get_time_sec)
        names(df_points_boys)[k] <- meet_list$meet[k]
        saveRDS(df_points_boys, paste0(path, "Simulation/df_points_boys",".rds"))
        write.csv(df_points_boys[[k]], paste0(path, "Simulation/df_points_boys.csv"))

        ### Done with beresford boys

        ### Beresford Girls

        #Find Missing Schools
        readin_girls %>% anti_join(list_schools, by="School") %>% group_by(School) %>% summarise(n=n())

        #Convert School Names to Standardized Naming
        result_girls_pre <- readin_girls %>%
        mutate(School = case_when(School == "DR Smary" ~ "DR St. Mary",
                                  School == "Freeman Acad" ~ "Freeman Acad/Mar",
                                  School == "McCook Centr" ~ "McCook-Central",
                                  School == "SF Lincoln" ~ "Sioux Falls Lincoln",
                                  School %in% c("Mount Vernon/Plankinton","Mount Vernon") ~ "Mount Vernon/Pla",
                                  School == "Potter county" ~ "Potter County",
                                  School == "SBA" ~ "Sunshine Bible A",
                                  School %in% c("Sanborn Central/Woonsocket","Sanborn Cent") ~ "Sanborn Cent/Woo",
                                  TRUE ~ School)) %>% 
          select(-X) %>% 
          mutate(points_pre = 1000,
                points_post = 1000) %>%
          filter(School %in% list_schools$School)#%>% 
          # inner_join(list_schools, by="School") %>%  #Remove out of state schools
          # select(-school_class)
        result_girls_pre$Place <- c(1:nrow(result_girls_pre)) #Giving each person an ID
        result_girls_pre$id <- c(1:nrow(result_girls_pre)) #Giving each person an ID

        df_name_girls[[k]] <- result_girls_pre %>% select(id, Name) #school_id
        saveRDS(df_name_girls, paste0(path, "Simulation/df_name_girls.rds"))

        # result_girls_pre %>% head()

        x <- nrow(result_girls_pre) #Number of individuals
        n <- 0.5*(x-1)*x #Total number of comparisons.
        comparisons <- matrix(0, nrow = x * (x - 1) / 2, ncol = 2)
        counter <- 1

        for (i in 1:(x - 1)) {
          for (j in (i + 1):x) {
            comparisons[counter, ] <- c(i, j)
            counter <- counter + 1
          }
        }

        result_final <- result_girls_pre
        for(i in 1:n){
          t <- comparisons[i,] #Take the ith comparison
          sub <- result_final[t,] #and subset the results to that comparison
          result_final <- result_final %>% 
            mutate(points_post = case_when(
              Place == t[1] ~ points_post + points_wager*sub$points_post[2], #Add x%  of the points from the looser to winner
              Place == t[2] ~ points_post - points_wager*sub$points_post[2], #Take x%  of the points from the looser and subtract
              TRUE ~ points_post
            ))
          
        }

        split_time <- str_split(result_final$Time, ":")
        get_time_sec <- unlist(lapply(1:nrow(result_final), function(i) as.numeric(split_time[[i]][1])*60 + 
                                        as.numeric(split_time[[i]][2])))


        df_points_girls[[k]] <- result_final %>%
          select(Name, School, id, points = points_post) %>% 
          mutate(time_min = get_time_sec)

        names(df_points_girls)[k] <- meet_list$meet[k]
        saveRDS(df_points_girls, paste0(path, "Simulation/df_points_girls",".rds"))
        write.csv(df_points_girls[[k]], paste0(path, "Simulation/df_points_girls.csv"))

        #Done with Beresford Girls
      }else{
        df_name_boys <- readRDS(paste0(path, "Simulation/df_name_boys.rds")) #%>% select(-X)
        df_schools <- read.csv(paste0(path, "Simulation/df_schools.csv")) %>% select(-X)
        df_points_boys <- readRDS(paste0(path, "Simulation/df_points_boys.rds")) #%>% select(-X)

        df_name_girls <- readRDS(paste0(path, "Simulation/df_name_girls.rds"))# %>% select(-X)
        df_points_girls <- readRDS(paste0(path, "Simulation/df_points_girls.rds"))# %>% select(-X)


          # k = 2
          tboys = paste0(meet_list$meet[k], "_boys")
          tgirls = paste0(meet_list$meet[k], "_girls")
          readin_boys <- read.csv(file=paste0(path,"Data/",tboys,".csv")) %>% 
            mutate(Name = toupper(Name))
          readin_girls <- read.csv(file=paste0(path,"Data/",tgirls,".csv")) %>% 
            mutate(Name = toupper(Name))

          flg_5k = meet_list$flg_5k[k]

          #Debugging code
          if(TRUE == FALSE){
            names(df_points_boys)
            df_points_boys[[k]] %>% View()
            df_points_girls[[k]] %>% View()
            df_points_boys[[9]] %>% filter(Name == "Pierce Baumberger")
            df_points_boys[[7]] %>% View()

            readin_boys %>% View()
          }

          if(TRUE == FALSE){
            #Appending Additional Schools that weren't in list
            list_schools %>% arrange(School) %>% View()
            list_schools_new <- rbind(list_schools, data.frame(School= c("Britton-Hecla"), school_class = c("B")))
            saveRDS(list_schools_new, paste0(path, "Simulation/list_schools.rds"))
            saveRDS(list_schools_new, paste0(path, "XC Ranking App/sdxc-basic/list_schools.rds"))
            list_schools <- readRDS(paste0(path, "Simulation/list_schools.rds"))
            list_schools %>% View()
          }

          nam <- readin_boys %>% anti_join(list_schools, by="School") %>% group_by(School) %>% summarise(n=n())
          print(paste("Browser for k =", k, meet_list$meet[k]))
          # browser()
          #Here we check the variable nam:
          #If empty continue.
          #If School is missing from  list_schools, update list_schools with new name
          #If School is misspelled add to mutates below, type break in command line, then restart loop at given k.
          readin_boys <- readin_boys %>%
            mutate(Name = case_when(
              Name == "AARON BLACHFORD" ~ "AARON BLACKFORD",
              Name == "ABE (ABRAM) CHANCE" ~ "ABRAM CHANCE",
              Name == "BECKHAM CANTALOUPE" ~ "BECKHAM CANTALOPE",
              Name == "BRAYDEN BRODEWYK" ~ "BRAYDEN BORDEWYK",
              Name == "BRYNNLY HUBER" ~ "BRYNLY HUBER",
              Name == "CHASE HENRIKSEN" ~ "CHASE HANRIKSEN",
              Name == "CHEVY KONST" ~ "CHEVY KNOST",
              Name == "COLE SCHELLPFEFFER" ~ "COLE SCHELLPEFFER",
              Name == "COLE NOONAN" ~ "COLE NOON",
              Name == "COOPER SCHAELFER" ~ "COOPER SCHAEFER",
              Name == "DAVID MCKINLEY" ~ "DAVID MCKINLEY",
              Name == "GAGE HON" ~ "GAGE HOHN",
              Name == "ISAAC VAN SCHARREL" ~ "ISAAC VAN SCHALKWYK",
              Name == "JEFFERY BOXCHEE" ~ "JEFFERY BOSCHEE",
              Name == "JOSHEPH LAPRATH" ~ "JOSEPH LAPRATH",
              Name == "JOSIA WIEBE" ~ "JOSIAH WIEBE",
              Name == "KEGAN RUSSELL" ~ "KEEGAN RUSSELL",
              Name == "LOGAN RUFER" ~ "LOGAN RUTER",
              Name == "MATTHEW NEELY" ~ "MATTHEW NEELEY",
              Name == "MATT PETERSON" ~ "MATTHEW PETERSON",
              Name == "NATHAN MEILUS" ~ "NATHAN MELIUS",
              Name == "NICHOL DESCHEPPER" ~ "NICHOLAS DESCHEPPER",
              Name == "SPENCEER OLSEN" ~ "SPENCER OLSEN",
              Name == "TATE DEVRIES" ~ "TATE DEVRIES",
              Name == "TAY LARSON" ~ "TREY LARSON",
              Name == "TREVIN LOUNDSBURY" ~ "TREVIN LOUNSBURY",
              Name == "RUKKER BOE" ~ "TUKKER BOE",
              Name == "TYLER TJERDSMA" ~ "TYLER TJEERDSMA",
              Name %in% c("WAMNI OMNI LITTLE THUNDE", "WAMANI OMNI LITTLE THUNDER") ~ "WAMNI OMNI LITTLE THUNDER",
              Name == "WILLIAM FIER CLOUD" ~ "William FIRE CLOUD",
              Name == "JORDAN ERTMAN" ~ "JORDAN ERFMAN",
              TRUE ~ Name
            )) %>% 
            mutate(School = case_when(
              School %in% c("Andes Central/Dakota Christian") ~ "Andes Cen/Dak Ch",
              School %in% c("Clark/Willow") ~ "Clark/Willow Lake",
              School %in% c("Cheyenne-Eagle Butte","Cheyenne-Eag") ~ "CEB",
              School %in% c("DR Smary","DR St. Marys","Dell Rapids St. Mary") ~ "DR St. Mary",
              School == "Dell Rapis" ~ "Dell Rapids",
              School == "Deubrook" ~ "Deubrook Area",
              School %in% c("Elkton-Lake") ~ "Elkton-Lake Benton",
              School %in% c("Faulkton") ~ "Faulkton Area",
              School %in% c("Freeman ") ~ "Freeman",
              School %in% c("Freeman Acad","Freeman Academy/Marion") ~ "Freeman Acad/Mar",
              School %in% c("Frederick") ~ "Frederick Area",
              School %in% c("Great Plains Lutheran","Great Plains") ~ "Great Plains Lut",
              School %in% c("Groton") ~ "Groton Area",
              School %in% c("Herreid/Selby") ~ "Herreid/Selby Area",
              School %in% c("Highmore") ~ "Highmore-Harrold",
              School %in% c("Iroquois/Lake Preston") ~ "Iroquois/Lak",
              School %in% c("James","JVC","James Valley","James Valley Christian") ~ "James Valley Chr",
              School %in% c("Kadoka Area") ~ "Kadoka",
              School %in% c("Kimball White Lake") ~ "Kimball/White Lake",
              School %in% c("Little WOund") ~ "Little Wound",
              School %in% c("McCook Centr","McCook","McCook Central/Montrose") ~ "McCook-Central",
              School %in% c("Mount Vernon/Plankinton","Mount Vernon") ~ "Mount Vernon/Pla",
              School %in% c("Oldham-Ramon") ~ "Oldham-Ramona-Rutland",
              School == "Potter county" ~ "Potter County",
              School %in% c("Pierre") ~ "Pierre T.F. Riggs",
              School %in% c("Rapid City C","Rapid City Christian") ~ "RC Christian",
              School == "SF Lincoln" ~ "Sioux Falls Lincoln",
              School %in% c("Sioux Falls Christian") ~ "SF Christian",
              School %in% c("Sioux Falls Jefferson") ~ "SF Jefferson",
              School %in% c("Sioux Falls Roosevelt") ~ "SF Roosevelt",
              School %in% c("Sioux Falls Washington") ~ "SF Washington",
              School %in% c("SBA","Sunshine","Sunshine Bible Academy") ~ "Sunshine Bible A",
              School %in% c("Sanborn Central/Woonsocket","Sanborn Cent","Sanborn") ~ "Sanborn Cent/Woo",
              School %in% c("Suly Buttes") ~ "Sully Buttes",
              School %in% c(" Stanley County","Stanely County") ~ "Stanley County",
              School %in% c("St. Francis INdian") ~ "St. Francis Indian",
              School == "Webster" ~ "Webster Area",
              TRUE ~ School)) %>%
              filter(School %in% list_schools$School)
              # inner_join(list_schools, by="School") %>%  #Remove out of state schools
              # select(-school_class)
          readin_boys$Place <- c(1:nrow(readin_boys))

          readin_girls <- readin_girls %>%
            mutate(Name = case_when(
              Name == "ABBY JOHANNESSON" ~ "ABBY JOHANNESON",
              Name == "AKANE METCALF" ~ "AKANE METCALFE",
              Name == "ALEXIS (LUCY) DALEY" ~ "ALEXIS DALEY",
              Name == "ALYSSA NOVSTUP" ~ "ALYSSA NOVSTRUP",
              Name == "ASIA VANDERWERFF" ~ "ASIA VANDERWERFF",
              Name == "AUSTASIA SORENSEN" ~ "AUSTINA SORENSEN",
              Name %in% c("BRYNLYN HUBER", "BRYNNY HUBER") ~ "BRYNNLY HUBER",
              Name == "BRIELLA WATTLAUFER" ~ "BRIELLA WETTLAUFER",
              Name %in% c("CADENCE AMIOTTE", "Candance AMIOTTE") ~ "CADANCE AMIOTTE",
              Name == "CLARE PETERSON" ~ "CLAIRE PETERSON",
              Name == "CLAIRE MCCALLUM" ~ "CLAIRE MCCALLUM",
              Name == "ELI RIEFFENBERGER" ~ "ELIZABETH RIEFFENBERGER",
              Name == "ELLA BOEKLEHEIDE" ~ "ELLA BOEKELHEIDE",
              Name == "ELIXABETH VOGEL" ~ "ELIZABETH VOGEL",
              Name == "ESTELLE WALTHNER" ~ "ESTELLE WALTNER",
              Name == "GRACE MCELROY" ~ "GRACE MCELROY",
              Name == "JADY WOLF" ~ "JACY WOLF",
              Name == "KATE MCELROY" ~ "KATE MCELROY",
              Name == "HADLEY GJERDSE" ~ "HADLEY GJERDE",
              Name == "HADASAH OLSON" ~ "HADASSAH OLSON",
              Name == "HANNA DELINE" ~ "HANNAH DELINE",
              Name == "HAZEL KANNEGAITER" ~ "HAZEL KANNEGIETER",
              Name == "ISABELLE WOODING" ~ "ISABELLE WOODRING",
              Name == "JADEYN THIN ELK" ~ "JADEN THIN ELK",
              Name == "KATIE ALLBEE" ~ "KATIE ALBEE",
              Name == "KEIDI OLSON" ~ "HEIDI OLSON",
              Name == "KENNAEDEE WAGNER" ~ "KENNADEE WAGNER",
              Name == "KINSLEY EVANS" ~ "KINSEY EVANS",
              Name == "LIBERTY TRYGSTAD" ~ "LIBERTY TRYGSTAD",
              Name == "LILYTH CATCHES" ~ "LILLYTH CATCHES",
              Name == "LIVY SWANSON" ~ "LILY SWANSON",
              Name == "MADILYN GELHAUS" ~ "MADISYN GELLHAUS",
              Name == "MAKAYLA HEESCH" ~ "MAKAYLA HEESCH",
              Name == "MORGAN VANVEEN" ~ "MORGAN VANVEEN",
              Name == "MACIAH BRANTNER" ~ "MICAIAH BRANTNER",
              Name == "RHYANN ROSELAND" ~ "RHYAN ROSELAND",
              Name == "RHAYONNA HOOD" ~ "RAHYONNA HOOD",
              Name == "SARAH VANDEBERG" ~ "SARAH VANDEBERG",
              Name == "SIETA WISEREMA" ~ "SIETA WIERSEMA",
              Name == "SOPHIE REDLER" ~ "SOPHIA REDLER",
              Name == "TANAYA PACHECO" ~ "TANAYIA PACHECO",
              Name %in% c("WI WANBIL WI LITTLE THUNDER","WI WANBLI WI LITTLE THUN") ~ "WI WANBLI WI LITTLE THUNDER",
              TRUE ~ Name
            )) %>% 
            mutate(School = case_when(
              School %in% c("Andes Central/Dakota Christian") ~ "Andes Cen/Dak Ch",
              School %in% c("Clark/Willow") ~ "Clark/Willow Lake",
              School %in% c("Cheyenne-Eagle Butte","Cheyenne-Eag") ~ "CEB",
              School %in% c("DR Smary","DR St. Marys","Dell Rapids St. Mary") ~ "DR St. Mary",
              School == "Dell Rapis" ~ "Dell Rapids",
              School == "Deubrook" ~ "Deubrook Area",
              School %in% c("Elkton-Lake") ~ "Elkton-Lake Benton",
              School %in% c("Faulkton") ~ "Faulkton Area",
              School %in% c("Freeman ") ~ "Freeman",
              School %in% c("Freeman Acad","Freeman Academy/Marion") ~ "Freeman Acad/Mar",
              School %in% c("Frederick") ~ "Frederick Area",
              School %in% c("Great Plains Lutheran","Great Plains") ~ "Great Plains Lut",
              School %in% c("Groton") ~ "Groton Area",
              School %in% c("Herreid/Selby") ~ "Herreid/Selby Area",
              School %in% c("Highmore") ~ "Highmore-Harrold",
              School %in% c("Iroquois/Lake Preston") ~ "Iroquois/Lak",
              School %in% c("James","JVC","James Valley","James Valley Christian") ~ "James Valley Chr",
              School %in% c("Kadoka Area") ~ "Kadoka",
              School %in% c("Kimball White Lake") ~ "Kimball/White Lake",
              School %in% c("Little WOund") ~ "Little Wound",
              School %in% c("McCook Centr","McCook","McCook Central/Montrose") ~ "McCook-Central",
              School %in% c("Mount Vernon/Plankinton","Mount Vernon") ~ "Mount Vernon/Pla",
              School %in% c("Oldham-Ramon") ~ "Oldham-Ramona-Rutland",
              School == "Potter county" ~ "Potter County",
              School %in% c("Pierre") ~ "Pierre T.F. Riggs",
              School %in% c("Rapid City C","Rapid City Christian") ~ "RC Christian",
              School == "SF Lincoln" ~ "Sioux Falls Lincoln",
              School %in% c("Sioux Falls Christian") ~ "SF Christian",
              School %in% c("Sioux Falls Jefferson") ~ "SF Jefferson",
              School %in% c("Sioux Falls Roosevelt") ~ "SF Roosevelt",
              School %in% c("Sioux Falls Washington") ~ "SF Washington",
              School %in% c("SBA","Sunshine") ~ "Sunshine Bible A",
              School %in% c("SBA","Sunshine","Sunshine Bible Academy") ~ "Sunshine Bible A",
              School %in% c("Sanborn Central/Woonsocket","Sanborn Cent","Sanborn") ~ "Sanborn Cent/Woo",
              School %in% c("Suly Buttes") ~ "Sully Buttes",
              School %in% c(" Stanley County","Stanely County") ~ "Stanley County",
              School %in% c("St. Francis INdian") ~ "St. Francis Indian",
              School == "Webster" ~ "Webster Area",
              TRUE ~ School)) %>% 
              filter(School %in% list_schools$School)
              # inner_join(list_schools, by="School") %>%  #Remove out of state schools
              # select(-school_class)
          readin_girls$Place <- c(1:nrow(readin_girls))

          get_missing_name <- readin_boys %>% 
            anti_join(df_name_boys[[k-1]], by="Name")
          if(nrow(get_missing_name) > 0){
            get_missing_name$id <- c((max(df_name_boys[[k-1]]$id)+1):(max(df_name_boys[[k-1]]$id)+nrow(get_missing_name))) #Assign Missing Name an ID
          }

          get_missing_school <- readin_boys %>% 
            anti_join(df_schools, by="School")
          get_missing_school2 <- get_missing_school %>%
            group_by(School) %>% 
            filter(row_number()==1) %>% #Select first runner from each school
            select(School) %>% 
            arrange(School) %>%
            ungroup()

          if(nrow(get_missing_school2) > 0){
            df_schools_new <- rbind(df_schools, get_missing_school2)
          }else{
            df_schools_new <- df_schools
          }

          if(nrow(get_missing_name) > 0){
            get_missing_name_with_school <- get_missing_name %>% left_join(df_schools_new, by="School")
            df_name_boys[[k]] <- rbind(df_name_boys[[k-1]], get_missing_name_with_school %>% select(Name, id))
          }else{
            df_name_boys[[k]] <- df_name_boys[[k-1]]
          }
        
          write.csv(df_schools_new, paste0(path, "Simulation/df_schools.csv"))
          saveRDS(df_name_boys, paste0(path, "Simulation/df_name_boys.rds"))

        
          split_time <- str_split(readin_boys$Time, ":")
          get_time_sec <- unlist(lapply(1:nrow(readin_boys), function(i) as.numeric(split_time[[i]][1])*60 + 
                                        as.numeric(split_time[[i]][2])))
        if(flg_5k == 1){

          result <- readin_boys %>%
            mutate(Time_sec = get_time_sec) %>%
            select(-X) %>%
            left_join(df_points_boys[[k-1]], by=c("Name","School")) %>%
            left_join(df_name_boys[[k]] %>% select(Name, id_new=id), by="Name") %>%
            rename(points_pre = points,
                  time_min_pre = time_min) %>%
            mutate(points_pre = case_when(is.na(points_pre)~ 1000,TRUE~points_pre),
                  time_min_pre = case_when(is.na(time_min_pre) ~ Time_sec, TRUE ~time_min_pre),
                  id = case_when(is.na(id) ~ id_new, TRUE~id),
                  #  school_id = case_when(is.na(school_id)~school_id_new,TRUE~school_id)
                  ) %>%
            mutate(points_post = points_pre,
                  time_min = case_when(Time_sec <= time_min_pre ~ Time_sec, TRUE ~ time_min_pre)) %>%
            select(-id_new, -time_min_pre) #-school_id_new,
        }else{

          result <- readin_boys %>%
            mutate(Time_sec = 9999) %>%
            select(-X) %>%
            left_join(df_points_boys[[k-1]], by=c("Name","School")) %>%
            left_join(df_name_boys[[k]] %>% select(Name, id_new=id), by="Name") %>%
            rename(points_pre = points,
                  time_min_pre = time_min) %>%
            mutate(points_pre = case_when(is.na(points_pre)~ 1000,TRUE~points_pre),
                  time_min_pre = case_when(is.na(time_min_pre) ~ Time_sec, TRUE ~time_min_pre),
                  id = case_when(is.na(id) ~ id_new, TRUE~id),
                  #  school_id = case_when(is.na(school_id)~school_id_new,TRUE~school_id)
                  ) %>%
            mutate(points_post = points_pre,
                  time_min = case_when(Time_sec <= time_min_pre ~ Time_sec, TRUE ~ time_min_pre)) %>%
            select(-id_new, -time_min_pre) #-school_id_new,
        }


          df_points_boys[[k]] <- do_scoring(result, df_points_boys[[k-1]])
          df_points_boys[[k]] %>% head()
          names(df_points_boys)[k] <- meet_list$meet[k]
          saveRDS(df_points_boys, paste0(path, "Simulation/df_points_boys",".rds"))
          write.csv(df_points_boys[[k]], paste0(path, "Simulation/df_points_boys.csv"))

        if(k == n_i){
          df_points_boys[[k]] <- df_points_boys[[k]] %>% 
            mutate(rnk_pts = row_number()) %>% #Already sorted by Points
            arrange(time_min) %>% 
            mutate(rnk_time = case_when(
              time_min == 9999 ~ NA, #If time is  not valid then have NA rank
              TRUE ~ row_number())) %>% #Otherwise have row number
            mutate(rnk_blnd_tmp = case_when(
              time_min == 9999 ~ rnk_pts,
              TRUE ~ (rnk_pts + rnk_time)/2)) %>% #Blend Ranks, use poins if time is invalid
            arrange(rnk_blnd_tmp, rnk_time) %>% #Tie break by time
            mutate(rnk_blnd = row_number()) %>% #Final Rank
            select(-rnk_pts, -rnk_time, -rnk_blnd_tmp) #%>% 
            # View()

          # saveRDS(list(), paste0(path, "XC Ranking App/sdxc-basic/df_points_boys_list.rds"))
          df_points_boys_list <- readRDS(paste0(path, "XC Ranking App/sdxc-basic/df_points_boys_list.rds"))
          df_points_boys_list[[week_i]] <- df_points_boys[[k]] %>% left_join(list_schools, by="School") # %>% View()
          saveRDS(df_points_boys_list, paste0(path, "XC Ranking App/sdxc-basic/df_points_boys_list.rds"))

          # write.csv(df_points_boys[[k]] %>% left_join(list_schools, by="School"), paste0(path, "XC Ranking App/sdxc-basic/df_points_boys.csv"))
        }

      ### Done Boys

      ### Start Girls

          get_missing_name <- readin_girls %>% 
            anti_join(df_name_girls[[k-1]], by="Name")
          if(nrow(get_missing_name) > 0){
            get_missing_name$id <- c((max(df_name_girls[[k-1]]$id)+1):(max(df_name_girls[[k-1]]$id)+nrow(get_missing_name))) #Assign Missing Name an ID
            get_missing_name_with_school <- get_missing_name %>% left_join(df_schools_new, by="School")
            df_name_girls[[k]] <- rbind(df_name_girls[[k-1]], get_missing_name_with_school %>% select(Name, id))
          }else{
            df_name_girls[[k]] <- df_name_girls[[k-1]]
          }

          saveRDS(df_name_girls, paste0(path, "Simulation/df_name_girls.rds"))

          split_time <- str_split(readin_girls$Time, ":")
          get_time_sec <- unlist(lapply(1:nrow(readin_girls), function(i) as.numeric(split_time[[i]][1])*60 + 
                                        as.numeric(split_time[[i]][2])))

        if(flg_5k == 1){
          result <- readin_girls %>%
            mutate(Time_sec = get_time_sec) %>%
            select(-X) %>%
            left_join(df_points_girls[[k-1]], by=c("Name","School")) %>%
            left_join(df_name_girls[[k]] %>% select(Name, id_new=id), by="Name") %>%
            rename(points_pre = points,
                  time_min_pre = time_min) %>%
            mutate(points_pre = case_when(is.na(points_pre)~ 1000,TRUE~points_pre),
                  time_min_pre = case_when(is.na(time_min_pre) ~ Time_sec, TRUE ~time_min_pre),
                  id = case_when(is.na(id) ~ id_new,TRUE~id),
                  #  school_id = case_when(is.na(school_id)~school_id_new,TRUE~school_id)
                  ) %>%
            mutate(points_post = points_pre,
                  time_min = case_when(Time_sec <= time_min_pre ~ Time_sec, TRUE ~ time_min_pre)) %>%
            select(-id_new, -time_min_pre) #-school_id_new,
        }else{
          result <- readin_girls %>%
            mutate(Time_sec = 9999) %>%
            select(-X) %>%
            left_join(df_points_girls[[k-1]], by=c("Name","School")) %>%
            left_join(df_name_girls[[k]] %>% select(Name, id_new=id), by="Name") %>%
            rename(points_pre = points,
                  time_min_pre = time_min) %>%
            mutate(points_pre = case_when(is.na(points_pre)~ 1000,TRUE~points_pre),
                  time_min_pre = case_when(is.na(time_min_pre) ~ Time_sec, TRUE ~time_min_pre),
                  id = case_when(is.na(id) ~ id_new,TRUE~id),
                  #  school_id = case_when(is.na(school_id)~school_id_new,TRUE~school_id)
                  ) %>%
            mutate(points_post = points_pre,
                  time_min = case_when(Time_sec <= time_min_pre ~ Time_sec, TRUE ~ time_min_pre)) %>%
            select(-id_new, -time_min_pre) #-school_id_new,
        }

          df_points_girls[[k]] <- do_scoring(result, df_points_girls[[k-1]])
          # points_final_girls %>% head()
          names(df_points_girls)[k] <- meet_list$meet[k] 
          saveRDS(df_points_girls, paste0(path, "Simulation/df_points_girls",".rds"))
          write.csv(df_points_girls[[k]], paste0(path, "Simulation/df_points_girls.csv"))

        if(k == n_i){
          df_points_girls[[k]] <- df_points_girls[[k]] %>% 
            mutate(rnk_pts = row_number()) %>% #Already sorted by Points
            arrange(time_min) %>% 
            mutate(rnk_time = case_when(
              time_min == 9999 ~ NA, #If time is  not valid then have NA rank
              TRUE ~ row_number())) %>% #Otherwise have row number
            mutate(rnk_blnd_tmp = case_when(
              time_min == 9999 ~ rnk_pts,
              TRUE ~ (rnk_pts + rnk_time)/2)) %>% #Blend Ranks, use poins if time is invalid
            arrange(rnk_blnd_tmp, rnk_time) %>% #Tie break by time
            mutate(rnk_blnd = row_number()) %>% #Final Rank
            select(-rnk_pts, -rnk_time, -rnk_blnd_tmp) #%>% 
            # View()

          # saveRDS(list(), paste0(path, "XC Ranking App/sdxc-basic/df_points_girls_list.rds"))
          df_points_girls_list <- readRDS(paste0(path, "XC Ranking App/sdxc-basic/df_points_girls_list.rds"))
          df_points_girls_list[[week_i]] <- df_points_girls[[k]] %>% left_join(list_schools, by="School")
          saveRDS(df_points_girls_list, paste0(path, "XC Ranking App/sdxc-basic/df_points_girls_list.rds"))

          # write.csv(df_points_girls[[k]] %>% left_join(list_schools, by="School"), paste0(path, "XC Ranking App/sdxc-basic/df_points_girls.csv"))
        }

      ### Done Girls
      }
    }

  }

