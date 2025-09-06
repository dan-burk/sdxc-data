
do_scoring <- function(result, df_points, points_wager = 0.05){
  
  x <- nrow(result) #Number of individuals
  n <- 0.5*(x-1)*x #Total number of comparisons.
  
  comparisons <- matrix(0, nrow = x * (x - 1) / 2, ncol = 2)
  counter <- 1
  
  for (i in 1:(x - 1)) {
    for (j in (i + 1):x) {
      comparisons[counter, ] <- c(i, j)
      counter <- counter + 1
    }
  }
  
  result_final <- result
  for(i in 1:n){
    t <- comparisons[i,] #Take the ith comparison
    sub <- result_final[t,] #and subset the results to that comparison
    result_final <- result_final %>% 
      mutate(points_post = case_when(
        Place == t[1] ~ points_post + points_wager*sub$points_post[2], #Add 1%  of the points from the looser to winner
        Place == t[2] ~ points_post - points_wager*sub$points_post[2], #Take 1%  of the points from the looser and subtract
        TRUE ~ points_post
      ))
    
  }
  
  df_points_new <- result_final %>%
    select(Name, School, id, points = points_post, time_min)
  
  get_new_points <- df_points_new %>% anti_join(df_points, by="id")
  

  df_points_rev <- df_points %>%
    left_join(df_points_new, by=c("Name","School","id")) %>% 
    mutate(points = case_when(!is.na(points.y) ~ points.y, TRUE ~ points.x)) %>%
    mutate(time_min = case_when(!is.na(time_min.y) ~ time_min.y, TRUE ~ time_min.x)) %>% 
    select(-points.x,-points.y,-time_min.x,-time_min.y)
  
  points_final <- rbind(df_points_rev, get_new_points) %>% 
    arrange(desc(points))
  
  return(points_final)
}