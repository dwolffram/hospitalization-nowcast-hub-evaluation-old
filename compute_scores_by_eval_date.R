source("utils.R")

compute_scores <- function(df, eval_date = "2022-08-08"){
  df_truth <- load_truth(as_of = eval_date)
  
  df <- df %>%
    left_join(df_truth, by = c("location", "age_group", "target_end_date" = "date"))
  
  df <- df %>%
    rowwise() %>%
    mutate(score = score(value, truth, type, quantile))
  
  df <- df %>% 
    mutate(level = ifelse(location == "DE", "national", "states"),
           level = ifelse(age_group != "00+", "age", level))
  
  df <- df %>% 
    group_by(level, model) %>% 
    summarize(score = mean(score))
  
  write_csv(df, paste0("data/scores_by_date/scores_", eval_date, ".csv.gz"))
  return(df)
}


df <- load_data(add_baseline = FALSE, add_median = FALSE, shorten_names = TRUE, fix_data = TRUE,
                add_truth = FALSE, exclude_missing = TRUE)

df <- df %>% 
  filter(type == "quantile")


for (d in as.list(seq(as.Date("2022-05-10"), as.Date("2022-08-08"), by=10))) {
  print(d)
  compute_scores(df, d)
}


for (d in as.list(seq(as.Date("2022-12-21"), as.Date("2022-12-31"), by=10))) {
  print(d)
  compute_scores(df, d)
}





# df_scores <- compute_scores(df)
# 
# 
# df_truth <- load_truth(as_of = "2022-08-08")
# 
# df <- df %>%
#   left_join(df_truth, by = c("location", "age_group", "target_end_date" = "date"))
# 
# df <- df %>%
#   rowwise() %>%
#   mutate(score = score(value, truth, type, quantile))
# 
# df <- df %>%
#   select(-c(pathogen, value, truth))
# 
# df <- df %>% 
#   mutate(level = ifelse(location == "DE", "national", "states"),
#          level = ifelse(age_group != "00+", "age", level))
# 
# s <- df %>% 
#   group_by(level, model) %>% 
#   summarize(score = mean(score))
