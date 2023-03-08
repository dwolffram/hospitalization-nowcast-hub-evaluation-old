library(tidyverse)
source("utils.R")

df <- load_data(add_baseline = TRUE, add_median = TRUE, shorten_names = TRUE, fix_data = TRUE,
                add_truth = TRUE, exclude_missing = TRUE, eval_date = "2022-08-08")

df <- df %>%
  rowwise() %>%
  mutate(score = score(value, truth, type, quantile))

df <- df %>%
  select(-c(pathogen, value, truth))

write_csv(df, paste0("data/scores.csv.gz"))

# Aggregate scores
df <- df %>%
  group_by(model, location, age_group, target, type) %>%
  summarize(score = mean(score))

write_csv(df, paste0("data/scores_aggregated.csv.gz"))

# d <- read_csv("data/scores.csv.gz")
# 
# s <- filter_data(d, level = "national")
# 
# s <- s%>%
#     group_by(model, location, age_group, target, type) %>%
#     summarize(score = mean(score))
# 
# s <- s%>%
#   group_by(model, type) %>%
#   summarize(score = mean(score))

## All targets get "same weight" bc of two-step aggregation

# s <- df %>%
#   filter(
#     location == "DE",
#     age_group == "00+"
#   ) %>%
#   group_by(model, type) %>%
#   summarize(score = mean(score))
