library(tidyverse)
source("utils.R")

df <- load_data(add_baseline = TRUE, add_median = FALSE, shorten_names = TRUE, fix_data = TRUE,
                add_truth = TRUE, exclude_missing = TRUE, eval_date = "2022-08-08")

df <- df %>% 
  filter(type == "quantile")

df_median <- df %>% 
  filter(quantile == 0.5) %>% 
  rename(med = value) %>% 
  select(-c(quantile, pathogen, retrospective, truth))

df <- df %>%
  left_join(df_median)

df_scores <- df %>%
  rowwise() %>%
  mutate(score = score(value, truth, type, quantile),
         spread = score(value, med, type, quantile))

# df_scores <- df_scores %>% 
#   mutate(overprediction = (med > truth),
#          underprediction = (med < truth))

# df_scores <- df_scores %>% 
#   mutate(penalty = score - spread)

df_scores <- df_scores %>% 
  mutate(overprediction = ifelse(med > truth, score - spread, 0),
         underprediction = ifelse(med < truth, score - spread, 0))




df_national <- filter_data(df_scores, level = "national")

df_national <- df_national %>% 
  group_by(model) %>% 
  summarize(spread = mean(spread),
            overprediction = mean(overprediction),
            underprediction = mean(underprediction),
            score = mean(score))

write_csv(df_national, paste0("data/wis_national.csv.gz"))



df_states <- filter_data(df_scores, level = "states")

df_states <- df_states %>% 
  group_by(model) %>% 
  summarize(spread = mean(spread),
            overprediction = mean(overprediction),
            underprediction = mean(underprediction),
            score = mean(score))

write_csv(df_states, paste0("data/wis_states.csv.gz"))


df_age <- filter_data(df_scores, level = "age")

df_age <- df_age %>% 
  group_by(model) %>% 
  summarize(spread = mean(spread),
            overprediction = mean(overprediction),
            underprediction = mean(underprediction),
            score = mean(score))

write_csv(df_age, paste0("data/wis_age.csv.gz"))




#### 0-7 days

df7 <- df_scores %>% 
  filter(target %in% paste(0:7*-1, "day ahead inc hosp"))

df_national <- filter_data(df7, level = "national")

df_national <- df_national %>% 
  group_by(model) %>% 
  summarize(spread = mean(spread),
            overprediction = mean(overprediction),
            underprediction = mean(underprediction),
            score = mean(score))

write_csv(df_national, paste0("data/wis_national_7d.csv.gz"))



df_states <- filter_data(df7, level = "states")

df_states <- df_states %>% 
  group_by(model) %>% 
  summarize(spread = mean(spread),
            overprediction = mean(overprediction),
            underprediction = mean(underprediction),
            score = mean(score))

write_csv(df_states, paste0("data/wis_states_7d.csv.gz"))


df_age <- filter_data(df7, level = "age")

df_age <- df_age %>% 
  group_by(model) %>% 
  summarize(spread = mean(spread),
            overprediction = mean(overprediction),
            underprediction = mean(underprediction),
            score = mean(score))

write_csv(df_age, paste0("data/wis_age_7d.csv.gz"))
