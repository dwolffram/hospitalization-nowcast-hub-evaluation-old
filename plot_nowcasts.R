library(tidyverse)
source("R/load_data.R")

df <- read_csv("data/submissions.csv.gz")

df_truth <- load_truth(location = "DE", age_group = "00+", as_of = "2022-05-01") %>%
  select(-value) %>%
  rename(truth = value_7d)

# df_truth0 <- read_csv("../hospitalization-nowcast-hub/data-truth/COVID-19/COVID-19_hospitalizations_preprocessed.csv",
#                       show_col_types = FALSE) %>% 
#   select(c(date, location, age_group, value_0d)) %>% 
#   rename(truth0 = value_0d) %>%
#   group_by(location, age_group) %>%
#   mutate(truth0 = roll_sumr(truth0, n = 7))


df1 <- df %>% 
  filter(model == "NowcastHub-MeanEnsemble",
         target == "0 day ahead inc hosp",
         type == "quantile",
         location == "DE") %>% 
  pivot_wider(names_from = quantile, names_prefix = "quantile_")

# df1 <- df1 %>% 
#   left_join(df_truth, by = c("target_end_date" = "date", "location", "age_group")) %>% 
#   left_join(df_truth0, by = c("target_end_date" = "date", "location", "age_group")) %>% 
#   drop_na()

df1 <- df1 %>% 
  left_join(df_truth, by = c("target_end_date" = "date", "location", "age_group")) %>% 
  left_join(r1, by = c("target_end_date" = "date", "location", "age_group")) %>% 
  drop_na()

ggplot(df1) +
  facet_wrap("age_group", scales = "free_y") +
  geom_ribbon(aes(x = target_end_date, ymin = quantile_0.025, ymax = quantile_0.975), 
              fill = "skyblue3", alpha = 0.5) +
  geom_ribbon(aes(x = target_end_date, ymin = quantile_0.25, ymax = quantile_0.75), 
              fill = "skyblue3", alpha = 0.7) +
  geom_line(aes(x = target_end_date, y = quantile_0.5), 
            color = "skyblue3", linetype = "solid") +
  geom_line(aes(x = target_end_date, y = truth)) +
  geom_line(aes(x = target_end_date, y = frozen_value)) +
  labs(x = NULL, y = "7-day hospitalization incidence")
 


df1 <- df1 %>% 
  # left_join(df_truth, by = c("target_end_date" = "date", "location", "age_group")) %>% 
  left_join(r1, by = c("target_end_date" = "date", "location", "age_group")) %>% 
  drop_na()

ggplot(df1) +
  facet_wrap("age_group", scales = "free_y") +
  geom_ribbon(aes(x = target_end_date, ymin = quantile_0.025, ymax = quantile_0.975), 
              fill = "skyblue3", alpha = 0.5) +
  geom_ribbon(aes(x = target_end_date, ymin = quantile_0.25, ymax = quantile_0.75), 
              fill = "skyblue3", alpha = 0.7) +
  geom_line(aes(x = target_end_date, y = quantile_0.5), 
            color = "skyblue3", linetype = "solid") +
  # geom_line(aes(x = target_end_date, y = truth)) +
  geom_line(aes(x = target_end_date, y = frozen_value)) +
  labs(x = NULL, y = "7-day hospitalization incidence")
