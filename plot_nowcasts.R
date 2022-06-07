library(tidyverse)
# source("R/load_data.R")
source("load_truth.R")

LEAD_TIME <- 14

df <- read_csv("data/submissions.csv.gz")

df_truth <- load_truth(as_of = "2022-06-01")
truth_frozen <- load_frozen_truth(LEAD_TIME, "2021-11-01")

df1 <- df %>%
  filter(
    model == "NowcastHub-MeanEnsemble",
    target == paste0("-", LEAD_TIME, " day ahead inc hosp"),
    type == "quantile",
    location == "DE"
  ) %>%
  pivot_wider(names_from = quantile, names_prefix = "quantile_")


df1 <- df1 %>%
  left_join(df_truth, by = c("target_end_date" = "date", "location", "age_group")) %>%
  left_join(truth_frozen, by = c("target_end_date" = "date", "location", "age_group")) %>%
  drop_na()

df1$age_group <- factor(df1$age_group, levels = c("00+", "00-04", "05-14", "15-34", "35-59", "60-79", "80+"))


ggplot(df1) +
  facet_wrap("age_group", scales = "free_y") +
  geom_ribbon(aes(x = target_end_date, ymin = quantile_0.025, ymax = quantile_0.975),
    fill = "skyblue3", alpha = 0.4
  ) +
  geom_ribbon(aes(x = target_end_date, ymin = quantile_0.25, ymax = quantile_0.75),
    fill = "skyblue3", alpha = 0.5
  ) +
  geom_line(aes(x = target_end_date, y = quantile_0.5),
    color = "skyblue3", linetype = "solid"
  ) +
  geom_line(aes(x = target_end_date, y = truth), color = "firebrick3") +
  geom_line(aes(x = target_end_date, y = frozen_value), linetype = "dashed") +
  labs(x = NULL, y = "7-day hospitalization incidence")
