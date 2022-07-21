library(tidyverse)
source("load_truth.R")

Sys.setlocale("LC_ALL", "C")

for(d in c(0, 2, 14, 7, 28, 35, 42, 49, 70)){
  print(d)
  t <- load_frozen_truth(d, "2021-11-01")
  write_csv(t, paste0("data/truth_frozen/truth_frozen_", d, "d.csv"))
}

df_all = data.frame()

for(delay in c(0, 2, 14, 7, 28, 35, 42, 49, 70)){
  t <- read_csv(paste0("data/truth_frozen/truth_frozen_", delay, "d.csv"),
                show_col_types = FALSE) %>% 
    mutate(d = delay)
  df_all <- bind_rows(df_all, t)
  }



df <- df_all

df <- df %>% 
  mutate(d = as_factor(d))

df <- df %>% 
  filter(age_group == "00+",
         location == "DE") 

# compute 7-day rolling sum within each stratum
df <- df %>%
  group_by(d) %>%
  mutate(truth = sum_run(frozen_value, 7, na_pad = TRUE)) %>%
  select(d, date, location, age_group, truth) %>%
  drop_na()


df <- df %>% 
  filter(age_group == "00+")

ggplot(df, aes(x = date, y = truth, color = d, group = d)) +
  facet_wrap("location", scales = "free") +
  geom_line()


df2 <- df %>% 
  filter(location == "DE")

ggplot(df2, aes(x = date, y = truth, fill = d, group = d)) +
  facet_wrap("location", scales = "free") +
  geom_area(position = "identity")

ggplot(df2, aes(x = date, y = truth, fill = fct_reorder(d, truth, .desc = TRUE)), group = d) +
  facet_wrap("location", scales = "free") +
  geom_area(position = "identity") +
  scale_fill_viridis_d(direction = 1) +
  theme_bw() +
  labs(x = NULL,
       y = "7-day hospitalization incidence",
       fill = "Days after initial report")


df3 <- df2 %>% 
  group_by(date) %>% 
  mutate(truth = truth/max(truth))

ggplot(df3, aes(x = date, y = truth, fill = fct_reorder(d, truth, .desc = TRUE)), group = d) +
  facet_wrap("location", scales = "free") +
  geom_area(position = "identity") +
  scale_fill_viridis_d(direction = 1) +
  theme_bw() +
  labs(x = NULL,
       y = "7-day hospitalization incidence",
       fill = "Days after initial report")
