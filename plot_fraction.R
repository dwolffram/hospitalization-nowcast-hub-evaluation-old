library(tidyverse)
source("load_truth.R")

Sys.setlocale("LC_ALL", "C")

# for(d in c(0, 2, 14, 7, 28, 35, 42, 49, 70)){
#   print(d)
#   t <- load_frozen_truth(d, "2021-11-01")
#   write_csv(t, paste0("data/truth_frozen/truth_frozen_", d, "d.csv"))
# }

df = data.frame()
for(delay in c(0, 2, 14, 7, 28, 35, 42, 49, 70)){
  t <- read_csv(paste0("data/truth_frozen/truth_frozen_", delay, "d.csv"),
                show_col_types = FALSE) %>% 
    mutate(d = delay)
  df <- bind_rows(df, t)
  }

df <- df %>% 
  mutate(d = as_factor(d),
         location = str_sub(location, -2, -1)) %>% 
  rename(value = frozen_value) %>% 
  filter(date <= "2022-05-10")

df_states <- df %>% 
  filter(age_group == "00+")

df_age <- df %>% 
  filter(location == "DE")

ggplot(df_states, aes(x = date, y = value, color = d, group = d)) +
  facet_wrap("location", scales = "free") +
  geom_line()

ggplot(df_states, aes(x = date, y = value, fill = fct_reorder(d, value, .desc = TRUE)), group = d) +
  facet_wrap("location", scales = "free") +
  geom_area(position = "identity") +
  scale_fill_viridis_d(direction = 1) +
  theme_bw() +
  labs(x = NULL,
       y = "7-day hospitalization incidence",
       fill = "Days after initial report")


df_fraction_states <- df_states %>% 
  group_by(location, date) %>% 
  mutate(value = value/max(value))

# a <- df_fraction_states %>% 
#   group_by(location, d) %>% 
#   summarize(m = max(date))

ggplot(df_fraction_states, aes(x = date, y = value, fill = fct_reorder(d, value, .desc = TRUE)), group = d) +
  facet_wrap("location", scales = "free") +
  geom_area(position = "identity") +
  scale_fill_viridis_d(direction = 1) +
  theme_bw() +
  labs(x = NULL,
       y = "7-day hospitalization incidence",
       fill = "Days after initial report")


df_fraction <- df_states %>% 
  group_by(location, date) %>% 
  mutate(value = value/max(value)) %>% 
  group_by(location, d) %>% 
  summarize(value = mean(value))

df_fraction <- df_fraction %>% 
  mutate(d = fct_reorder(d, value, .desc = TRUE)) %>% 
  arrange(d)

ggplot(df_fraction, aes(x = location, y = value, fill = d)) +
  geom_bar(stat = "identity", position = "identity") +
  scale_fill_viridis_d(direction = 1) +
  theme_bw() +
  labs(x = NULL,
       y = "7-day hospitalization incidence",
       fill = "Days after initial report")





df_fraction <- df_states %>% 
  group_by(location, d) %>% 
  summarize(value = sum(value)) %>% 
  group_by(location) %>% 
  mutate(value = value/max(value))

df_fraction <- df_fraction %>% 
  mutate(d = fct_reorder(d, value, .desc = TRUE)) %>% 
  arrange(d)


ggplot(df_fraction, aes(x = fct_reorder(location, value, .fun = min, .desc = TRUE), y = value, fill = d)) +
  geom_bar(stat = "identity", position = "identity") +
  scale_fill_viridis_d(direction = 1) +
  theme_bw(base_size = 11) +
  theme(
    panel.grid.major = element_line(size = 0.05),
    panel.grid.minor = element_line(size = 0.05)
  ) +
  labs(x = NULL,
       y = "Fraction of final hosp. inc.",
       fill = "Days after\ninitial report") 

ggsave("figures/fraction_by_state.pdf", width = 160, height = 110, unit = "mm", device = "pdf")



df_fraction <- df_fraction %>% 
  mutate(d = as.numeric(as.character(d))) %>% 
  arrange(d)

ggplot(df_fraction, aes(x = d, y = value, color = location, group = location)) +
  geom_line() +
  ylim(c(0, 1)) +
  theme_bw(base_size = 11) +
  theme(
    panel.grid.major = element_line(size = 0.05),
    panel.grid.minor = element_line(size = 0.05)
  ) +
  labs(x = "Days after initial report",
       y = "Fraction of final hosp. inc.",
       color = NULL) 

ggsave("figures/fraction_by_state_over_time.pdf", width = 160, height = 110, unit = "mm", device = "pdf")

