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
  filter(age_group == "00+",
         location != "DE")

df_age <- df %>% 
  filter(location == "DE",
         age_group != "00+")

ggplot(df_states, aes(x = date, y = value, color = d, group = d)) +
  facet_wrap("location", scales = "free") +
  geom_line()

ggplot(df_states, aes(x = date, y = value, fill = fct_reorder(d, value, .desc = TRUE)), group = d) +
  facet_wrap("location", scales = "free_y") +
  geom_area(position = "identity") +
  scale_fill_viridis_d(direction = 1) +
  theme_bw() +
  labs(x = NULL,
       y = "7-day hospitalization incidence",
       fill = "Days after\ninitial report") +
  scale_x_date(date_breaks = "3 months", minor_breaks = "1 month", date_labels =  "%b %Y") 

ggsave("figures/fraction_absolute_time_states.pdf", width = 300, height = 250, unit = "mm", device = "pdf")



df_fraction_states <- df_states %>% 
  group_by(location, date) %>% 
  mutate(value = value/max(value))

# a <- df_fraction_states %>% 
#   group_by(location, d) %>% 
#   summarize(m = max(date))

ggplot(df_fraction_states, aes(x = date, y = value, fill = fct_reorder(d, value, .desc = TRUE)), group = d) +
  facet_wrap("location", scales = "fixed") +
  geom_area(position = "identity") +
  scale_fill_viridis_d(direction = 1) +
  theme_bw() +
  labs(x = NULL,
       y = "Fraction of final report",
       fill = "Days after\ninitial report") +
  scale_x_date(date_breaks = "3 months", minor_breaks = "1 month", date_labels =  "%b %Y") 

ggsave("figures/fraction_time_states.pdf", width = 300, height = 250, unit = "mm", device = "pdf")


# df_fraction <- df_states %>% 
#   group_by(location, date) %>% 
#   mutate(value = value/max(value)) %>% 
#   group_by(location, d) %>% 
#   summarize(value = mean(value))
# 
# df_fraction <- df_fraction %>% 
#   mutate(d = fct_reorder(d, value, .desc = TRUE)) %>% 
#   arrange(d)
# 
# ggplot(df_fraction, aes(x = location, y = value, fill = d)) +
#   geom_bar(stat = "identity", position = "identity") +
#   scale_fill_viridis_d(direction = 1) +
#   theme_bw() +
#   labs(x = NULL,
#        y = "7-day hospitalization incidence",
#        fill = "Days after initial report")



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
       y = "Fraction of final report",
       fill = "Days after\ninitial report") 

ggsave("figures/fraction_state.pdf", width = 200, height = 120, unit = "mm", device = "pdf")



df_fraction <- df_fraction %>% 
  mutate(d = as.numeric(as.character(d))) %>% 
  arrange(d)

ggplot(df_fraction, aes(x = d, y = value, color = location, group = location)) +
  geom_line() +
  ylim(c(0, 1)) +
  theme_bw() +
  theme(
    panel.grid.major = element_line(size = 0.05),
    panel.grid.minor = element_line(size = 0.05)
  ) +
  labs(x = "Days after initial report",
       y = "Fraction of final report",
       color = "State") 

ggsave("figures/fraction_line_state.pdf", width = 200, height = 120, unit = "mm", device = "pdf")


### BY AGE GROUP

df_fraction_age <- df_age %>% 
  group_by(age_group, date) %>% 
  mutate(value = value/max(value))

ggplot(df_age, aes(x = date, y = value, fill = fct_reorder(d, value, .desc = TRUE)), group = d) +
  facet_wrap("age_group", scales = "free_y") +
  geom_area(position = "identity") +
  scale_fill_viridis_d(direction = 1) +
  theme_bw() +
  labs(x = NULL,
       y = "7-day hospitalization incidence",
       fill = "Days after\ninitial report") +
  scale_x_date(date_breaks = "3 months", minor_breaks = "1 month", date_labels =  "%b %Y") 

ggsave("figures/fraction_absolute_time_age.pdf", width = 300, height = 200, unit = "mm", device = "pdf")


ggplot(df_fraction_age, aes(x = date, y = value, fill = fct_reorder(d, value, .desc = TRUE)), group = d) +
  facet_wrap("age_group", scales = "fixed") +
  geom_area(position = "identity") +
  scale_fill_viridis_d(direction = 1) +
  theme_bw() +
  labs(x = NULL,
       y = "Fraction of final report",
       fill = "Days after\ninitial report") +
  scale_x_date(date_breaks = "3 months", minor_breaks = "1 month", date_labels =  "%b %Y") 

ggsave("figures/fraction_time_age.pdf", width = 300, height = 200, unit = "mm", device = "pdf")


df_fraction <- df_age %>% 
  group_by(age_group, d) %>% 
  summarize(value = sum(value)) %>% 
  group_by(age_group) %>% 
  mutate(value = value/max(value))

df_fraction <- df_fraction %>% 
  mutate(d = fct_reorder(d, value, .desc = TRUE)) %>% 
  arrange(d)


ggplot(df_fraction, aes(x = age_group, y = value, fill = d)) +
  geom_bar(stat = "identity", position = "identity") +
  scale_fill_viridis_d(direction = 1) +
  theme_bw() +
  theme(
    panel.grid.major = element_line(size = 0.05),
    panel.grid.minor = element_line(size = 0.05)
  ) +
  labs(x = NULL,
       y = "Fraction of final report",
       fill = "Days after\ninitial report") 

ggsave("figures/fraction_age.pdf", width = 200, height = 120, unit = "mm", device = "pdf")
