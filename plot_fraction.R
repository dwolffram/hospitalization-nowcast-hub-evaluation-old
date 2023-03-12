library(tidyverse)
library(patchwork)
source("load_truth.R")

Sys.setlocale("LC_ALL", "C")

frozen_sum <- function(df, indices_frozen) {
  if (nrow(df) != 7) {
    return(NA)
  } else {
    values <- df %>%
      ungroup() %>%
      select(any_of(paste0("value_", 0:(ncol(indices_frozen) - 1), "d")))
    
    sum(values[indices_frozen[, 1:ncol(values)]])
  }
}

load_frozen_truth <- function(lead_time = 0, start_date = "2021-11-01") {
  indices_frozen <- make_frozen_indices(lead_time)
  
  df <- read_csv("../hospitalization-nowcast-hub/data-truth/COVID-19/COVID-19_hospitalizations.csv",
                 show_col_types = FALSE
  ) %>%
    filter(date >= start_date) %>% 
    rename(value_81d = `value_>80d`)
  
  df <- df %>%
    group_by(location, age_group) %>%
    run_by(idx = "date", k = "7 days") %>%
    mutate(frozen_value = runner(x = ., f = function(x) {
      frozen_sum(x, indices_frozen)
    })) %>%
    select(c(date, location, age_group, frozen_value)) %>%
    drop_na()
}

# t <- load_frozen_truth(81, "2021-11-01")
# write_csv(t, paste0("data/truth_frozen/truth_frozen_", 81, "d.csv"))
# 
# 
# for(d in c(0, 2, 14, 7, 28, 35, 42, 49, 70)){
#   print(d)
#   t <- load_frozen_truth(d, "2021-11-01")
#   write_csv(t, paste0("data/truth_frozen/truth_frozen_", d, "d.csv"))
# }

df <- data.frame()
for (delay in c(0, 2, 7, 14, 28, 35, 42, 49, 70, 81)) {
  t <- read_csv(paste0("data/truth_frozen/truth_frozen_", delay, "d.csv"),
    show_col_types = FALSE
  ) %>%
    mutate(d = delay)
  df <- bind_rows(df, t)
}

df <- df %>%
  mutate(
    d = as_factor(d),
    location = str_sub(location, -2, -1)
  ) %>%
  rename(value = frozen_value) %>%
  filter(date >= "2021-11-22",
         date <= "2022-04-29")
  #filter(date <= "2022-08-11")
  #filter(date <= "2022-05-30")

df$d <- factor(df$d, levels = sort(unique(df$d)), labels = c(0, 2, 7, 14, 28, 35, 42, 49, 70, "70+"))

df_national <- df %>%
  filter(
    location == "DE",
    age_group == "00+"
  )

df_states <- df %>%
  filter(
    age_group == "00+",
    location != "DE"
  )

df_age <- df %>%
  filter(
    location == "DE",
    age_group != "00+"
  )



### PERCENTAGE (OVER TIME)

# National
df_fraction_national <- df_national %>%
  subset(d %in% c(0, 2, 7, 14, 28, 49, 70, "70+")) %>%
  group_by(location, date) %>%
  mutate(value = value / max(value))

ggplot(df_fraction_national, aes(x = date, y = value, fill = fct_reorder(d, value, .desc = TRUE)), group = d) +
  facet_wrap("location", scales = "fixed") +
  geom_area(position = "identity", alpha = 0.5) +
  scale_fill_viridis_d(direction = 1) +
  theme_bw() +
  labs(
    x = NULL,
    y = "Fraction of final report",
    fill = "Days after\ninitial report"
  ) +
  scale_x_date(date_breaks = "2 months", minor_breaks = "1 month", date_labels = "%b %Y")

ggsave("figures/fraction_time_national.pdf", width = 150, height = 120, unit = "mm", device = "pdf")


df_fraction_national <- df_national %>%
  subset(d %in% c(0, 2, 7, 14, 28, 49, 70, "70+")) %>% 
  group_by(location, date) %>%
  mutate(value = value / max(value)) 

df_fraction_national <- df_fraction_national %>% 
  group_by(date) %>% 
  mutate(value = value - lag(value, default = 0, order_by = d))

df_fraction_national$d <- factor(df_fraction_national$d, 
                                 levels = rev(sort(unique(df_fraction_national$d))), 
                                 labels = rev(c(0, 2, 7, 14, 28, 49, 70, "70+")))
                                 #labels = rev(c(0, 2, 7, 14, 28, 35, 42, 49, 70, "70+")))

 
p1 <- ggplot(df_fraction_national, aes(x = date, y = value, fill = d, color = d)) +
  facet_wrap("location", scales = "fixed") +
  geom_area(position = "stack", alpha = 1, size = 0.3) +
  # scale_fill_viridis_d(direction = 1) +
  # scale_color_viridis_d(direction = 1, guide = "none") +
  scale_color_brewer(palette = "RdYlGn", guide = "none") +
  scale_fill_brewer(palette = "RdYlGn") +
  theme_bw() +
  labs(
    x = NULL,
    y = "Fraction of final report",
    fill = "Days after\ninitial report"
  ) +
  scale_x_date(date_breaks = "1 months", minor_breaks = "1 month", date_labels = "%b %Y", expand = c(0.1, 0))

# ggsave("figures/fraction_time_national_new.pdf", width = 150, height = 120, unit = "mm", device = "pdf")







### PERCENTAGE (BY STRATUM)
# States
df_fraction <- df_states %>%
  subset(d %in% c(0, 2, 7, 14, 28, 49, 70, "70+")) %>% 
  group_by(location, d) %>%
  summarize(value = sum(value)) %>%
  group_by(location) %>%
  mutate(value = value / max(value))

df_fraction <- df_fraction %>%
  mutate(d = fct_reorder(d, value, .desc = TRUE)) %>%
  arrange(d)

p2 <- ggplot(df_fraction, aes(x = fct_reorder(location, value, .fun = min, .desc = TRUE), y = value, fill = d)) +
  geom_bar(stat = "identity", position = "identity", alpha = 1) +
  # scale_fill_viridis_d(direction = 1) +
  scale_fill_brewer(palette = "RdYlGn") +
  theme_bw(base_size = 11) +
  theme(
    panel.grid.major = element_line(size = 0.05),
    panel.grid.minor = element_line(size = 0.05)
  ) +
  labs(
    x = NULL,
    y = "Fraction of final report",
    fill = "Days after\ninitial report"
  )

# ggsave("figures/fraction_state_new.pdf", width = 200, height = 120, unit = "mm", device = "pdf")


# Age

# df_fraction <- df_age %>%
#   subset(d %in% c(0, 2, 7, 14, 28, 49, 70, "70+")) %>% 
#   group_by(age_group, d) %>%
#   summarize(value = sum(value)) %>%
#   group_by(age_group) %>%
#   mutate(value = value / max(value))
# 
# df_fraction <- df_fraction %>%
#   mutate(d = fct_reorder(d, value, .desc = TRUE)) %>%
#   arrange(d)
# 
# ggplot(df_fraction, aes(x = age_group, y = value, fill = d)) +
#   geom_bar(stat = "identity", position = "identity", alpha = 1) +
#   # scale_fill_viridis_d(direction = 1) +
#   scale_fill_brewer(palette = "RdYlGn") +
#   theme_bw(base_size = 11) +
#   theme(
#     panel.grid.major = element_line(size = 0.05),
#     panel.grid.minor = element_line(size = 0.05)
#   ) +
#   labs(
#     x = NULL,
#     y = "Fraction of final report",
#     fill = "Days after\ninitial report"
#   )
# 
# ggsave("figures/fraction_time_age.pdf", width = 300, height = 200, unit = "mm", device = "pdf")



### Weekday
library(lubridate)


df_fraction <- df_national %>%
  subset(d %in% c(0, 2, 7, 14, 28, 49, 70, "70+")) %>% 
  mutate(weekday = wday(date, week_start = 1, label = TRUE)) %>% 
  group_by(weekday, d) %>%
  summarize(value = sum(value)) %>%
  group_by(weekday) %>%
  mutate(value = value / max(value))

df_fraction <- df_fraction %>%
  mutate(d = fct_reorder(d, value, .desc = TRUE)) %>%
  arrange(d)

p3 <- ggplot(df_fraction, aes(x = weekday, y = value, fill = d)) +
  geom_bar(stat = "identity", position = "identity") +
  # scale_fill_viridis_d(direction = 1) +
  scale_fill_brewer(palette = "RdYlGn") +
  theme_bw(base_size = 11) +
  theme(
    panel.grid.major = element_line(size = 0.05),
    panel.grid.minor = element_line(size = 0.05)
  ) +
  labs(
    x = NULL,
    y = "Fraction of final report",
    fill = "Days after\ninitial report"
  )


p1 + theme(legend.position = "none", aspect.ratio = 1) + 
  p2 + facet_grid("" ~ "States") + theme(legend.position = "none", 
                                         aspect.ratio = 1,
                                         strip.background.y = element_blank(),
                                         strip.text.y = element_blank()) +
  p3 + facet_grid("" ~ "Weekdays") + theme(aspect.ratio = 1,
                                         strip.background.y = element_blank(),
                                         strip.text.y = element_blank())

ggsave("figures/fractions.pdf", width = 400, height = 120, unit = "mm", device = "pdf")


### Over time by state

df_fraction_states <- df_states %>%
  subset(d %in% c(0, 2, 7, 14, 28, 49, 70, "70+")) %>% 
  group_by(location, date) %>%
  mutate(value = value / max(value))

ggplot(df_fraction_states, aes(x = date, y = value, fill = fct_reorder(d, value, .desc = TRUE)), group = d) +
  facet_wrap("location", scales = "fixed") +
  geom_area(position = "identity") +
  # scale_fill_viridis_d(direction = 1) +
  scale_fill_brewer(palette = "RdYlGn") +
  theme_bw() +
  labs(
    x = NULL,
    y = "Fraction of final report",
    fill = "Days after\ninitial report"
  ) +
  scale_x_date(date_breaks = "3 months", minor_breaks = "1 month", date_labels = "%b %Y")

ggsave("figures/fraction_time_states.pdf", width = 300, height = 250, unit = "mm", device = "pdf")




