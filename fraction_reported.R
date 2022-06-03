library(tidyverse)

MAX_DELAY <- 80

df_truth <- read_csv("../hospitalization-nowcast-hub/data-truth/COVID-19/COVID-19_hospitalizations_preprocessed.csv",
  show_col_types = FALSE
)

df <- df_truth %>%
  drop_na() %>%
  filter(
    location == "DE",
    age_group == "00+"
  ) %>%
  select(c(date, location, age_group, paste0("value_", 0:MAX_DELAY, "d")))

# cumsum for one fixed date
df <- df %>%
  pivot_longer(starts_with("value")) %>%
  group_by(date, location, age_group) %>%
  mutate(value = cumsum(value))

df <- df %>%
  filter(name %in% paste0("value_", c(0, 7, 14, 21, 28, MAX_DELAY), "d"))

df$name <- factor(df$name, levels = paste0("value_", c(0, 7, 14, 21, 28, MAX_DELAY), "d"))

df <- df %>%
  pivot_wider(names_from = name, values_from = value) %>%
  rowwise() %>%
  mutate(across(starts_with("value"), function(x) x / value_80d))

df <- df %>%
  pivot_longer(starts_with("value"))

df$name <- factor(df$name, levels = rev(paste0("value_", c(0, 7, 14, 21, 28, MAX_DELAY), "d")), labels = rev(c(0, 7, 14, 21, 28, MAX_DELAY)))

ggplot(df, aes(x = date, y = value, fill = name)) +
  #facet_wrap("age_group") +
  geom_area(position = "identity", alpha = 0.5, color = "black", size = 0.1) +
  scale_fill_brewer(palette = "RdYlGn") +
  scale_x_date(date_labels = "%b %Y") +
  scale_y_continuous(labels = function(y) ifelse(y == 0, "0", y)) +
  labs(
    x = NULL,
    y = "Fraction reported \n(of final data)",
    fill = "Days after\nfirst report"
  ) +   
  theme(text = element_text(size = 24),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 18),
        axis.text = element_text(size = 18))

ggsave("figures/fraction_reported.pdf", width = 300, height = 150, unit = "mm", device = "pdf")



#### 7 day

df_truth <- read_csv("../hospitalization-nowcast-hub/data-truth/COVID-19/COVID-19_hospitalizations_preprocessed.csv",
                     show_col_types = FALSE
)

df <- df_truth %>%
  drop_na() %>%
  filter(
    location == "DE",
    age_group == "00+"
  ) %>%
  select(c(date, location, age_group, paste0("value_", 0:MAX_DELAY, "d")))

# cumsum for one fixed date
df <- df %>%
  pivot_longer(starts_with("value")) %>%
  group_by(date, location, age_group) %>%
  mutate(value = cumsum(value))

df <- df %>%
  filter(name %in% paste0("value_", c(0, 7, 14, 21, 28, MAX_DELAY), "d"))

df$name <- factor(df$name, levels = paste0("value_", c(0, 7, 14, 21, 28, MAX_DELAY), "d"))

df <- df %>%
  pivot_wider(names_from = name, values_from = value) 

df <- df %>% 
  ungroup() %>% 
  mutate(across(starts_with("value"), roll_sumr, n = 7))

df <- df %>%
  drop_na() %>% 
  rowwise() %>%
  mutate(across(starts_with("value"), function(x) x / value_80d))

df <- df %>%
  pivot_longer(starts_with("value"))

df$name <- factor(df$name, levels = rev(paste0("value_", c(0, 7, 14, 21, 28, MAX_DELAY), "d")), labels = rev(c(0, 7, 14, 21, 28, MAX_DELAY)))

ggplot(df, aes(x = date, y = value, fill = name)) +
  #facet_wrap("age_group") +
  geom_area(position = "identity", alpha = 0.5, color = "black", size = 0.1) +
  scale_fill_brewer(palette = "RdYlGn") +
  scale_x_date(date_labels = "%b %Y") +
  scale_y_continuous(labels = function(y) ifelse(y == 0, "0", y)) +
  labs(
    x = NULL,
    y = "Fraction reported \n(of final data)",
    fill = "Days after\nfirst report"
  ) +   
  theme(text = element_text(size = 24),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 18),
        axis.text = element_text(size = 18))


#####

df <- df_truth %>%
  drop_na() %>%
  filter(
    location == "DE",
    age_group == "00+"
  ) %>%
  select(c(date, location, age_group, paste0("value_", 0:MAX_DELAY, "d")))


df <- df %>%
  select(c(date, paste0("value_", c(0, 7, 14, 21, 28, MAX_DELAY), "d")))

df <- df %>% 
  ungroup() %>% 
  mutate(across(starts_with("value"), roll_sumr, n = 7))

df <- df %>%
  drop_na() %>% 
  rowwise() %>% 
  mutate(final = sum(c_across(starts_with("value")), na.rm = TRUE))
         
df <- df %>% 
  mutate(across(starts_with("value"), function(x) x / final))

df <- df %>%
  pivot_longer(starts_with("value"))

df$name <- factor(df$name, levels = rev(paste0("value_", c(0, 7, 14, 21, 28, MAX_DELAY), "d")), labels = rev(c(0, 7, 14, 21, 28, MAX_DELAY)))

ggplot(df, aes(x = date, y = value, fill = name)) +
  #facet_wrap("age_group") +
  geom_area(position = "stack", alpha = 0.5, color = "black", size = 0.1) +
  scale_fill_brewer(palette = "RdYlGn") +
  scale_x_date(date_labels = "%b %Y") +
  scale_y_continuous(labels = function(y) ifelse(y == 0, "0", y)) +
  labs(
    x = NULL,
    y = "Fraction reported \n(of final data)",
    fill = "Days after\nfirst report"
  ) +   
  theme(text = element_text(size = 24),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 18),
        axis.text = element_text(size = 18))




#####

df_truth <- read_csv("../hospitalization-nowcast-hub/data-truth/COVID-19/COVID-19_hospitalizations_preprocessed.csv",
                     show_col_types = FALSE
)

df <- df_truth %>%
  drop_na() %>%
  filter(
    location == "DE",
    age_group == "00+"
  ) %>%
  select(c(date, location, age_group, paste0("value_", 0:MAX_DELAY, "d")))

# cumsum for one fixed date
df <- df %>%
  pivot_longer(starts_with("value")) %>%
  group_by(date, location, age_group) %>%
  mutate(value = cumsum(value))

df <- df %>%
  filter(name %in% paste0("value_", c(0, 7, 14, 21, 28, MAX_DELAY), "d"))

df$name <- factor(df$name, levels = paste0("value_", c(0, 7, 14, 21, 28, MAX_DELAY), "d"))

df <- df %>%
  pivot_wider(names_from = name, values_from = value) 

df <- df %>% 
  ungroup() %>% 
  mutate(across(starts_with("value"), roll_sumr, n = 7))

df <- df %>%
  drop_na() %>% 
  rowwise() %>%
  mutate(across(starts_with("value"), function(x) x / value_80d))

# revert cumsum for one fixed date
df <- df %>%
  pivot_longer(starts_with("value")) %>%
  group_by(date, location, age_group) %>%
  mutate(value = value - lag(value, default = 0))

df$name <- factor(df$name, levels = paste0("value_", c(0, 7, 14, 21, 28, MAX_DELAY), "d"))

df <- df %>%
  pivot_longer(starts_with("value"))

df$name <- factor(df$name, levels = rev(paste0("value_", c(0, 7, 14, 21, 28, MAX_DELAY), "d")), labels = rev(c(0, 7, 14, 21, 28, MAX_DELAY)))

ggplot(df, aes(x = date, y = value, fill = name)) +
  #facet_wrap("age_group") +
  geom_area(aes(color = name), position = "stack", alpha = 0.5, size = 0.1) +
  scale_fill_brewer(palette = "RdYlGn") +
  scale_color_brewer(palette = "RdYlGn") +
  scale_x_date(date_labels = "%b %Y") +
  scale_y_continuous(labels = function(y) ifelse(y == 0, "0", y)) +
  labs(
    x = NULL,
    y = "Fraction reported \n(of final data)",
    fill = "Days after\nfirst report"
  ) +   
  theme(text = element_text(size = 24),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 18),
        axis.text = element_text(size = 18)) +
  guides(color = "none")

ggsave("figures/fraction_reported7.pdf", width = 300, height = 175, unit = "mm", device = "pdf")

