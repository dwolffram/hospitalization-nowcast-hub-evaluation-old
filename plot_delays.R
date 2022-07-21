library(tidyverse)
source("load_truth.R")

df <- load_reporting_triangle("2022-04-29")

# d <- df %>% 
#   mutate(across(value_0d:`value_>80d`, function(x) x != 0))

library(ggridges)

d <- df %>% 
  pivot_longer(cols = c(value_0d:`value_>80d`),
               names_to = "delay",
               values_to = "value") %>% 
  filter(delay %in% c("value_7d", "value_14d", "value_21d")) %>% 
  filter(value <= 20)

ggplot(d, aes(x = value, y = delay)) +
  geom_density_ridges(alpha = 0.7)



d <- df %>% 
  filter(age_group == "00+") %>% 
  pivot_longer(cols = c(value_0d:`value_>80d`),
               names_to = "delay",
               values_to = "value") %>%
  filter(delay != "value_>80d",
         delay %in% c("value_7d", "value_14d", "value_21d", "value_28d",
                      "value_35d", "value_42d", "value_49d", "value_56d", 
                      "value_63d", "value_70d", "value_77d")) %>% 
  mutate(horizon = as.factor(as.numeric(str_extract(delay, "-?\\d+"))))

ggplot(d, aes(horizon, value)) +
  geom_point()

ggplot(d, aes(horizon, value)) +
  geom_boxplot(outlier.shape = NA) +
  # coord_cartesian(ylim = c(0, 50)) +
  facet_wrap("location", scales = "free")


e <- d %>% 
  filter(horizon %in% c(7, 14, 21, 56),
         value != 0)

ggplot(e, aes(x = value, y = location)) +
  geom_density_ridges(alpha = 0.7) +
  facet_wrap("horizon", scales = "free") +
  xlim(0, 10)
