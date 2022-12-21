library(tidyverse)
source("utils.R")

df <- read_csv("data/wis_national.csv.gz")

base_score <- df %>%
  filter(model == "KIT-frozen_baseline") %>%
  pull(score)

df <- df %>%
  filter(model != "KIT-frozen_baseline",
         model != "RKI")

scores <- df %>% 
  select(-score) %>% 
  pivot_longer(cols = c(underprediction, spread, overprediction), names_to = "penalty")

ggplot() +
  geom_bar(data = scores, aes(x = model, y = value, fill = model, alpha = penalty), color = "black", stat = "identity") +
  geom_label(data = df, aes(x = model, y = 0.9*score, label = sprintf("%0.2f", round(score, digits = 2))),
             fill = "white", alpha = 0.7, hjust = 1,
             label.size = NA, label.r = unit(0, "pt"), size = 9 * 5 / 14) +
  scale_fill_manual(values = MODEL_COLORS, guide = "none") +
  scale_color_manual(values = MODEL_COLORS) +
  scale_alpha_manual(values = c(0.7, 0.2, 1)) +
  labs(
    y = "WIS",
    x = NULL,
    color = "Model",
    alpha = "WIS component"
  ) + 
  
  coord_flip() +
  theme_bw() +
  theme(legend.position = "right")
