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


p1 <- ggplot() +
  geom_bar(data = scores, aes(x = model, y = value, fill = model, alpha = penalty), color = "black", stat = "identity") +
  geom_label(data = df, aes(x = model, y = score, label = sprintf("%0.2f", round(score, digits = 2))),
             fill = "white", alpha = 0.7, vjust = -0.1,
             label.size = NA, label.r = unit(0, "pt"), size = 9 * 5 / 14) +
  scale_fill_manual(values = MODEL_COLORS, guide = "none") +
  scale_color_manual(values = MODEL_COLORS) +
  scale_alpha_manual(values = c(0.6, 0.2, 1)) +
  labs(
    y = "WIS",
    x = NULL,
    color = "Model",
    alpha = "WIS component"
  ) + 
  #coord_flip() +
  theme_bw() +
  theme(legend.position = "bottom")


plot_wis <- function(level = "national", add_ae = TRUE){
  df <- read_csv(paste0("data/wis_", level, ".csv.gz"))
  
  df <- df %>% 
    mutate(model = fct_relevel(model, rev(c(
      "Epiforecasts", "ILM", "KIT-frozen_baseline", "KIT",
      "LMU", "RIVM", "RKI", "SU", "SZ", "MeanEnsemble", "MedianEnsemble"
    ))))
  
  base_score <- df %>%
    filter(model == "KIT-frozen_baseline") %>%
    pull(score)
  
  df <- df %>%
    filter(model != "KIT-frozen_baseline",
           model != "RKI")
  
  scores <- df %>% 
    select(-score) %>% 
    pivot_longer(cols = c(underprediction, spread, overprediction), names_to = "penalty")
  
  df_ae <- load_scores(aggregate_scores = TRUE)
  df_ae <- df_ae %>% 
    mutate(model = fct_relevel(model, rev(c(
      "Epiforecasts", "ILM", "KIT-frozen_baseline", "KIT",
      "LMU", "RIVM", "RKI", "SU", "SZ", "MeanEnsemble", "MedianEnsemble"
    ))))
  df_ae <- filter_scores(df_ae, "median", level, by_horizon = FALSE) %>%
    filter(model != "KIT-frozen_baseline")
  
  
  ggplot() +
    {if (add_ae) geom_point(data = df_ae, aes(x = model, y = score, fill = model), shape = 23)} +
    geom_bar(data = df, aes(x = model, y = score), fill = "white", stat = "identity") + # so you can't see through bars
    geom_bar(data = scores, aes(x = model, y = value, fill = model, color = model, alpha = penalty), stat = "identity") +
    # geom_label(data = df, aes(x = model, y = 0.1*base_score, label = sprintf("%0.1f", round(score, digits = 1))),
    #            fill = "white", alpha = 0.7, hjust = 1,
    #            label.size = NA, label.r = unit(0, "pt"), size = 9 * 5 / 14,
    #            label.padding = unit(0.1, "lines"),) +
    geom_label(data = df, aes(x = model, y = score, label = sprintf("%0.1f", round(score, digits = 1))),
               fill = "white", alpha = 1, hjust = 1.3,
               label.r = unit(0.25, "lines"), size = 9 * 5 / 14,
               label.padding = unit(0.15, "lines")) +
    scale_fill_manual(values = MODEL_COLORS, guide = "none") +
    scale_color_manual(values = MODEL_COLORS, guide = "none") +
    scale_alpha_manual(values = c(0.5, 0.2, 1), labels = c("Overprediction", "Spread", "Underprediction"),
                       guide = guide_legend(reverse = TRUE)) +
    scale_y_continuous(name = "Mean WIS / AE", 
                       sec.axis = sec_axis(trans = ~ . / base_score, 
                                           name = paste("Relative", "WIS"))) +
    labs(
      y = "Mean WIS/AE",
      x = NULL,
      color = "Model",
      alpha = NULL
    ) + 
    coord_flip() +
    theme_bw() +
    theme(legend.position = "bottom",
          legend.key.height= unit(0.3, 'cm'),
          legend.key.width= unit(0.3, 'cm'),
          legend.text = element_text(size = 7.5))
}

plot_wis("national")

p1 <- plot_wis("national")
p3 <- plot_wis("states")
p5 <- plot_wis("age")

wrap_elements(p1 + p2 + p2b + plot_annotation(title = "National level") & theme(plot.title = element_text(hjust = 0.5), aspect.ratio = 1)) /
  wrap_elements(p3 + p4 + p4b + plot_annotation(title = "Average across states") & theme(plot.title = element_text(hjust = 0.5), aspect.ratio = 1)) /
  wrap_elements(p5 + p6 + p6b + plot_annotation(title = "Average across age groups") & theme(plot.title = element_text(hjust = 0.5), aspect.ratio = 1))

ggsave("figures/scores_wis.pdf", width = 300, height = 350, unit = "mm", device = "pdf")
