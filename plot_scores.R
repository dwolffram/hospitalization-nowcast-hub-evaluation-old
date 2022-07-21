library(tidyverse)
library(patchwork)

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#F0E442", "#009E73", "#0072B2", "#D55E00", "#CC79A7", "#000000")

END_DATE <- "2022-04-29"
EVAL_DATE <- "2022-07-17"

df_all <- read_csv(paste0("data/scores_", END_DATE, "_", EVAL_DATE, ".csv.gz"))

short_names <- c("Epiforecasts", "ILM", "KIT-Baseline", 
                 "LMU", "MeanEnsemble", "MedianEnsemble", 
                 "RIVM", "RKI", "SU", "SZ")

df_all$model <- factor(df_all$model, levels = unique(df_all$model), 
                       labels = short_names)

# model_colors <- setNames(c("#999999", "#E69F00", "#56B4E9", "#F0E442", "#009E73", "#0072B2", "#D55E00", "#CC79A7", "#000000"),
#                          c("Epiforecasts", "ILM", "KIT-Baseline", "LMU", "MeanEnsemble", "MedianEnsemble", "RIVM", "SU", "SZ"))

model_colors <- setNames(c("#999999", "#E69F00", "#56B4E9", "#F0E442", "#009E73", "#D55E00", "#CC79A7", "#000000"),
                         c("Epiforecasts", "ILM", "KIT-Baseline", "LMU", "MeanEnsemble", "RIVM", "SU", "SZ"))

df <- df_all %>% 
  filter(type == "quantile",
         !model %in% c("RKI", "MedianEnsemble"))


mean_scores <- df %>%
  filter(location == "DE") %>% 
  group_by(model) %>%
  summarize(mean_qs = mean(score))


ggplot(mean_scores, aes(x = model, y = mean_qs, fill = model)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(mean_qs, digits = 2)), hjust = 1.25) +
  scale_fill_manual(values = model_colors) +
  labs(
    y = "Mean quantile score",
    x = NULL,
    color = "Model"
  ) +
  theme(legend.position = "none") +
  coord_flip() +
  scale_x_discrete(limits = rev(unique(mean_scores$model)))

ggplot(mean_scores, aes(x = reorder(model, -mean_qs), y = mean_qs, fill = model)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(mean_qs, digits = 2)), hjust = -0.25) +
  scale_fill_manual(values = model_colors) +
  labs(
    y = "Mean quantile score",
    x = NULL,
    color = "Model"
  ) +
  theme(legend.position = "none") +
  coord_flip() +
  labs(title = "Average across age groups (national level)")


plot_mean_scores <- function (df, reorder = FALSE){
  if (reorder){
    ggplot(mean_scores, aes(x = reorder(model, -mean_qs), y = mean_qs, fill = model)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = round(mean_qs, digits = 2)), hjust = -0.25) +
      scale_fill_manual(values = model_colors) +
      labs(
        y = "Mean quantile score",
        x = NULL,
        color = "Model"
      ) +
      theme(legend.position = "none") +
      coord_flip()  
  } else {
    ggplot(mean_scores, aes(x = model, y = mean_qs, fill = model)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = round(mean_qs, digits = 2)), hjust = -0.25, size = 16*5/14) +
      scale_fill_manual(values = model_colors) +
      labs(
        y = "Mean quantile score",
        x = NULL,
        color = "Model"
      ) +
      theme(legend.position = "none") +
      coord_flip() +
      scale_x_discrete(limits = rev(unique(mean_scores$model)))
    
    # ggplot(mean_scores, aes(x = model, y = mean_qs, fill = model)) +
    #   geom_bar(stat = "identity") +
    #   geom_text(aes(label = round(mean_qs, digits = 2)), vjust = -0.5, size = 16*5/14) +
    #   scale_fill_manual(values = model_colors) +
    #   labs(
    #     y = "Mean quantile score",
    #     x = NULL,
    #     color = "Model"
    #   ) +
    #   theme(legend.position = "none") +
    #   scale_x_discrete(limits = rev(unique(mean_scores$model))) 
  }
}



mean_scores <- df %>%
  filter(location == "DE") %>% 
  group_by(model) %>%
  summarize(mean_qs = mean(score))

p1 <- plot_mean_scores(mean_scores, reorder = FALSE) +
  # facet_grid("" ~ "Average across age groups") +
  theme(
    strip.background.y = element_blank(),
    strip.text.y = element_blank())


mean_scores <- df %>%
  filter(location == "DE",
         age_group == "00+") %>% 
  group_by(model) %>%
  summarize(mean_qs = mean(score))

plot_mean_scores(mean_scores, reorder = TRUE) +
  labs(title = "National level")



mean_scores <- df %>%
  filter(age_group == "00+") %>% 
  group_by(model) %>%
  summarize(mean_qs = mean(score))

plot_mean_scores(mean_scores, reorder = TRUE) +
  labs(title = "All locations")


mean_scores <- df %>%
  filter(age_group == "00+",
         location != "DE") %>% 
  group_by(model) %>%
  summarize(mean_qs = mean(score))

p2 <- plot_mean_scores(mean_scores, reorder = FALSE) +
  # facet_grid("" ~ "Average across states") +
  scale_x_discrete(limits = rev(c("Epiforecasts", "ILM", "KIT-Baseline", 
                              "LMU", "MeanEnsemble", "RIVM", "SU", "SZ"))) +
  theme(
    strip.background.y = element_blank(),
    strip.text.y = element_blank())


scores_horizon <- df %>%
  filter(location == "DE") %>% 
  group_by(model, target) %>%
  summarize(mean_qs = mean(score), .groups = "drop") %>%
  mutate(horizon = as.numeric(str_extract(target, "-?\\d+")))

p3 <- ggplot(scores_horizon, aes(x = horizon, y = mean_qs, color = model)) +
  # facet_grid("" ~ "Average across age groups") +
  geom_point(size = 3) +
  geom_line(size = 1.5) +
  scale_color_manual(values = model_colors, drop = TRUE) +
  labs(
    x = "Horizon (days)",
    y = "Mean quantile score",
    color = "Model"
  ) +
  theme(legend.position = "none",
        strip.background.y = element_blank(),
        strip.text.y = element_blank())


scores_horizon <- df %>%
  filter(age_group == "00+",
         location != "DE") %>% 
  group_by(model, target) %>%
  summarize(mean_qs = mean(score), .groups = "drop") %>%
  mutate(horizon = as.numeric(str_extract(target, "-?\\d+")))

p4 <- ggplot(scores_horizon, aes(x = horizon, y = mean_qs, color = model)) +
  # facet_grid("" ~ "Average across states") +
  geom_point(size = 3) +
  geom_line(size = 1.5) +
  scale_color_manual(values = model_colors, drop = TRUE) +
  labs(
    x = "Horizon (days)",
    y = "Mean quantile score",
    color = "Model"
  ) +
  theme(legend.position = "none",
        strip.background.y = element_blank(),
        strip.text.y = element_blank())


p1 + expand_limits(y = 70)

p1 + expand_limits(y = 75) + plot_spacer() +  p3 +
  plot_layout(widths = c(6, 0.5 , 6)) +
  plot_annotation(title = "Average scores across age groups") &   
  theme(text = element_text(size = 24),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 16),
        axis.text = element_text(size = 16),
        plot.title = element_text(hjust = 0.5)) 


ggsave("figures/scores_by_age.pdf", width = 300, height = 150, unit = "mm", device = "pdf")




p2 + expand_limits(y = 18.5) + plot_spacer() +  p4 +
  plot_layout(widths = c(6, 0.5 , 6)) +
  plot_annotation(title = "Average scores across states") &   
  theme(text = element_text(size = 24),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 16),
        axis.text = element_text(size = 16),
        plot.title = element_text(hjust = 0.5)) 


ggsave("figures/scores_by_states.pdf", width = 300, height = 150, unit = "mm", device = "pdf")





mean_scores <- df %>%
  filter(location == "DE") %>% 
  group_by(model, age_group) %>%
  summarize(mean_qs = mean(score))

ggplot(mean_scores, aes(x = model, y = mean_qs, fill = model)) +
  facet_wrap("age_group", scales = "free_x") +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = model_colors) +
  labs(
    y = "Mean quantile score",
    x = NULL,
    color = "Model"
  ) +
  theme(legend.position = "none") +
  coord_flip() +
  scale_x_discrete(limits = rev(unique(mean_scores$model))) +
  labs(title = "Average scores by age group")


mean_scores <- df %>%
  filter(age_group == "00+") %>% 
  group_by(model, location) %>%
  summarize(mean_qs = mean(score))

ggplot(mean_scores, aes(x = model, y = mean_qs, fill = model)) +
  facet_wrap("location", scales = "free_x") +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = cbPalette) +
  labs(
    y = "Mean quantile score",
    x = NULL,
    color = "Model"
  ) +
  theme(legend.position = "none") +
  coord_flip() +
  scale_x_discrete(limits = rev(unique(mean_scores$model))) +
  labs(title = "Average scores by state")









df <- df_all %>% 
  filter(type == "quantile",
         model != "RKI-weekly_report",
         location != "DE")

mean_scores <- df %>%
  filter(location != "DE") %>% 
  group_by(model, location) %>%
  summarize(mean_qs = mean(score, na.rm = TRUE))

ggplot(mean_scores, aes(x = model, y = mean_qs, fill = model)) +
  facet_wrap("location", scales = "free") +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = cbPalette) +
  labs(
    y = "Mean quantile score",
    x = NULL,
    color = "Model"
  ) +
  theme(legend.position = "none") +
  coord_flip() +
  scale_x_discrete(limits = rev(unique(mean_scores$model)))


scores_horizon <- df %>%
  group_by(model, target) %>%
  summarize(mean_qs = mean(score), .groups = "drop") %>%
  mutate(horizon = as.numeric(str_extract(target, "-?\\d+")))

ggplot(scores_horizon, aes(x = horizon, y = mean_qs, color = model)) +
  geom_line() +
  scale_color_manual(values = cbPalette) +
  labs(
    x = "Horizon (days)",
    y = "Mean quantile score",
    color = "Model"
  ) +
  theme(legend.position = "none")
