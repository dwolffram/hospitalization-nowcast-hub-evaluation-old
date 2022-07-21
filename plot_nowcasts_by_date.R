library(tidyverse)
source("load_truth.R")

Sys.setlocale("LC_ALL", "C")


as_of1 = "2022-04-24"
as_of2 = "2022-06-10"

df <- read_csv(paste0("data/submissions_", as_of2, ".csv.gz"))

df_nowcasts <- df %>% 
  filter(type == "quantile",
         forecast_date == "2022-04-24") %>%
  pivot_wider(names_from = quantile, names_prefix = "quantile_")

df_truth1 <- load_truth(as_of = as_of1)
df_truth2 <- load_truth(as_of = as_of2)

df_truth1 <- df_truth1 %>% 
  rename(truth1 = truth) %>% 
  filter(location == "DE")

df_truth2 <- df_truth2 %>% 
  rename(truth2 = truth) %>% 
  filter(location == "DE")


df1 <- df_nowcasts %>%
  right_join(df_truth1, by = c("target_end_date" = "date", "location", "age_group")) %>%
  right_join(df_truth2, by = c("target_end_date" = "date", "location", "age_group"))

df1$age_group <- factor(df1$age_group, levels = c("00+", "00-04", "05-14", "15-34", "35-59", "60-79", "80+"))

alphas <- setNames(c(0.7, 0.4), c("50%", "95%"))
line_colors <- setNames(c("gray", "black"), c(as_of1, as_of2))

df1 %>% 
  filter(target_end_date >= as.Date("2022-03-01"),
         model %in% c("NowcastHub-MeanEnsemble", NA)) %>% 
  ggplot() +
  facet_wrap("age_group", scales = "free_y") +
  geom_vline(xintercept = as.Date(as_of1), linetype = "dashed") +
  geom_line(aes(x = target_end_date, y = truth1, color = as_of1)) +
  geom_blank(aes(x = target_end_date, y = truth2)) +
  # geom_line(aes(x = target_end_date, y = truth2, color = as_of2)) +
  # geom_ribbon(aes(x = target_end_date, ymin = quantile_0.025, ymax = quantile_0.975, alpha = "95%"),
  #             fill = "skyblue3"
  # ) +
  # geom_ribbon(aes(x = target_end_date, ymin = quantile_0.25, ymax = quantile_0.75, alpha = "50%"),
  #             fill = "skyblue3"
  # ) +
  # geom_line(aes(x = target_end_date, y = quantile_0.5),
  #           color = "skyblue3", linetype = "solid"
  # ) +

  labs(x = NULL, y = "7-day hospitalization incidence") +
  scale_alpha_manual(name = "Nowcasts with \nprediction intervals:", values = alphas,
                     guide = guide_legend(order = 2, title.position = "top", title.hjust = 0)) +
  scale_color_manual(name = "Truth as of", values = line_colors[1],
                     guide = guide_legend(order = 1, title.position = "top", title.hjust = 0)) +
  theme(legend.position = c(0.35, 0.25), legend.justification = c(0, 1), legend.box.just = "left", 
        legend.direction = "vertical", legend.box = "horizontal") +    
  theme(text = element_text(size = 24),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 16),
        axis.text = element_text(size = 16))

ggsave("figures/example_by_age_1.pdf", width = 300, height = 200, unit = "mm", device = "pdf")



# Multiple models one location
short_names <- c("Epiforecasts", "ILM", "KIT-Baseline", 
                 "LMU", "MeanEnsemble", "MedianEnsemble", 
                 "RIVM", "RKI", "SU", "SZ")

df1$model <- factor(df1$model, levels = unique(df1$model), 
                       labels = short_names)

df1 %>% 
  filter(target_end_date >= as.Date("2022-03-01"),
         !model %in% c("MeanEnsemble", "MedianEnsemble", NA),
         age_group == "00+") %>% 
  ggplot() +
  facet_wrap("model", scales = "free_y") +
  geom_vline(xintercept = as.Date(as_of1), linetype = "dashed") +
  geom_line(aes(x = target_end_date, y = truth1, color = as_of1)) +
  # geom_blank(aes(x = target_end_date, y = truth2)) +
  geom_line(aes(x = target_end_date, y = truth2, color = as_of2)) +
  geom_ribbon(aes(x = target_end_date, ymin = quantile_0.025, ymax = quantile_0.975, alpha = "95%"),
              fill = "skyblue3"
  ) +
  geom_ribbon(aes(x = target_end_date, ymin = quantile_0.25, ymax = quantile_0.75, alpha = "50%"),
              fill = "skyblue3"
  ) +
  geom_line(aes(x = target_end_date, y = quantile_0.5),
            color = "skyblue3", linetype = "solid"
  ) +
  
  labs(x = NULL, y = "7-day hospitalization incidence") +
  scale_alpha_manual(name = "Nowcasts with \nprediction intervals:", values = alphas,
                     guide = guide_legend(order = 2, title.position = "top", title.hjust = 0)) +
  scale_color_manual(name = "Truth as of", values = line_colors,
                     guide = guide_legend(order = 1, title.position = "top", title.hjust = 0)) +
  theme(legend.position = c(0.35, 0.25), legend.justification = c(0, 1), legend.box.just = "left", 
        legend.direction = "vertical", legend.box = "horizontal") +    
  theme(text = element_text(size = 24),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 16),
        axis.text = element_text(size = 16))


# 
# df2 <- df1 %>% 
#   filter(target_end_date >= as.Date("2022-03-01"),
#          target_end_date <= as.Date("2022-05-15"),
#          !model %in% c("MeanEnsemble", "MedianEnsemble", NA),
#          age_group == "00+") 

df2 <- df1 %>% 
  filter(target_end_date >= as.Date("2022-03-01"),
         target_end_date <= as.Date("2022-05-15"),
         model %in% c("MeanEnsemble"),
         age_group == "00+") 

df3 <- df1 %>% 
  filter(target_end_date >= as.Date("2022-03-01"),
         target_end_date <= as.Date("2022-05-15"),
         age_group == "00+") 

ggplot(df2) +
  facet_wrap("location", scales = "free_y") +
  geom_vline(xintercept = as.Date(as_of1), linetype = "dashed") +
  geom_line(data = df3, aes(x = target_end_date, y = truth1, color = as_of1)) +
  # geom_blank(aes(x = target_end_date, y = truth2)) +
  geom_line(data = df3, aes(x = target_end_date, y = truth2, color = as_of2)) +
  geom_ribbon(aes(x = target_end_date, ymin = quantile_0.025, ymax = quantile_0.975, alpha = "95%", fill = model),
  ) +
  geom_ribbon(aes(x = target_end_date, ymin = quantile_0.25, ymax = quantile_0.75, alpha = "50%", fill = model),
  ) +
  labs(x = NULL, y = "7-day hospitalization incidence") +
  scale_alpha_manual(name = "Nowcasts with \nprediction intervals:", values = alphas,
                     guide = guide_legend(order = 2, title.position = "top", title.hjust = 0)) +
  scale_color_manual(name = "Truth as of", values = line_colors,
                     guide = guide_legend(order = 1, title.position = "top", title.hjust = 0)) +
  guides(fill=guide_legend(title="Model")) +
  # theme(legend.position = c(0.35, 0.25), legend.justification = c(0, 1), legend.box.just = "left", 
  #       legend.direction = "vertical", legend.box = "horizontal") +    
  theme(text = element_text(size = 24),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 16),
        axis.text = element_text(size = 16))

ggsave("figures/mean_ensemble.pdf", width = 300, height = 200, unit = "mm", device = "pdf")

