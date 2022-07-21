library(tidyverse)
source("load_truth.R")

Sys.setlocale("LC_ALL", "C")

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#F0E442", "#009E73", "#0072B2", "#D55E00", "#CC79A7", "#000000")

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


df_all <- df_nowcasts %>%
  right_join(df_truth1, by = c("target_end_date" = "date", "location", "age_group")) %>%
  right_join(df_truth2, by = c("target_end_date" = "date", "location", "age_group"))

df_all$age_group <- factor(df_all$age_group, levels = c("00+", "00-04", "05-14", "15-34", "35-59", "60-79", "80+"))

short_names <- c("Epiforecasts", "ILM", "KIT-Baseline", 
                 "LMU", "MeanEnsemble", "MedianEnsemble", 
                 "RIVM", "RKI", "SU", "SZ")

df_all$model <- factor(df_all$model, levels = unique(df_all$model), 
                    labels = short_names)

alphas <- setNames(c(0.7, 0.4), c("50%", "95%"))
line_colors <- setNames(c("gray", "black"), c(as_of1, as_of2))


df1 <- df_all %>%
  filter(target_end_date >= as.Date("2022-03-01"),
         target_end_date <= as.Date("2022-05-15"),
         !model %in% c("RKI", "MeanEnsemble", "MedianEnsemble", NA),
         age_group == "00+") %>% 
  mutate(model_type = "Individual models")

df2 <- df_all %>% 
  filter(target_end_date >= as.Date("2022-03-01"),
         target_end_date <= as.Date("2022-05-15"),
         model %in% c("MeanEnsemble"),
         age_group == "00+") %>% 
  mutate(model_type = "Ensemble model")

df3 <- bind_rows(df1, df2)
df3$model_type <- factor(df3$model_type, levels = c("Individual models", "Ensemble model"))


df4 <- df_all %>% 
  filter(target_end_date >= as.Date("2022-03-01"),
         target_end_date <= as.Date("2022-05-15"),
         age_group == "00+") 

ggplot(df3) +
  facet_wrap("model_type") +
  geom_vline(xintercept = as.Date(as_of1), linetype = "dashed") +
  geom_line(data = df4, aes(x = target_end_date, y = truth1, color = as_of1)) +
  # geom_blank(aes(x = target_end_date, y = truth2)) +
  geom_line(data = df4, aes(x = target_end_date, y = truth2, color = as_of2)) +
  geom_ribbon(aes(x = target_end_date, ymin = quantile_0.025, ymax = quantile_0.975, alpha = "95%", fill = model),
  ) +
  geom_ribbon(aes(x = target_end_date, ymin = quantile_0.25, ymax = quantile_0.75, alpha = "50%", fill = model),
  ) +
  labs(x = NULL, y = "7-day hospitalization incidence") +
  scale_alpha_manual(name = "Prediction interval", values = alphas,
                     guide = guide_legend(order = 2, title.position = "top", title.hjust = 0)) +
  scale_color_manual(name = "Truth as of", values = line_colors,
                     guide = guide_legend(order = 1, title.position = "top", title.hjust = 0)) +
  guides(fill=guide_legend(title="Model", ncol = 2)) +
  theme(legend.position = "bottom", legend.box.just = "left", 
        legend.direction = "vertical", legend.box = "horizontal") +
  theme(text = element_text(size = 24),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 16),
        axis.text = element_text(size = 16)) +
  scale_fill_manual(values = cbPalette)

ggsave("figures/individual_and_ensemble.pdf", width = 300, height = 200, unit = "mm", device = "pdf")
