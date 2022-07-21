library(tidyverse)
library(patchwork)

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#F0E442", "#009E73", "#0072B2", "#D55E00", "#CC79A7", "#000000")

END_DATE <- "2022-04-01"
EVAL_DATE <- "2022-06-10"

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

df <- df %>%
  filter(
    location == "DE",
    # age_group == "00+",
  ) %>%
  select(-c(pathogen, score)) %>% 
  mutate(horizon = (str_extract(target, "-?\\d+")))


df_wide <- df %>% 
  pivot_wider(names_from = quantile, values_from = value, names_prefix = "quantile_")

df_wide <- df_wide %>% 
  mutate(c50 = (truth >= quantile_0.25  & truth <= quantile_0.75),
         c95 = (truth >= quantile_0.025 & truth <= quantile_0.975)) 

coverage_df <- df_wide %>% 
  group_by(model, horizon) %>% 
  summarize(c50 = mean(c50, na.rm = TRUE),
            c95 = mean(c95, na.rm = TRUE))

coverage_df <- coverage_df %>% 
  filter(horizon %in% c(0, -7, -14)) %>% 
  mutate(horizon = paste(horizon, "days")) 

alphas <- setNames(c(0.7, 0.4), c("50%", "95%"))

p3 <- ggplot(coverage_df, aes(x = model)) +
  facet_wrap("horizon") +
  #geom_col(aes(y = c95, fill = model, alpha = "95%")) +
  geom_col(aes(y = c50, fill = model, alpha = "50%")) +
  geom_hline(yintercept = c(0.5, 0.95), linetype = "dashed") +
  scale_y_continuous(labels = function(y) ifelse(y == 0, "0", y)) +
  labs(x = NULL,
       y = "Empirical coverage",
       color = "Model",
       alpha = "Prediction \ninterval") +
  scale_fill_manual(values = model_colors) +
  coord_flip() +
  theme(text = element_text(size = 24),
        # legend.position = "none"
  ) +
  scale_x_discrete(limits = rev(unique(coverage_df$model))) +
  guides(fill = "none") +
  scale_alpha_manual(values = alphas) +
  theme(legend.position = "right",
        legend.title = element_text(size = 18),
        axis.text = element_text(size = 18))

p3

ggsave("figures/coverage_age.pdf", width = 300, height = 120, unit = "mm", device = "pdf")



ggplot(coverage_df, aes(x = model)) +
  facet_wrap("horizon") +
  expand_limits(y = 1) +
  geom_col(aes(y = c95, fill = model, alpha = "95%")) +
  geom_col(aes(y = c50, fill = model, alpha = "50%")) +
  geom_hline(yintercept = c(0.5, 0.95), linetype = "dashed") +
  scale_y_continuous(labels = function(y) ifelse(y == 0, "0", y)) +
  labs(x = NULL,
       y = "Empirical coverage",
       color = "Model",
       alpha = "Prediction \ninterval") +
  scale_fill_manual(values = model_colors) +
  coord_flip() +
  theme(text = element_text(size = 24),
        # legend.position = "none"
  ) +
  scale_x_discrete(limits = rev(unique(coverage_df$model))) +
  guides(fill = "none") +
  scale_alpha_manual(values = alphas) +
  theme(legend.position = "right",
        legend.title = element_text(size = 18),
        axis.text = element_text(size = 18))


ggsave("figures/coverage_age2.pdf", width = 300, height = 120, unit = "mm", device = "pdf")
