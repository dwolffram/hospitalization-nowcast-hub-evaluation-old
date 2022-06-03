library(tidyverse)
source("R/load_data.R")

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#F0E442", "#009E73", "#0072B2", "#D55E00", "#CC79A7", "#000000")

df <- read_csv("data/submissions.csv.gz")

df <- df %>%
  filter(
    location == "DE",
    age_group == "00+",
    type == "quantile",
    model != "RKI-weekly_report"
  ) %>%
  select(-pathogen) %>% 
  mutate(horizon = (str_extract(target, "-?\\d+")))

short_names <- c("Epiforecasts", "ILM", "KIT-Baseline", 
                 "LMU", "MeanEnsemble", "MedianEnsemble", 
                 "RIVM", "SU", "SZ")

df$model <- factor(df$model, levels = unique(df$model), 
                   labels = short_names)

df_truth <- load_truth(location = "DE", age_group = "00+", as_of = "2022-05-01") %>%
  select(-value) %>%
  rename(truth = value_7d)

df <- df %>%
  left_join(df_truth, by = c("location", "age_group", "target_end_date" = "date"))

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
  geom_col(aes(y = c95, fill = model, alpha = "95%")) +
  geom_col(aes(y = c50, fill = model, alpha = "50%")) +
  geom_hline(yintercept = c(0.5, 0.95), linetype = "dashed") +
  scale_y_continuous(labels = function(y) ifelse(y == 0, "0", y)) +
  labs(x = NULL,
       y = "Empirical coverage",
       color = "Model",
       alpha = "Prediction \ninterval") +
  scale_fill_manual(values = cbPalette) +
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

ggsave("figures/coverage_1.pdf", width = 300, height = 120, unit = "mm", device = "pdf")





#########################################################




coverage_df <- coverage_df %>% 
  pivot_longer(cols = c(c50, c95), names_to = "level") %>% 
  mutate(level = paste0(str_sub(level, 2), "%")) 

alphas <- setNames(c(0.5, 0.9), c("50%", "95%"))


ggplot(coverage_df, aes(x = model)) +
  facet_wrap("horizon") +
  geom_col(aes(y = value, fill = model, alpha = level)) +
  geom_hline(yintercept = c(0.5, 0.95), linetype = "dashed") +
  scale_y_continuous(labels = function(y) ifelse(y == 0, "0", y)) +
  labs(x = NULL,
       y = "Empirical coverage (50% and 95% interval)",
       color = "Model",
       alpha = "Coverage level") +
  scale_fill_manual(values = cbPalette) +
  coord_flip() +
  theme(text = element_text(size = 24),
        # legend.position = "none"
  ) +
  scale_x_discrete(limits = rev(unique(coverage_df$model))) +
  guides(fill = "none") +
  scale_alpha_manual(values = alphas)

ggsave("figures/coverage4.pdf", width = 300, height = 120, unit = "mm", device = "pdf")





alphas <- setNames(c(1, 0.5), c("50%", "95%"))


ggplot(coverage_df, aes(x = model)) +
  facet_wrap("horizon") +
  geom_col(aes(y = value, fill = model, alpha = level)) +
  geom_hline(yintercept = c(0.5, 0.95), linetype = "dashed") +
  scale_y_continuous(labels = function(y) ifelse(y == 0, "0", y)) +
  labs(x = NULL,
       y = "Empirical coverage",
       color = "Model",
       alpha = "Prediction interval") +
  scale_fill_manual(values = cbPalette) +
  coord_flip() +
  theme(text = element_text(size = 24)) +
  scale_x_discrete(limits = rev(unique(coverage_df$model))) +
  guides(fill = "none") +
  scale_alpha_manual(values = alphas)












coverage_df <- df %>% 
  group_by(model, quantile) %>% 
  summarize(c = mean(truth <= value))

ggplot(coverage_df, aes(x = quantile, y = c)) +
  facet_wrap("model") +
  geom_line()

coverage_df <- df %>% 
  mutate(horizon = (str_extract(target, "-\\d+")))%>%
  group_by(model, horizon, quantile) %>% 
  summarize(c = mean(truth <= value))

ggplot(coverage_df, aes(x = quantile, y = c, color = horizon)) +
  facet_wrap("model") +
  geom_line()

df_wide <- df %>% 
  pivot_wider(names_from = quantile, values_from = value, names_prefix = "quantile_")

df_wide <- df_wide %>% 
  mutate(c50 = (truth >= quantile_0.25  & truth <= quantile_0.75),
         c95 = (truth >= quantile_0.025 & truth <= quantile_0.975)) 

coverage_df <- df_wide %>% 
  group_by(model, horizon) %>% 
  summarize(c50 = mean(c50, na.rm = TRUE),
            c95 = mean(c95, na.rm = TRUE))

ggplot(coverage_df, aes(x = as.numeric(horizon), y = c50, color = model)) +
  geom_hline(yintercept = 0.5) +
  geom_line() +
  labs(x = "Horizon",
       y = "Empirical coverage (50% interval)",
       color = "Model")


coverage_df %>% 
  filter(horizon %in% c(0, -7, -14)) %>% 
  ggplot(aes(x = as.factor(as.numeric(horizon)), y = c95, fill = model)) +
  geom_hline(yintercept = c(0.5, 0.95)) +
  geom_col(position = "dodge", alpha = 0.5) +
  geom_col(aes(y = c50, fill = model), alpha = 0.9, position = "dodge") +
  labs(x = "Horizon",
       y = "Empirical coverage (50% and 95% interval)",
       color = "Model") +
  scale_fill_manual(values = cbPalette)


dput(unique(coverage_df$model))

short_names <- c("Epiforecasts", "ILM", "KIT-Baseline", 
                 "LMU", "MeanEnsemble", "MedianEnsemble", 
                 "RIVM", "SU", "SZ")

df$model <- factor(df$model, levels = unique(df$model), 
                   labels = short_names)



coverage_df %>% 
  filter(horizon %in% c(0, -7, -14)) %>% 
  ggplot(aes(x = model, y = c95, fill = model)) +
  facet_wrap("horizon") +
    geom_col(position = "dodge", alpha = 0.5) +
  geom_col(aes(y = c50, fill = model), alpha = 0.9, position = "dodge") +
  geom_hline(yintercept = c(0.5, 0.95), linetype = "dashed") +
  scale_y_continuous(labels = function(y) ifelse(y == 0, "0", y)) +
  labs(x = NULL,
       y = "Empirical coverage (50% and 95% interval)",
       color = "Model") +
  scale_fill_manual(values = cbPalette) +
  coord_flip() +
  theme(text = element_text(size = 24),
        # axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        legend.position = "none") +
  scale_x_discrete(limits = rev(unique(coverage_df$model)))

ggsave("figures/coverage3.pdf", width = 300, height = 120, unit = "mm", device = "pdf")



coverage_df <- coverage_df %>% 
  pivot_longer(cols = c(c50, c95), names_to = "level") %>% 
  mutate(level = paste0(str_sub(level, 2), "%")) 

alphas <- setNames(c(1, 0.5), c("50%", "95%"))

ggplot(coverage_df, aes(x = model)) +
  facet_wrap("horizon") +
  geom_col(aes(y = value, fill = model, alpha = level, group = model), position = "dodge") +
  geom_hline(yintercept = c(0.5, 0.95), linetype = "dashed") +
  scale_y_continuous(labels = function(y) ifelse(y == 0, "0", y)) +
  labs(x = NULL,
       y = "Empirical coverage",
       color = "Model",
       alpha = "Prediction interval") +
  scale_fill_manual(values = cbPalette) +
  coord_flip() +
  theme(text = element_text(size = 24)) +
  scale_x_discrete(limits = rev(unique(coverage_df$model))) +
  guides(fill = "none") +
  scale_alpha_manual(values = alphas)



coverage_df %>% 
  ggplot(aes(x = model)) +
  facet_wrap("horizon") +
  geom_col(aes(y = c95, fill = model, alpha = level), position = "dodge") +
  geom_col(aes(y = c50, fill = model, alpha = level), position = "dodge") +
  geom_hline(yintercept = c(0.5, 0.95), linetype = "dashed") +
  scale_y_continuous(labels = function(y) ifelse(y == 0, "0", y)) +
  labs(x = NULL,
       y = "Empirical coverage (50% and 95% interval)",
       color = "Model",
       alpha = "Coverage level") +
  scale_fill_manual(values = cbPalette) +
  coord_flip() +
  theme(text = element_text(size = 24),
        # legend.position = "none"
        ) +
  scale_x_discrete(limits = rev(unique(coverage_df$model))) +
  guides(fill = "none") +
  scale_alpha_manual(name = "Prediction interval", values = alphas)


+
  scale_alpha_continuous(breaks = c(0.8, 0.9), labels = c("95%", "50%"))

ggsave("figures/coverage4.pdf", width = 300, height = 120, unit = "mm", device = "pdf")



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

coverage_df <- coverage_df %>% 
  pivot_longer(cols = c(c50, c95), names_to = "level") %>% 
  mutate(level = as.numeric(str_sub(level, 2))/100) %>% 
  mutate(level = ifelse(level == 0.5, 0.9, 0.8))
  
  
ggplot(coverage_df, aes(x = model)) +
  facet_wrap("horizon") +
  geom_col(aes(y = value, fill = model, group = level, alpha = level), position = "dodge") +
  geom_hline(yintercept = c(0.5, 0.95), linetype = "dashed") +
  scale_y_continuous(labels = function(y) ifelse(y == 0, "0", y)) +
  labs(x = NULL,
       y = "Empirical coverage (50% and 95% interval)",
       color = "Model",
       alpha = "Coverage level") +
  scale_fill_manual(values = cbPalette) +
  coord_flip() +
  theme(text = element_text(size = 24),
        # legend.position = "none"
  ) +
  scale_x_discrete(limits = rev(unique(coverage_df$model))) +
  guides(fill = "none") +
  scale_alpha_continuous(breaks = c(0.8, 0.9), labels = c("95%", "50%"))

ggsave("figures/coverage4.pdf", width = 300, height = 120, unit = "mm", device = "pdf")

