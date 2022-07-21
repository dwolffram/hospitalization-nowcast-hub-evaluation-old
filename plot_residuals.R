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

df <- df %>% 
  filter(quantile == 0.5)

df <- df %>% 
  mutate(residual = truth - value)

df %>% 
  filter(location == "DE",
         age_group == "00+",
         target == "-2 day ahead inc hosp",
         abs(residual) <= 4000) %>% 
  ggplot(aes(x = target_end_date, y = residual, color = model)) +
  geom_line() +
  geom_hline(yintercept = 0) +
  facet_wrap("age_group", scales = "free")

library(GGally)

df_corr <- df %>% 
  filter(location == "DE",
         age_group == "00+",
         target == "-2 day ahead inc hosp") 

ggplot(df_corr, aes(x = target_end_date, y = residual, color = model)) +
  geom_line() +
  geom_hline(yintercept = 0) +
  facet_wrap("age_group", scales = "free")


df_corr <- df_corr %>%
  pivot_wider(id_cols = c(location, age_group, forecast_date, target_end_date, target, type, quantile),
              names_from = model, values_from = residual) %>% 
  select(-c(location, age_group, forecast_date, target_end_date, target, type, quantile))
  
  
ggpairs(df_corr)

library(ggridges)

ggplot(df_corr, aes(x = residual, y = model, fill = model)) +
  geom_vline(xintercept = 0) +
  geom_density_ridges(alpha = 0.7)


ggplot(df_corr, aes(x = model, y = residual, fill = model)) +
  geom_hline(yintercept = 0) +
  geom_boxplot(outlier.shape = NA) +
  ylim(-2000, 2000)
