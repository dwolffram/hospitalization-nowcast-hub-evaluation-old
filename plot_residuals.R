library(tidyverse)
library(patchwork)
Sys.setlocale("LC_ALL", "C")

df <- read_csv(paste0("data/scores_2022-04-29_2022-08-02_filled.csv.gz"))

short_names <- c(
  "Epiforecasts", "ILM", "KIT-simple_nowcast",
  "LMU", "MeanEnsemble", "MedianEnsemble",
  "RIVM", "RKI", "SU", "SZ"
)

df$model <- factor(df$model,
  levels = sort(unique(df$model)),
  labels = short_names
)

model_colors <- setNames(
  c("#999999", "#E69F00", "#56B4E9", "#F0E442", "#009E73", "#60D1B3", "#D55E00", "#3C4AAD", "#CC79A7", "#000000"),
  c("Epiforecasts", "ILM", "KIT-simple_nowcast", "LMU", "MeanEnsemble", "MedianEnsemble", "RIVM", "RKI", "SU", "SZ")
)

df <- df %>%
  filter(quantile == 0.5) %>%
  mutate(residual = truth - value)

# National
df %>%
  filter(
    age_group == "00+",
    location == "DE",
    target == "-2 day ahead inc hosp"
  ) %>%
  ggplot(aes(x = target_end_date, y = residual, color = model)) +
  geom_line() +
  geom_hline(yintercept = 0) +
  facet_wrap("location", scales = "free") +
  scale_color_manual(values = model_colors) +
  labs(
    x = "Target end date",
    y = "Residual",
    color = "Model"
  ) +
  theme_bw()

ggsave("figures/residuals_line_national.pdf", width = 300, height = 120, unit = "mm", device = "pdf")


# States
df %>%
  filter(
    age_group == "00+",
    location != "DE",
    target == "-2 day ahead inc hosp"
  ) %>%
  ggplot(aes(x = target_end_date, y = residual, color = model)) +
  geom_line() +
  geom_hline(yintercept = 0) +
  facet_wrap("location", scales = "free_y", ncol = 4) +
  scale_color_manual(values = model_colors) +
  labs(
    x = "Target end date",
    y = "Residual",
    color = "Model"
  ) +
  theme_bw() +
  theme(legend.position = "top")

ggsave("figures/residuals_line_states.pdf", width = 300, height = 300, unit = "mm", device = "pdf")


# Age groups
df %>%
  filter(
    location == "DE",
    age_group != "00+",
    target == "-2 day ahead inc hosp",
    abs(residual) <= 4000
  ) %>%
  ggplot(aes(x = target_end_date, y = residual, color = model)) +
  geom_line() +
  geom_hline(yintercept = 0) +
  facet_wrap("age_group", scales = "free_y") +
  scale_color_manual(values = model_colors) +
  labs(
    x = "Target end date",
    y = "Residual",
    color = "Model"
  ) +
  theme_bw() +
  theme(legend.position = "top")

ggsave("figures/residuals_line_age.pdf", width = 300, height = 200, unit = "mm", device = "pdf")


library(GGally)

df_corr <- df %>%
  filter(
    location == "DE",
    age_group == "00+",
    target == "-2 day ahead inc hosp"
  )

df_corr <- df_corr %>%
  pivot_wider(
    id_cols = c(location, age_group, forecast_date, target_end_date, target, type, quantile),
    names_from = model, values_from = residual
  ) %>%
  select(-c(location, age_group, forecast_date, target_end_date, target, type, quantile))


ggpairs(df_corr)

ggsave("figures/residuals_correlation_national.pdf", width = 300, height = 300, unit = "mm", device = "pdf")


library(ggridges)

df_res <- df %>%
  filter(
    location == "DE",
    age_group == "00+",
    target == "-2 day ahead inc hosp"
  )

ggplot(df_res, aes(x = residual, y = model, fill = model)) +
  facet_wrap("location") +
  geom_vline(xintercept = 0) +
  geom_density_ridges(alpha = 0.7) +
  scale_fill_manual(values = model_colors) +
  labs(
    x = "Residual",
    y = "Model",
    fill = "Model"
  ) +
  xlim(-2500, 2500) +
  theme_bw() +
  theme(legend.position = "none")

ggsave("figures/residuals_density_national.pdf", width = 300, height = 150, unit = "mm", device = "pdf")


### National
df_res <- df %>%
  filter(
    location == "DE",
    age_group == "00+",
    target == "-2 day ahead inc hosp"
  )

ggplot(df_res, aes(x = model, y = residual, fill = model)) +
  facet_wrap("location") +
  geom_hline(yintercept = 0) +
  geom_boxplot(outlier.shape = NA) +
  ylim(-2000, 2000) +
  scale_fill_manual(values = model_colors) +
  labs(
    x = "Model",
    y = "Residual",
    fill = "Model"
  ) +
  theme_bw() +
  theme(legend.position = "none")

ggsave("figures/residuals_boxplot_national.pdf", width = 300, height = 200, unit = "mm", device = "pdf")


# States
df_res <- df %>%
  filter(
    age_group == "00+",
    location != "DE",
    target == "-2 day ahead inc hosp"
  )

ggplot(df_res, aes(x = model, y = residual, fill = model)) +
  facet_wrap("location", scales = "free_y", ncol = 4) +
  geom_hline(yintercept = 0) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(values = model_colors) +
  labs(
    x = "Model",
    y = "Residual",
    fill = "Model"
  ) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)
        # axis.text.x = element_text(angle = 15, hjust = 0.5, vjust = 0.8)
        )

ggsave("figures/residuals_boxplot_states.pdf", width = 300, height = 250, unit = "mm", device = "pdf")


# Age groups
df_res <- df %>%
  filter(
    location == "DE",
    age_group != "00+",
    target == "-2 day ahead inc hosp"
  )

ggplot(df_res, aes(x = model, y = residual, fill = model)) +
  facet_wrap("age_group", scales = "free_y") +
  geom_hline(yintercept = 0) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(values = model_colors) +
  labs(
    x = "Model",
    y = "Residual",
    fill = "Model"
  ) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)
        # axis.text.x = element_text(angle = 15, hjust = 0.5, vjust = 0.8)
  )

ggsave("figures/residuals_boxplot_age.pdf", width = 300, height = 150, unit = "mm", device = "pdf")

