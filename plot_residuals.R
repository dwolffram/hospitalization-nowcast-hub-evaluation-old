library(tidyverse)
library(patchwork)
source("utils.R")
Sys.setlocale("LC_ALL", "C")

df <- load_scores(aggregate_scores = FALSE, shorten_names = TRUE)

QUANTILE = 0.5
HORIZON = -2

df <- df %>%
  filter(quantile == QUANTILE) %>%
  mutate(residual = truth - value)

df_national <- df %>%
  filter(
    age_group == "00+",
    location == "DE",
    target == paste(HORIZON, "day ahead inc hosp")
  )

df_states <- df %>%
  filter(
    age_group == "00+",
    location != "DE",
    target == paste(HORIZON, "day ahead inc hosp")
  )

df_age <- df %>%
  filter(
    location == "DE",
    age_group != "00+",
    target == paste(HORIZON, "day ahead inc hosp")
  )


### LINE PLOTS

# some customizations used in all plots
theme_residuals <- list(
  scale_color_manual(values = MODEL_COLORS),
  scale_x_date(date_breaks = "2 months", minor_breaks = "1 month", date_labels = "%b %Y"),
  labs(
    x = "Target end date",
    y = "Residual",
    color = "Model"
  ),
  theme_bw()
)

# National
ggplot(df_national, aes(x = target_end_date, y = residual, color = model)) +
  geom_line() +
  geom_hline(yintercept = 0) +
  facet_wrap("location", scales = "free") +
  theme_residuals

ggsave("figures/residuals_line_national.pdf", width = 300, height = 120, unit = "mm", device = "pdf")

# States
ggplot(df_states, aes(x = target_end_date, y = residual, color = model)) +
  geom_line() +
  geom_hline(yintercept = 0) +
  facet_wrap("location", scales = "free_y", ncol = 4) +
  theme_residuals +
  theme(legend.position = "top")

ggsave("figures/residuals_line_states.pdf", width = 300, height = 300, unit = "mm", device = "pdf")

# Age groups
ggplot(df_age, aes(x = target_end_date, y = residual, color = model)) +
  geom_line() +
  geom_hline(yintercept = 0) +
  facet_wrap("age_group", scales = "free_y") +
  theme_residuals +
  theme(legend.position = "top")

ggsave("figures/residuals_line_age.pdf", width = 300, height = 200, unit = "mm", device = "pdf")


### BOX PLOTS

# some customizations used in all plots
theme_residuals_box <- list(
  scale_fill_manual(values = MODEL_COLORS),
  labs(
    x = "Model",
    y = "Residual",
    fill = "Model"
  ),
  theme_bw(),
  theme(legend.position = "none")
)

# National
ggplot(df_national, aes(x = model, y = residual, fill = model)) +
  facet_wrap("location") +
  geom_hline(yintercept = 0) +
  geom_boxplot(outlier.shape = NA) +
  ylim(-2000, 2000) +
  theme_residuals_box

ggsave("figures/residuals_boxplot_national.pdf", width = 300, height = 200, unit = "mm", device = "pdf")

# States
ggplot(df_states, aes(x = model, y = residual, fill = model)) +
  facet_wrap("location", scales = "free_y", ncol = 4) +
  geom_hline(yintercept = 0) +
  geom_boxplot(outlier.shape = NA) +
  theme_residuals_box +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)
  )

ggsave("figures/residuals_boxplot_states.pdf", width = 300, height = 250, unit = "mm", device = "pdf")

# Age groups
ggplot(df_age, aes(x = model, y = residual, fill = model)) +
  facet_wrap("age_group", scales = "free_y") +
  geom_hline(yintercept = 0) +
  geom_boxplot(outlier.shape = NA) +
  theme_residuals_box +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)
  )

ggsave("figures/residuals_boxplot_age.pdf", width = 300, height = 150, unit = "mm", device = "pdf")


### CORRELATION PLOT

library(GGally)

df_corr <- df_national %>%
  pivot_wider(
    id_cols = c(location, age_group, forecast_date, target_end_date, target, type, quantile),
    names_from = model, values_from = residual
  ) %>%
  select(-c(location, age_group, forecast_date, target_end_date, target, type, quantile))

ggpairs(df_corr)

ggsave("figures/residuals_correlation_national.pdf", width = 300, height = 300, unit = "mm", device = "pdf")


### DENSITY PLOT

library(ggridges)

ggplot(df_national, aes(x = residual, y = model, fill = model)) +
  facet_wrap("location") +
  geom_vline(xintercept = 0) +
  geom_density_ridges(alpha = 0.7) +
  scale_fill_manual(values = MODEL_COLORS) +
  labs(
    x = "Residual",
    y = "Model",
    fill = "Model"
  ) +
  xlim(-2500, 2500) +
  theme_bw() +
  theme(legend.position = "none")

ggsave("figures/residuals_density_national.pdf", width = 300, height = 150, unit = "mm", device = "pdf")
