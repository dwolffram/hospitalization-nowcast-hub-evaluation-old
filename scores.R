library(tidyverse)
library(patchwork)
source("R/load_data.R")

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#F0E442", "#009E73", "#0072B2", "#D55E00", "#CC79A7", "#000000")

df_truth <- load_truth(location = "DE", age_group = "00+", as_of = "2022-05-01") %>%
  select(-value) %>%
  rename(truth = value_7d)

HUB_PATH <- "../hospitalization-nowcast-hub/data-processed/"

models <- list.dirs(HUB_PATH, full.names = FALSE, recursive = FALSE)
models

files <- list.files(HUB_PATH, pattern = "*.csv", recursive = TRUE)

START_DATE <- "2021-11-22"
END_DATE <- "2022-03-01"

df_files <- data.frame(path = files) %>%
  mutate(model = str_split(path, "/", simplify = TRUE)[, 1]) %>%
  mutate(forecast_date = str_extract(path, "\\d{4}-\\d{2}-\\d{2}")) %>%
  filter(
    forecast_date >= START_DATE,
    forecast_date <= END_DATE
  )

# load all submissions into one dataframe in long format
df <- data.frame()
pb <- txtProgressBar(min = 0, max = nrow(df_files), style = 3)
for (i in 1:nrow(df_files)) {
  row <- df_files[i, ]
  df_temp <- read_csv(paste0(HUB_PATH, row$path),
    show_col_types = FALSE, progress = FALSE
  )
  df_temp$model <- row$model
  df <- bind_rows(df, df_temp)

  setTxtProgressBar(pb, i)
}

write_csv(df, "data/submissions.csv.gz")

df <- read_csv("data/submissions.csv.gz")

df <- df %>%
  filter(
    location == "DE",
    age_group == "00+",
    type == "quantile",
    model != "RKI-weekly_report"
  ) %>%
  select(-pathogen)

short_names <- c("Epiforecasts", "ILM", "KIT-Baseline", 
                 "LMU", "MeanEnsemble", "MedianEnsemble", 
                 "RIVM", "SU", "SZ")

df$model <- factor(df$model, levels = unique(df$model), 
                   labels = short_names)

df <- df %>%
  left_join(df_truth, by = c("location", "age_group", "target_end_date" = "date"))

# simple implementation of quantile score
qs <- function(q, y, alpha) {
  (as.numeric(y < q) - alpha) * (q - y)
}

df <- df %>%
  rowwise() %>%
  mutate(qs = qs(value, truth, quantile))

mean_scores <- df %>%
  group_by(model) %>%
  summarize(mean_qs = mean(qs))

scores_horizon <- df %>%
  group_by(model, target) %>%
  summarize(mean_qs = mean(qs), .groups = "drop") %>%
  mutate(horizon = as.numeric(str_extract(target, "-?\\d+")))


p1 <- ggplot(scores_horizon, aes(x = horizon, y = mean_qs, color = model)) +
  geom_line() +
  scale_color_manual(values = cbPalette) +
  labs(
    x = "Horizon (days)",
    y = "Mean quantile score",
    color = "Model"
  ) +
  theme(legend.position = "none")

p2 <- ggplot(mean_scores, aes(x = model, y = mean_qs, fill = model)) +
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


(p2 + p1) / plot_spacer() / p3 & theme(text = element_text(size = 24),
                legend.title = element_text(size = 18),
                axis.text = element_text(size = 18)) 

# ggsave("figures/scores_1.pdf", width = 300, height = 120, unit = "mm", device = "pdf")


(p2 | p1) / 
  plot_spacer() / 
  p3 +
  plot_layout(heights = c(6, 0.5 , 6)) & 
  theme(text = element_text(size = 24),
  legend.title = element_text(size = 18),
  axis.text = element_text(size = 18))  

ggsave("figures/evaluation.pdf", width = 300, height = 250, unit = "mm", device = "pdf")
