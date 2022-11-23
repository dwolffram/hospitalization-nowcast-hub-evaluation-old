library(tidyverse)
source("utils.R")
library(ggridges)

df <- load_scores(aggregate_scores = FALSE, shorten_names = TRUE)

temp <- df %>%
  filter(
    type == "quantile",
    location == "DE",
    age_group != "00+",
    model != "KIT-frozen_baseline"
  )

# temp <- df %>%
#   filter(!(location == "DE" & age_group == "00+"),
#           type == "quantile",
#          model != "KIT-frozen_baseline")
#
# temp <- df %>%
#   filter(type == "quantile",
#          location != "DE",
#          age_group == "00+",
#          model != "KIT-frozen_baseline")

df_rank <- temp %>%
  group_by(location, age_group, target, target_end_date, model) %>%
  summarize(mean_score = mean(score)) %>%
  group_by(location, age_group, target, target_end_date) %>%
  arrange(model, mean_score) %>%
  mutate(rank = rank(mean_score, ties.method = "min")) %>%
  arrange(target_end_date)

m <- length(unique(df_rank$model))

df_rank <- df_rank %>%
  group_by(model) %>%
  mutate(
    meanRank = 1 - mean(rank) / m,
    rank = 1 - rank / m
  )

# df_rank$is_ensemble <- (df_rank$model != "Baseline")

ggplot(df_rank, aes(x = reorder(model, -meanRank), y = rank)) +
  geom_boxplot(alpha = 0.3, outlier.shape = NA) +
  stat_summary(fun = mean, geom = "point", shape = 3, size = 2) +
  coord_flip() +
  xlab("Model") +
  ylab("Rank") +
  scale_x_discrete("Model", labels = parse(text = levels(reorder(df_rank$model, -df_rank$meanRank))))

# ggplot(df_rank, aes(x=rank, y=reorder(model, -meanRank))) +
#   geom_density_ridges() +
#   xlab("Model") +
#   ylab("Rank")

df_mean_rank <- df_rank %>%
  group_by(model) %>%
  summarize(rank = mean(rank))

ggplot(df_rank, aes(x = rank, y = reorder(model, meanRank), fill = factor(stat(quantile)))) +
  stat_density_ridges(
    geom = "density_ridges_gradient", calc_ecdf = TRUE,
    quantiles = 4, quantile_lines = TRUE,
    bandwidth = 0.075,
    scale = 0.9
  ) +
  scale_fill_viridis_d(name = "Quartiles", breaks = 1:4, alpha = 0.6) +
  geom_point(data = df_mean_rank, aes(x = rank, y = model), inherit.aes = FALSE, shape = 3) +
  xlab("Standardized rank") +
  ylab("Model") +
  xlim(c(0, 1)) +
  theme_bw()

ggsave("figures/ranks_age.pdf", width = 250, height = 200, unit = "mm", device = "pdf")


df_rank %>%
  group_by(model) %>%
  summarize(q = mean(rank >= 1 / 2))

#######

# temp <- temp %>%
#   mutate(horizon = as.numeric(str_extract(target, "-?\\d+"))) %>% 
#   filter(target %in% paste(0:7*-1, "day ahead inc hosp"))
  
