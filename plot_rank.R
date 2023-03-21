source("utils.R")
library(ggridges)
library(patchwork)

df <- load_scores(aggregate_scores = FALSE, load_baseline = FALSE, 
                  short_horizons = FALSE)

temp <- filter_data(df, level = "national")
temp <- filter_data(df, level = "states")
temp <- filter_data(df, level = "age")


ggplot(temp, aes(x = model, y = score)) +
  geom_boxplot()


plot_ranks <- function(df, level = "national"){
  temp <- filter_data(df, level = level)
  
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
  
  df_mean_rank <- df_rank %>%
    group_by(model) %>%
    summarize(rank = mean(rank))
  
  COLORS <- setNames(c("#440154", "#31688e", "#35b779", "#fde725"), 1:4)
  
  ggplot(df_rank, aes(x = rank, y = reorder(model, meanRank), 
                      fill = factor(stat(quantile)))) +
    stat_density_ridges(
      geom = "density_ridges_gradient", calc_ecdf = TRUE,
      quantiles = 4, quantile_lines = TRUE,
      bandwidth = 0.075,
      scale = 0.9, 
      alpha = 0.8
    ) +
    scale_fill_manual(name = "Quartile", values = alpha(COLORS, 0.6)) +
    # scale_fill_viridis_d(name = "Quartiles", breaks = 1:4, alpha = 0.6) +
    geom_point(data = df_mean_rank, aes(x = rank, y = model), inherit.aes = FALSE, shape = 3) +
    xlab("Standardized rank") +
    ylab("Model") +
    xlim(c(0, 1)) +
    theme_bw()
}

p1 <- plot_ranks(df, "national")
p2 <- plot_ranks(df, "states")
p3 <- plot_ranks(df, "age")

p1 + ylab(NULL) + theme(legend.position = "None") + facet_grid( ~ "National level") +
  p2 + ylab(NULL) + theme(legend.position = "bottom") + facet_grid( ~ "States") +
  p3 + ylab(NULL) + theme(legend.position = "None") + facet_grid( ~ "Age groups")

# (p1 + ylab(NULL) + theme(legend.position = "None") + facet_grid( ~ "National level")) /
#   (p2 + ylab(NULL) + theme(legend.position = "None") + facet_grid( ~ "States")) /
#   p3 + ylab(NULL) + facet_grid( ~ "Age groups")

ggsave("figures/ranks.pdf", width = 300, height = 150, unit = "mm", device = "pdf")






# df_rank %>%
#   group_by(model) %>%
#   summarize(q = mean(rank >= 1 / 2))









# df_rank$is_ensemble <- (df_rank$model != "Baseline")

# ggplot(df_rank, aes(x = reorder(model, -meanRank), y = rank)) +
#   geom_boxplot(alpha = 0.3, outlier.shape = NA) +
#   stat_summary(fun = mean, geom = "point", shape = 3, size = 2) +
#   coord_flip() +
#   xlab("Model") +
#   ylab("Rank") +
#   scale_x_discrete("Model", labels = parse(text = levels(reorder(df_rank$model, -df_rank$meanRank))))

# ggplot(df_rank, aes(x=rank, y=reorder(model, -meanRank))) +
#   geom_density_ridges() +
#   xlab("Model") +
#   ylab("Rank")

  
