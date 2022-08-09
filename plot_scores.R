library(tidyverse)
library(patchwork)
source("utils.R")

plot_scores <- function(df, type = "quantile", level = "national", by_horizon = FALSE) {
  scores <- filter_scores(df, type, level, by_horizon)
  ylabel <- if (type == "quantile") "Mean quantile score" else "Mean absolute error"

  if (by_horizon) {
    ggplot(scores, aes(x = horizon, y = score, color = model)) +
      geom_point() +
      geom_line(size = 1.25) +
      scale_color_manual(values = MODEL_COLORS) +
      labs(
        x = "Horizon (days)",
        y = ylabel,
        color = "Model"
      ) +
      theme_bw() +
      theme(legend.position = "none")
  } else {
    ggplot(scores, aes(x = model, y = score, fill = model)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = round(score, digits = 2)), hjust = -0.25) +
      scale_fill_manual(values = MODEL_COLORS) +
      labs(
        y = ylabel,
        x = NULL,
        color = "Model"
      ) +
      coord_flip() +
      expand_limits(y = 1.1 * max(scores$score)) +
      scale_x_discrete(limits = rev(unique(scores$model))) +
      theme_bw() +
      theme(legend.position = "none")
  }
}

df <- load_scores(aggregate_scores = TRUE, shorten_names = TRUE)

# Quantile scores

p1 <- plot_scores(df, "quantile", "national", by_horizon = FALSE)
p2 <- plot_scores(df, "quantile", "national", by_horizon = TRUE)
p3 <- plot_scores(df, "quantile", "states", by_horizon = FALSE)
p4 <- plot_scores(df, "quantile", "states", by_horizon = TRUE)
p5 <- plot_scores(df, "quantile", "age", by_horizon = FALSE)
p6 <- plot_scores(df, "quantile", "age", by_horizon = TRUE)

wrap_elements(p1 + p2 + plot_annotation(title = "National level") & theme(plot.title = element_text(hjust = 0.5))) /
  wrap_elements(p3 + p4 + plot_annotation(title = "Average across states") & theme(plot.title = element_text(hjust = 0.5))) /
  wrap_elements(p5 + p6 + plot_annotation(title = "Average across age groups") & theme(plot.title = element_text(hjust = 0.5)))

ggsave("figures/quantile_scores.pdf", width = 300, height = 350, unit = "mm", device = "pdf")


# Absolute errors

p1 <- plot_scores(df, "mean", "national", by_horizon = FALSE)
p2 <- plot_scores(df, "mean", "national", by_horizon = TRUE)
p3 <- plot_scores(df, "mean", "states", by_horizon = FALSE)
p4 <- plot_scores(df, "mean", "states", by_horizon = TRUE)
p5 <- plot_scores(df, "mean", "age", by_horizon = FALSE)
p6 <- plot_scores(df, "mean", "age", by_horizon = TRUE)

wrap_elements(p1 + p2 + plot_annotation(title = "National level") & theme(plot.title = element_text(hjust = 0.5))) /
  wrap_elements(p3 + p4 + plot_annotation(title = "Average across states") & theme(plot.title = element_text(hjust = 0.5))) /
  wrap_elements(p5 + p6 + plot_annotation(title = "Average across age groups") & theme(plot.title = element_text(hjust = 0.5)))

ggsave("figures/absolute_errors.pdf", width = 300, height = 350, unit = "mm", device = "pdf")
