library(tidyverse)
library(patchwork)

df <- read_csv(paste0("data/scores_2022-08-02.csv.gz"))

model_colors <- setNames(
  c("#999999", "#E69F00", "#56B4E9", "#F0E442", "#009E73", "#60D1B3", "#D55E00", "#3C4AAD", "#CC79A7", "#000000"),
  c("Epiforecasts", "ILM", "KIT-simple_nowcast", "LMU", "MeanEnsemble", "MedianEnsemble", "RIVM", "RKI", "SU", "SZ")
)


filter_scores <- function(df, type = "quantile", level = "national", by_horizon = FALSE, average = TRUE) {
  df <- df %>%
    filter(type == !!type)

  if (level == "national") {
    df <- df %>%
      filter(
        location == "DE",
        age_group == "00+"
      )
  } else if (level == "states") {
    df <- df %>%
      filter(
        location != "DE",
        model != "ILM"
      )
  } else if (level == "age") {
    df <- df %>%
      filter(
        location == "DE",
        age_group != "00+",
        model != "RKI"
      )
  }


  if (by_horizon) {
    df <- df %>%
      mutate(horizon = as.numeric(str_extract(target, "-?\\d+"))) %>%
      arrange(model, location, age_group, horizon)

    if (average) {
      df <- df %>%
        group_by(model, target, horizon) %>%
        summarize(score = mean(score), .groups = "drop") %>%
        arrange(model, horizon)
    }
  } else {
    if (average) {
      df <- df %>%
        group_by(model) %>%
        summarize(score = mean(score), .groups = "drop")
    } else {
      df <- df %>%
        group_by(model, location, age_group) %>%
        summarize(score = mean(score), .groups = "drop")
    }
  }

  return(df)
}

scores <- filter_scores(df, "quantile", "national")
scores <- filter_scores(df, "quantile", "national", FALSE, TRUE)

scores <- filter_scores(df, "quantile", "states")
scores <- filter_scores(df, "quantile", "states", TRUE, TRUE)

scores <- filter_scores(df, "quantile", "age")
scores <- filter_scores(df, "quantile", "age", by_horizon = TRUE, average = TRUE)


plot_scores <- function(df, type = "quantile", level = "national", by_horizon = FALSE) {
  scores <- filter_scores(df, type, level, by_horizon)
  ylabel <- if (type == "quantile") "Mean quantile score" else "Mean absolute error"

  if (by_horizon) {
    ggplot(scores, aes(x = horizon, y = score, color = model)) +
      geom_point() +
      geom_line(size = 1.25) +
      scale_color_manual(values = model_colors) +
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
      scale_fill_manual(values = model_colors) +
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
