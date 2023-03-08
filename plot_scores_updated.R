source("utils.R")
library(patchwork)

metrics <- setNames(
  c("absolute error", "squared error", "WIS"),
  c("median", "mean", "quantile")
)

plot_scores <- function(df, type = "quantile", level = "national", 
                        by_horizon = FALSE, relative = FALSE, add_ae = FALSE,
                        short_horizons = FALSE) {
  
  #df <- load_scores(aggregate_scores = FALSE, load_baseline = TRUE, short_horizons = short_horizons) 
  scores <- filter_scores(df, type, level, by_horizon)
  metric <- metrics[type]
  ylabel <- if (relative) paste("Relative", metric) else paste("Mean", metric)
  
  if (by_horizon) {
    if (relative) {
      base_scores <- scores %>%
        filter(model == "KIT-frozen_baseline") %>%
        select(-c(model, target)) %>%
        rename(base_score = score)
      
      scores <- scores %>%
        left_join(base_scores, by = "horizon") %>%
        mutate(score = score / base_score)
    }
    
    scores <- scores %>%
      filter(model != "KIT-frozen_baseline")
    
    ggplot(scores, aes(x = horizon, y = score, color = model)) +
      geom_line(size = 1) +
      scale_color_manual(values = MODEL_COLORS) +
      labs(
        x = "Horizon (days)",
        y = ylabel,
        color = "Model"
      ) +
      scale_x_continuous(breaks = 0:5*-5,
                         minor_breaks = -28:0) +
      expand_limits(y = 0) +
      theme_bw() +
      theme(legend.position = "none")
  } else {
    if (relative) {
      base_score <- scores %>%
        filter(model == "KIT-frozen_baseline") %>%
        pull(score)
    }
    
    scores <- scores %>%
      filter(model != "KIT-frozen_baseline") 
    
    if (add_ae) {
      scores_ae <- filter_scores(df, "median", level, by_horizon) %>%
        filter(model != "KIT-frozen_baseline")
    }
    
    ggplot() +
      {if (add_ae) geom_point(data = scores_ae, aes(x = model, y = score, fill = model), shape = 23)} +
      geom_bar(data = scores, aes(x = model, y = score, fill = model), stat = "identity") +
      geom_label(data = scores, aes(x = model, y = 0.5*score, label = sprintf("%0.1f", round(score, digits = 1))),
                 fill = "white", alpha = 1, hjust = 0.5,
                 label.r = unit(0.25, "lines"), size = 9 * 5 / 14,
                 label.padding = unit(0.15, "lines")) +
      scale_fill_manual(values = MODEL_COLORS) +
      scale_color_manual(values = MODEL_COLORS) +
      labs(
        y = ylabel,
        x = NULL,
        color = "Model"
      ) + 
      
      coord_flip() +
      theme_bw() +
      theme(legend.position = "none") +
      {
        if (relative) {
          scale_y_continuous(
            name = paste("Mean", metric),
            sec.axis = sec_axis(trans = ~ . / base_score, name = paste("Relative", metric))
          )
        }
      }
  }
}

df <- read_csv("data/scores_updated.csv.gz")

# TODO: load original scores and bind_rows
#       compute WIS

plot_scores(df, type = "quantile", level = "national", by_horizon = TRUE, short_horizons = FALSE,
            relative = FALSE)
