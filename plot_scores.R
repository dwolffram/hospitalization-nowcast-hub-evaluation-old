source("utils.R")
library(patchwork)

metrics <- setNames(
  c("absolute error", "squared error", "WIS"),
  c("median", "mean", "quantile")
)

plot_scores <- function(type = "quantile", level = "national", 
                        by_horizon = FALSE, relative = FALSE, add_ae = FALSE,
                        short_horizons = FALSE) {
  
  df <- load_scores(aggregate_scores = FALSE, load_baseline = TRUE, short_horizons = short_horizons) 
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
      geom_line() + # size = 1
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
      mutate(model = fct_relevel(model, c(
        "Epiforecasts", "ILM", "KIT-frozen_baseline", "KIT",
        "LMU", "RIVM", "RKI", "SU", "SZ", "MeanEnsemble", "MedianEnsemble"
      )))
    
    scores <- scores %>%
      filter(model != "KIT-frozen_baseline") %>% 
      mutate(model = fct_drop(model, only = "KIT-frozen_baseline"))
    
    if (add_ae) {
      scores_ae <- filter_scores(df, "median", level, by_horizon) %>%
        filter(model != "KIT-frozen_baseline") %>% 
        mutate(model = fct_drop(model, only = "KIT-frozen_baseline"))
    }
    
    ggplot() +
      {if (add_ae) geom_point(data = scores_ae, aes(x = model, y = score, fill = model), shape = 23)} +
      geom_bar(data = scores, aes(x = model, y = score, fill = model), stat = "identity") +
      geom_label(
        data = scores, aes(x = model, y = 0.5 * score, label = sprintf("%0.1f", round(score, digits = 1))),
        fill = "white", alpha = 1, hjust = 0.5,
        label.r = unit(0.25, "lines"), size = 5/.pt, # 9 * 5 / 14
        label.padding = unit(0.15, "lines")
      ) +
      scale_fill_manual(values = MODEL_COLORS) +
      scale_color_manual(values = MODEL_COLORS) +
      scale_x_discrete(limits = rev, drop = FALSE) +
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

plot_scores(type = "quantile", level = "national", by_horizon = TRUE, short_horizons = TRUE,
            relative = TRUE)


# Quantile score

p1 <- plot_scores("quantile", "national", by_horizon = FALSE, relative = TRUE, add_ae = TRUE)
p2 <- plot_scores("quantile", "national", by_horizon = TRUE)
p2b <- plot_scores("quantile", "national", by_horizon = TRUE, relative = TRUE)
p3 <- plot_scores("quantile", "states", by_horizon = FALSE, relative = TRUE, add_ae = TRUE)
p4 <- plot_scores("quantile", "states", by_horizon = TRUE)
p4b <- plot_scores("quantile", "states", by_horizon = TRUE, relative = TRUE)
p5 <- plot_scores("quantile", "age", by_horizon = FALSE, relative = TRUE, add_ae = TRUE)
p6 <- plot_scores("quantile", "age", by_horizon = TRUE)
p6b <- plot_scores("quantile", "age", by_horizon = TRUE, relative = TRUE)


wrap_elements(p1 + p2 + p2b + plot_annotation(title = "National level") & theme(plot.title = element_text(hjust = 0.5), aspect.ratio = 1)) /
  wrap_elements(p3 + p4 + p4b + plot_annotation(title = "Average across states") & theme(plot.title = element_text(hjust = 0.5), aspect.ratio = 1)) /
  wrap_elements(p5 + p6 + p6b + plot_annotation(title = "Average across age groups") & theme(plot.title = element_text(hjust = 0.5), aspect.ratio = 1))

ggsave("figures/scores_qs.pdf", width = 300, height = 350, unit = "mm", device = "pdf")


# Squared error

p1 <- plot_scores("mean", "national", by_horizon = FALSE, relative = TRUE)
p2 <- plot_scores("mean", "national", by_horizon = TRUE)
p2b <- plot_scores("mean", "national", by_horizon = TRUE, relative = TRUE)
p3 <- plot_scores("mean", "states", by_horizon = FALSE, relative = TRUE)
p4 <- plot_scores("mean", "states", by_horizon = TRUE)
p4b <- plot_scores("mean", "states", by_horizon = TRUE, relative = TRUE)
p5 <- plot_scores("mean", "age", by_horizon = FALSE, relative = TRUE)
p6 <- plot_scores("mean", "age", by_horizon = TRUE)
p6b <- plot_scores("mean", "age", by_horizon = TRUE, relative = TRUE)

# wrap_elements(p1 + p2 + p2b + plot_annotation(title = "National level") & theme(plot.title = element_text(hjust = 0.5), aspect.ratio = 1)) /
#   wrap_elements(p3 + p4 + p4b + plot_annotation(title = "Average across states") & theme(plot.title = element_text(hjust = 0.5), aspect.ratio = 1)) /
#   wrap_elements(p5 + p6 + p6b + plot_annotation(title = "Average across age groups") & theme(plot.title = element_text(hjust = 0.5), aspect.ratio = 1))
# 
# ggsave("figures/scores_mse.pdf", width = 300, height = 350, unit = "mm", device = "pdf")

t <- list(theme(
  plot.title = element_text(size = 8, hjust = 0.5, margin = margin(10, 0, -3, 0), face = "bold"),
  legend.title = element_text(size = 6), 
  legend.text  = element_text(size = 5),
  legend.key.size = unit(0.4, "lines"),
  axis.title = element_text(size = 7),
  axis.text = element_text(size = 6),
  axis.ticks = element_line(colour = "black", size = 0.25),
  panel.grid.major = element_line(size = 0.15),
  panel.grid.minor = element_line(size = 0.1),
  plot.margin = unit(c(2, 3, 2, 2), "pt"), 
  legend.margin = margin(4, 0, 0, 0),
  legend.box.spacing = unit(0, "pt"),
  legend.background = element_rect(fill='transparent')))


(p1 + p2 + labs(title = "National level") + p2b) /
  (p3 + p4 + labs(title = "Average across states") + p4b) /
  (p5 + p6 + labs(title = "Average across age groups") + p6b) + plot_annotation(theme = theme(plot.margin = margin())) & t 

ggsave("figures/scores_mse.pdf", width = 164, height = 185, unit = "mm", device = "pdf")



# Absolute error

p1 <- plot_scores("median", "national", by_horizon = FALSE, relative = TRUE)
p2 <- plot_scores("median", "national", by_horizon = TRUE)
p2b <- plot_scores("median", "national", by_horizon = TRUE, relative = TRUE)
p3 <- plot_scores("median", "states", by_horizon = FALSE, relative = TRUE)
p4 <- plot_scores("median", "states", by_horizon = TRUE)
p4b <- plot_scores("median", "states", by_horizon = TRUE, relative = TRUE)
p5 <- plot_scores("median", "age", by_horizon = FALSE, relative = TRUE)
p6 <- plot_scores("median", "age", by_horizon = TRUE)
p6b <- plot_scores("median", "age", by_horizon = TRUE, relative = TRUE)

# wrap_elements(p1 + p2 + p2b + plot_annotation(title = "National level") & theme(plot.title = element_text(hjust = 0.5), aspect.ratio = 1)) /
#   wrap_elements(p3 + p4 + p4b + plot_annotation(title = "Average across states") & theme(plot.title = element_text(hjust = 0.5), aspect.ratio = 1)) /
#   wrap_elements(p5 + p6 + p6b + plot_annotation(title = "Average across age groups") & theme(plot.title = element_text(hjust = 0.5), aspect.ratio = 1))
# 
# ggsave("figures/scores_ae.pdf", width = 300, height = 350, unit = "mm", device = "pdf")

(p1 + p2 + labs(title = "National level") + p2b) /
  (p3 + p4 + labs(title = "Average across states") + p4b) /
  (p5 + p6 + labs(title = "Average across age groups") + p6b) + plot_annotation(theme = theme(plot.margin = margin())) & t 

ggsave("figures/scores_ae.pdf", width = 164, height = 185, unit = "mm", device = "pdf")



#### 0 - 7 days back

# p1 <- plot_scores("quantile", "national", by_horizon = FALSE, relative = TRUE, add_ae = TRUE, 
#                   short_horizons = TRUE) + labs(title = "National level")
# p2 <- plot_scores("quantile", "states", by_horizon = FALSE, relative = TRUE, add_ae = TRUE,
#                   short_horizons = TRUE) + labs(title = "States")
# p3 <- plot_scores("quantile", "age", by_horizon = FALSE, relative = TRUE, add_ae = TRUE,
#                   short_horizons = TRUE) + labs(title = "Age groups") + expand_limits(y = 128)
# 
# 
# df2 <- load_data(add_baseline = FALSE, add_median = FALSE, shorten_names = TRUE, 
#                  fix_data = TRUE, add_truth = TRUE, exclude_missing = TRUE, eval_date = "2022-08-08")
# 
# df2 <- df2 %>% 
#   mutate(model = fct_relevel(model, c(
#     "Epiforecasts", "ILM", "KIT",
#     "LMU", "RIVM", "RKI", "SU", "SZ", "MeanEnsemble", "MedianEnsemble"
#   )))
# 
# df2 <- df2 %>% 
#   filter(target %in% paste(0:7*-1, "day ahead inc hosp"))
# 
# p4 <- plot_coverage_all(df2, "national") + theme(legend.position = "none")
# p5 <- plot_coverage_all(df2, "states") + theme(legend.position = "none")
# p6 <- plot_coverage_all(df2, "age")  + theme(legend.position = "right") 
# 
# (p1 + p2 + p3) /
#   (p4 + p5 + p6) & theme(plot.title = element_text(hjust = 0.5), aspect.ratio = 1)
# 
# 
# ggsave("figures/scores_0-7d.pdf", width = 350, height = 200, unit = "mm", device = "pdf")
