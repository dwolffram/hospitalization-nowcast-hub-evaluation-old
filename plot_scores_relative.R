library(tidyverse)
library(patchwork)
source("utils.R")

metrics <- setNames(
  c("absolute error", "squared error", "WIS"),
  c("median", "mean", "quantile")
)

plot_scores <- function(df, type = "quantile", level = "national", by_horizon = FALSE, relative = FALSE, add_ae = FALSE) {
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
      # geom_point() +
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
    
    # max_score <- max(scores_ae$score)
    # ylim <- (1 + 0.1*nchar(trunc(abs(max_score)))) * max_score # depending on the number of digits

    ggplot() +
      {if (add_ae) geom_point(data = scores_ae, aes(x = model, y = score), shape = 4)} +
      geom_bar(data = scores, aes(x = model, y = score, fill = model), stat = "identity") +
      # geom_text(aes(label = sprintf("%0.2f", round(score, digits = 2))), hjust = -0.25, size = 9 * 5 / 14) +
      geom_label(data = scores, aes(x = model, y = 0.9*score, label = sprintf("%0.2f", round(score, digits = 2))), 
                 fill = "white", alpha = 0.7, hjust = 1,
                 label.size = NA, label.r = unit(0, "pt"), size = 9 * 5 / 14) +
      scale_fill_manual(values = MODEL_COLORS) +
      labs(
        y = ylabel,
        x = NULL,
        color = "Model"
      ) + 
      
      coord_flip() +
      # expand_limits(y = ylim) +
      # scale_x_discrete(limits = c("MedianEnsemble", "MeanEnsemble", "SZ", "SU", "RKI", "RIVM", 
      #                             "LMU", "KIT", "ILM", "Epiforecasts")) +
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

df <- load_scores(aggregate_scores = TRUE, shorten_names = TRUE)

df <- df %>% 
  mutate(model = fct_relevel(model, rev(c(
    "Epiforecasts", "ILM", "KIT-frozen_baseline", "KIT",
    "LMU", "RIVM", "RKI", "SU", "SZ", "MeanEnsemble", "MedianEnsemble"
  ))))

# Quantile score

p1 <- plot_scores(df, "quantile", "national", by_horizon = FALSE, relative = TRUE, add_ae = TRUE)
p2 <- plot_scores(df, "quantile", "national", by_horizon = TRUE)
p2b <- plot_scores(df, "quantile", "national", by_horizon = TRUE, relative = TRUE)
p3 <- plot_scores(df, "quantile", "states", by_horizon = FALSE, relative = TRUE, add_ae = TRUE)
p4 <- plot_scores(df, "quantile", "states", by_horizon = TRUE)
p4b <- plot_scores(df, "quantile", "states", by_horizon = TRUE, relative = TRUE)
p5 <- plot_scores(df, "quantile", "age", by_horizon = FALSE, relative = TRUE, add_ae = TRUE)
p6 <- plot_scores(df, "quantile", "age", by_horizon = TRUE)
p6b <- plot_scores(df, "quantile", "age", by_horizon = TRUE, relative = TRUE)


wrap_elements(p1 + p2 + p2b + plot_annotation(title = "National level") & theme(plot.title = element_text(hjust = 0.5), aspect.ratio = 1)) /
  wrap_elements(p3 + p4 + p4b + plot_annotation(title = "Average across states") & theme(plot.title = element_text(hjust = 0.5), aspect.ratio = 1)) /
  wrap_elements(p5 + p6 + p6b + plot_annotation(title = "Average across age groups") & theme(plot.title = element_text(hjust = 0.5), aspect.ratio = 1))

ggsave("figures/scores_relative_qs.pdf", width = 300, height = 350, unit = "mm", device = "pdf")


# Squared error

p1 <- plot_scores(df, "mean", "national", by_horizon = FALSE, relative = TRUE)
p2 <- plot_scores(df, "mean", "national", by_horizon = TRUE)
p2b <- plot_scores(df, "mean", "national", by_horizon = TRUE, relative = TRUE)
p3 <- plot_scores(df, "mean", "states", by_horizon = FALSE, relative = TRUE)
p4 <- plot_scores(df, "mean", "states", by_horizon = TRUE)
p4b <- plot_scores(df, "mean", "states", by_horizon = TRUE, relative = TRUE)
p5 <- plot_scores(df, "mean", "age", by_horizon = FALSE, relative = TRUE)
p6 <- plot_scores(df, "mean", "age", by_horizon = TRUE)
p6b <- plot_scores(df, "mean", "age", by_horizon = TRUE, relative = TRUE)

wrap_elements(p1 + p2 + p2b + plot_annotation(title = "National level") & theme(plot.title = element_text(hjust = 0.5), aspect.ratio = 1)) /
  wrap_elements(p3 + p4 + p4b + plot_annotation(title = "Average across states") & theme(plot.title = element_text(hjust = 0.5), aspect.ratio = 1)) /
  wrap_elements(p5 + p6 + p6b + plot_annotation(title = "Average across age groups") & theme(plot.title = element_text(hjust = 0.5), aspect.ratio = 1))

ggsave("figures/scores_relative_mse.pdf", width = 300, height = 350, unit = "mm", device = "pdf")


# Absolute error

p1 <- plot_scores(df, "median", "national", by_horizon = FALSE, relative = TRUE)
p2 <- plot_scores(df, "median", "national", by_horizon = TRUE)
p2b <- plot_scores(df, "median", "national", by_horizon = TRUE, relative = TRUE)
p3 <- plot_scores(df, "median", "states", by_horizon = FALSE, relative = TRUE)
p4 <- plot_scores(df, "median", "states", by_horizon = TRUE)
p4b <- plot_scores(df, "median", "states", by_horizon = TRUE, relative = TRUE)
p5 <- plot_scores(df, "median", "age", by_horizon = FALSE, relative = TRUE)
p6 <- plot_scores(df, "median", "age", by_horizon = TRUE)
p6b <- plot_scores(df, "median", "age", by_horizon = TRUE, relative = TRUE)

wrap_elements(p1 + p2 + p2b + plot_annotation(title = "National level") & theme(plot.title = element_text(hjust = 0.5), aspect.ratio = 1)) /
  wrap_elements(p3 + p4 + p4b + plot_annotation(title = "Average across states") & theme(plot.title = element_text(hjust = 0.5), aspect.ratio = 1)) /
  wrap_elements(p5 + p6 + p6b + plot_annotation(title = "Average across age groups") & theme(plot.title = element_text(hjust = 0.5), aspect.ratio = 1))

ggsave("figures/scores_relative_ae.pdf", width = 300, height = 350, unit = "mm", device = "pdf")








# 0 - 7 days horizon

df <- load_scores(aggregate_scores = TRUE, shorten_names = TRUE)

df <- df %>% 
  filter(target %in% paste(0:7*-1, "day ahead inc hosp"))

p1 <- plot_scores(df, "quantile", "national", by_horizon = FALSE, relative = TRUE) + labs(title = "National level")
p2 <- plot_scores(df, "quantile", "states", by_horizon = FALSE, relative = TRUE) + labs(title = "States")
p3 <- plot_scores(df, "quantile", "age", by_horizon = FALSE, relative = TRUE) + labs(title = "Age groups")


df2 <- load_data(add_baseline = FALSE, add_median = FALSE, shorten_names = TRUE, 
                fix_data = TRUE, add_truth = TRUE, exclude_missing = TRUE, eval_date = "2022-08-08")
df2 <- df2 %>% 
  filter(target %in% paste(0:7*-1, "day ahead inc hosp"))

p4 <- plot_coverage_all(df2, "national")
p5 <- plot_coverage_all(df2, "states")
p6 <- plot_coverage_all(df2, "age")

(p1 + p2 + p3) /
 (p4 + p5 + p6) & theme(plot.title = element_text(hjust = 0.5), aspect.ratio = 1)


ggsave("figures/scores_0-7d.pdf", width = 350, height = 200, unit = "mm", device = "pdf")



#
# plot_scores(df, "quantile", "age", by_horizon = TRUE, relative = FALSE)
# plot_scores(df, "quantile", "states", TRUE, TRUE)
# plot_scores(df, "quantile", "age", FALSE, FALSE)
# plot_scores(df, "mean", "age", FALSE, TRUE)
#
#
# type <- "quantile"
# level <- "national"
# by_horizon <- FALSE
#
# scores <- filter_scores(df, type, level, by_horizon)
#
# base_score <- scores %>%
#   filter(model == "KIT-frozen_baseline") %>%
#   pull(score)
#
# scores <- scores %>%
#   filter(model != "KIT-frozen_baseline")
#
# ggplot(scores, aes(x = model, y = score, fill = model)) +
#   geom_bar(stat = "identity") +
#   geom_text(aes(label = round(score, digits = 2)), hjust = -0.25) +
#   scale_fill_manual(values = MODEL_COLORS) +
#   labs(
#     y = ylabel,
#     x = NULL,
#     color = "Model"
#   ) +
#   coord_flip() +
#   expand_limits(y = 1.1 * max(scores$score)) +
#   scale_x_discrete(limits = rev(unique(scores$model))) +
#   theme_bw() +
#   theme(legend.position = "none") +
#   scale_y_continuous(
#     name = "Mean quantile score",
#     sec.axis = sec_axis(trans = ~ . / base_score, name = "Relative quantile score")
#   )
#
#
# scores <- filter_scores(df, "quantile", "national", TRUE)
#
# scores_baseline <- scores %>%
#   filter(model == "KIT-frozen_baseline")
#
# base_scores <- scores_baseline %>%
#   select(-c(model, target)) %>%
#   rename(base_score = score)
#
# scores <- scores %>%
#   filter(model != "KIT-frozen_baseline")
#
# scores <- scores %>%
#   left_join(base_scores)
#
# scores <- scores %>%
#   mutate(score = score / base_score)
#
# ggplot(scores, aes(x = horizon, y = score, color = model)) +
#   geom_point() +
#   geom_line(size = 1.25) +
#   scale_color_manual(values = MODEL_COLORS) +
#   labs(
#     x = "Horizon (days)",
#     y = ylabel,
#     color = "Model"
#   ) +
#   theme_bw() +
#   theme(legend.position = "none")
