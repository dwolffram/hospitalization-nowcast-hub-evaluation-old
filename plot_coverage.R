library(tidyverse)
library(patchwork)
source("utils.R")

plot_coverage <- function(df, level = "national") {
  df <- filter_data(df, type = "quantile", level = level) %>%
    mutate(horizon = as.numeric(str_extract(target, "-?\\d+")))
  
  df_wide <- df %>%
    pivot_wider(names_from = quantile, values_from = value, names_prefix = "quantile_")

  df_wide <- df_wide %>%
    mutate(
      c50 = (truth >= quantile_0.25 & truth <= quantile_0.75),
      c95 = (truth >= quantile_0.025 & truth <= quantile_0.975)
    )

  coverage_df <- df_wide %>%
    group_by(model, horizon) %>%
    summarize(
      c50 = mean(c50, na.rm = TRUE),
      c95 = mean(c95, na.rm = TRUE)
    )

  coverage_df <- coverage_df %>%
    filter(horizon %in% c(0, -7, -14)) %>%
    mutate(horizon = paste(horizon, "days"))

  alphas <- setNames(c(0.7, 0.4), c("50%", "95%"))

  ggplot(coverage_df, aes(x = model)) +
    facet_wrap("horizon") +
    expand_limits(y = 1) +
    geom_col(aes(y = c95, fill = model, alpha = "95%")) +
    geom_col(aes(y = c50, fill = model, alpha = "50%")) +
    geom_hline(yintercept = c(0.5, 0.95), linetype = "dashed") +
    scale_y_continuous(labels = function(y) ifelse(y == 0, "0", y)) +
    labs(
      x = NULL,
      y = "Empirical coverage",
      color = "Model",
      alpha = "Prediction \ninterval"
    ) +
    scale_fill_manual(values = MODEL_COLORS) +
    coord_flip() +
    scale_x_discrete(limits = rev(unique(coverage_df$model))) +
    guides(fill = "none") +
    scale_alpha_manual(values = alphas) +
    theme_bw() +
    theme(legend.position = "right")
}

# df <- load_scores(aggregate_scores = FALSE, shorten_names = TRUE, load_baseline = FALSE)

df <- load_data(add_baseline = FALSE, add_median = FALSE, shorten_names = TRUE, 
                fix_data = TRUE, add_truth = TRUE, exclude_missing = TRUE, eval_date = "2022-08-08")

p1 <- plot_coverage(df, "national")
p2 <- plot_coverage(df, "states")
p3 <- plot_coverage(df, "age")

wrap_elements(p1 + plot_annotation(title = "National level") & theme(plot.title = element_text(hjust = 0.5))) /
  wrap_elements(p2 + plot_annotation(title = "States") & theme(plot.title = element_text(hjust = 0.5))) /
  wrap_elements(p3 + plot_annotation(title = "Age groups") & theme(plot.title = element_text(hjust = 0.5)))


ggsave("figures/coverage.pdf", width = 300, height = 350, unit = "mm", device = "pdf")


######

plot_coverage_lines <- function(df, level = "national") {
  TITLES <- setNames(
    c("National level", "States", "Age groups"),
    c("national", "states", "age")
  )

  df <- filter_data(df, type = "quantile", level = level) %>%
    mutate(horizon = as.numeric(str_extract(target, "-?\\d+")))

  df_wide <- df %>%
    pivot_wider(names_from = quantile, values_from = value, names_prefix = "quantile_")

  df_wide <- df_wide %>%
    mutate(
      c50 = (truth >= quantile_0.25 & truth <= quantile_0.75),
      c95 = (truth >= quantile_0.025 & truth <= quantile_0.975)
    )

  coverage_df <- df_wide %>%
    group_by(model, horizon) %>%
    summarize(
      c50 = mean(c50, na.rm = TRUE),
      c95 = mean(c95, na.rm = TRUE)
    )

  coverage_long <- coverage_df %>%
    pivot_longer(cols = c(c50, c95), names_to = "quantile") %>%
    mutate(
      quantile_label = paste0(str_sub(quantile, 2, 3), "% prediction interval"),
      quantile_level = as.numeric(paste0("0.", str_sub(quantile, 2, 3)))
    )

  nominal_levels <- data.frame(
    quantile_label = c("50% prediction interval", "95% prediction interval"),
    quantile_level = c(0.5, 0.95)
  )


  ggplot(coverage_long, aes(x = horizon, y = value, color = model)) +
    facet_wrap("quantile_label") +
    geom_line() +
    theme_bw() +
    scale_color_manual(values = MODEL_COLORS, limits = force) +
    geom_hline(data = nominal_levels, aes(yintercept = quantile_level), linetype = "dashed") +
    labs(
      x = "Horizon (days)",
      y = "Empirical coverage",
      color = "Model",
      title = TITLES[level]
    ) +
    theme(plot.title = element_text(hjust = 0.5))
}

p1 <- plot_coverage_lines(df, "national")
p2 <- plot_coverage_lines(df, "states")
p3 <- plot_coverage_lines(df, "age")

(p1 + theme(legend.position = "none")) /
  (p2 + theme(legend.position = "right")) /
  (p3 + theme(legend.position = "none"))

ggsave("figures/coverage_lines.pdf", width = 300, height = 350, unit = "mm", device = "pdf")


### Coverage across all horizons

plot_coverage_all <- function(df, level = "national") {
  df <- filter_data(df, type = "quantile", level = level) %>%
    mutate(horizon = as.numeric(str_extract(target, "-?\\d+"))) 
  
  df_wide <- df %>%
    pivot_wider(names_from = quantile, values_from = value, names_prefix = "quantile_")
  
  df_wide <- df_wide %>%
    mutate(
      c50 = (truth >= quantile_0.25 & truth <= quantile_0.75),
      c95 = (truth >= quantile_0.025 & truth <= quantile_0.975)
    )
  
  coverage_df <- df_wide %>%
    group_by(model) %>%
    summarize(
      c50 = mean(c50, na.rm = TRUE),
      c95 = mean(c95, na.rm = TRUE)
    )
  
  # coverage_df <- coverage_df %>%
  #   filter(horizon %in% c(0, -7, -14)) %>%
  #   mutate(horizon = paste(horizon, "days"))
  
  alphas <- setNames(c(0.7, 0.4), c("50%", "95%"))
  
  ggplot(coverage_df, aes(x = model)) +
    #facet_wrap("horizon") +
    expand_limits(y = 1) +
    geom_col(aes(y = c95, fill = model, alpha = "95%")) +
    geom_col(aes(y = c50, fill = model, alpha = "50%")) +
    geom_hline(yintercept = c(0.5, 0.95), linetype = "dashed") +
    scale_y_continuous(labels = function(y) ifelse(y == 0, "0", y)) +
    labs(
      x = NULL,
      y = "Empirical coverage",
      color = "Model",
      alpha = "Prediction \ninterval"
    ) +
    scale_fill_manual(values = MODEL_COLORS) +
    coord_flip() +
    scale_x_discrete(limits = rev(unique(coverage_df$model))) +
    guides(fill = "none") +
    scale_alpha_manual(values = alphas) +
    theme_bw() +
    # theme(legend.position = "right")
    theme(legend.position = c(0.9, 0.35), legend.justification = c(1, 1), legend.box.just = "left")
}

p4 <- plot_coverage_all(df, "national")
p5 <- plot_coverage_all(df, "states")
p6 <- plot_coverage_all(df, "age")

p1 <- plot_coverage_lines(df, "national")
p2 <- plot_coverage_lines(df, "states")
p3 <- plot_coverage_lines(df, "age")
 
# ((p4 + p1 + theme(legend.position = "none") + plot_layout(widths = c(1, 2))) /
#   (p5 + p2 + theme(legend.position = "none") + plot_layout(widths = c(1, 2))) /
#   (p6 + p3 + theme(legend.position = "none") + plot_layout(widths = c(1, 2)))) 
# 
# ggsave("figures/coverage_all.pdf", width = 300, height = 350, unit = "mm", device = "pdf")



wrap_elements(p4 + p1 + labs(title = NULL) + theme(legend.position = "none") + plot_layout(widths = c(1, 2)) + plot_annotation(title = "National level") & theme(plot.title = element_text(hjust = 0.5), aspect.ratio = 1)) /
  wrap_elements(p5 + p2 + labs(title = NULL) + theme(legend.position = "none") + plot_layout(widths = c(1, 2)) + plot_annotation(title = "States") & theme(plot.title = element_text(hjust = 0.5), aspect.ratio = 1)) /
  wrap_elements(p6 + p3 + labs(title = NULL) + theme(legend.position = "none") + plot_layout(widths = c(1, 2)) + plot_annotation(title = "Age groups") & theme(plot.title = element_text(hjust = 0.5), aspect.ratio = 1))

ggsave("figures/coverage_all.pdf", width = 300, height = 350, unit = "mm", device = "pdf")

# plot_coverage_all(df, "national") + theme(legend.position = c(0.85, 0.15), legend.justification = c(1, 1), legend.box.just = "left")
