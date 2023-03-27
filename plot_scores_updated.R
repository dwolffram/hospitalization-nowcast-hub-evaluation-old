source("utils.R")
library(patchwork)

ALL_MODELS <- c(
  "ILM", "ILM (updated)", "KIT", "KIT (updated)", "LMU", "LMU (updated)", "RKI", "RKI (updated)" 
)

COLORS_UPDATED <- setNames(
  c("#56B4E9", "#3c7da3", "#F0E442", "#a89f2e", "#E69F00", "#a16f00", "#3C4AAD", "#2a3379"),
  c("KIT", "KIT (updated)", "LMU", "LMU (updated)", "ILM", "ILM (updated)", "RKI", "RKI (updated)")
)


plot_wis_updated <- function(level = "national") {
  df <- read_csv(paste0("data/wis_", level, ".csv.gz"))
  
  base_score <- df %>%
    filter(model == "KIT-frozen_baseline") %>%
    pull(score)
  
  df <- df %>% 
    filter(model %in% c("ILM", "KIT", "LMU", "RKI"))
  
  df_updated <- read_csv(paste0("data/wis_updated_", level, ".csv.gz"))%>% 
    mutate(model = paste(model, "(updated)"))
  
  
  df <- bind_rows(df, df_updated)
  
  df <- df %>%
    mutate(model = fct_expand(model, ALL_MODELS),
           model = fct_relevel(model, ALL_MODELS))
  
  scores <- df %>%
    select(-score) %>%
    pivot_longer(cols = c(underprediction, spread, overprediction), names_to = "penalty")
  
  df_ae <- load_scores(aggregate_scores = FALSE, load_baseline = FALSE, short_horizons = FALSE)
  df_ae <- filter_scores(df_ae, "median", level, by_horizon = FALSE) %>%
    filter(model %in% c("ILM", "KIT", "LMU", "RKI"))
  
  
  df_ae_updated <- read_csv("data/scores_updated.csv.gz")
  df_ae_updated <- filter_scores(df_ae_updated, "median", level, by_horizon = FALSE) %>% 
    mutate(model = paste(model, "(updated)"))
  
  df_ae <- bind_rows(df_ae, df_ae_updated)
  
  ggplot() +
    geom_point(data = df_ae, aes(x = model, y = score, fill = model), shape = 23, size = 0.5) +
    geom_bar(data = df, aes(x = model, y = score), fill = "white", stat = "identity") + # so you can't see through bars
    geom_bar(data = scores, aes(x = model, y = value, fill = model, alpha = penalty, color = model), size = 0.1, stat = "identity") +
    geom_label(
      data = df, aes(x = model, y = 0.5 * score, label = sprintf("%0.1f", round(score, digits = 1))),
      fill = "white", alpha = 1, hjust = 0.5,
      label.r = unit(0.15, "lines"), # 0.25
      size = 5/.pt, # 9 * 5 / 14
      label.padding = unit(0.1, "lines") # 0.15
    ) +
    scale_fill_manual(values = COLORS_UPDATED, guide = "none") +
    scale_color_manual(values = COLORS_UPDATED, guide = "none") +
    scale_alpha_manual(
      values = c(0.5, 0.2, 1), labels = c("Overprediction", "Spread", "Underprediction"),
      guide = guide_legend(reverse = TRUE, title.position = "top", title.hjust = 0.5)
    ) +
    scale_x_discrete(limits = rev, drop = FALSE) +
    scale_y_continuous(
      name = "Mean WIS / AE",
      sec.axis = sec_axis(
        trans = ~ . / base_score,
        name = "Relative WIS"
      )
    ) +
    labs(
      x = NULL,
      alpha = "Decomposition of WIS"
    ) +
    coord_flip() +
    theme_bw() +
    theme(
      legend.key.size = unit(0.4, "lines")
      legend.position = "bottom"#,
      # legend.key.height = unit(0.3, "cm"),
      # legend.key.width = unit(0.3, "cm"),
      # legend.text = element_text(size = 7.5)
    )
}



plot_wis_updated("national")
plot_wis_updated("states")
plot_wis_updated("age")


level <- "national"

df <- load_data(add_baseline = FALSE, add_median = FALSE, shorten_names = TRUE, 
                fix_data = TRUE, add_truth = TRUE, exclude_missing = TRUE, eval_date = "2022-08-08")
df <- df %>% 
  filter(model %in% c("ILM", "KIT", "LMU", "RKI"),
         type == "quantile")

df_updated <- read_csv("data/submissions_updated.csv.gz") %>% 
  filter(type == "quantile") %>% 
  mutate(model = paste(model, "(updated)"))

df_truth <- load_truth(as_of = "2022-08-08")

df_updated <- df_updated %>%
  left_join(df_truth, by = c("location", "age_group", "target_end_date" = "date"))


df <- bind_rows(df, df_updated)

df <- df %>%
  mutate(model = fct_expand(model, ALL_MODELS),
         model = fct_relevel(model, ALL_MODELS))


plot_coverage_all <- function(df, level = "national") {
  df <- filter_data(df, type = "quantile", level = level)
  
  df <- df %>%
    mutate(model = fct_expand(model, ALL_MODELS),
           model = fct_relevel(model, ALL_MODELS))
  
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
  
  
  alphas <- setNames(c(0.7, 0.4), c("50%", "95%"))
  
  ggplot(coverage_df, aes(x = model)) +
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
    scale_fill_manual(values = COLORS_UPDATED) +
    coord_flip() +
    scale_x_discrete(limits = rev, drop = FALSE) +
    guides(fill = "none") +
    scale_alpha_manual(values = alphas) +
    theme_bw() +
    # theme(legend.position = "right")
    theme(legend.position = c(0.9, 0.35), legend.justification = c(1, 1), legend.box.just = "left")
}

plot_coverage_all(df)



p1 <- plot_wis_updated("national") + theme(legend.position = "none") + labs(title = "National level")
p2 <- plot_wis_updated("states") + theme(legend.position = "none") + labs(title = "States")
p3 <- plot_wis_updated("age") + theme(legend.position = "right", legend.justification = "left") + labs(title = "Age groups")

p4 <- plot_coverage_all(df, "national") + theme(legend.position = "none")
p5 <- plot_coverage_all(df, "states") + theme(legend.position = "none")
p6 <- plot_coverage_all(df, "age") + theme(legend.position = "right", legend.justification = "left")


t <- list(theme(
  plot.title = element_text(size = 8, hjust = 0.5, face = "bold"),
  legend.title = element_text(size = 6), 
  legend.text  = element_text(size = 5),
  legend.key.size = unit(0.4, "lines"),
  axis.title = element_text(size = 7),
  axis.text = element_text(size = 6),
  axis.ticks = element_line(colour = "black", size = 0.25),
  panel.grid.major = element_line(size = 0.15),
  panel.grid.minor = element_line(size = 0.1),
  plot.margin = unit(c(0, 5, 10, 2), "pt"), 
  legend.margin = margin(0, 0, 0, 4),
  legend.box.spacing = unit(0, "pt"),
  legend.background = element_rect(fill='transparent')))


((p1 + p2 + theme(axis.ticks.y = element_blank(), axis.text.y = element_blank()) + p3 + theme(axis.ticks.y = element_blank(), axis.text.y = element_blank())) /
  (p4 + p5 + theme(axis.ticks.y = element_blank(), axis.text.y = element_blank()) + p6 + theme(axis.ticks.y = element_blank(), axis.text.y = element_blank())) & t) + plot_annotation(theme = theme(plot.margin = margin()))


ggsave("figures/scores_updated.pdf", width = 164, height = 90, unit = "mm", device = "pdf")



# ggsave("figures/scores_updated.pdf", width = 350, height = 200, unit = "mm", device = "pdf")
