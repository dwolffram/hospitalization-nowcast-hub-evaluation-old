source("utils.R")
library(patchwork)

plot_wis <- function(level = "national", add_ae = TRUE, short_horizons = FALSE, per_100k = FALSE) {
  df <- read_csv(paste0("data/wis_", level, ifelse(short_horizons, "_7d", ""), 
                        ifelse(per_100k, "_100k", ""), ".csv.gz"))

  df <- df %>%
    mutate(model = fct_relevel(model, c(
      "Epiforecasts", "ILM", "KIT-frozen_baseline", "KIT",
      "LMU", "RIVM", "RKI", "SU", "SZ", "MeanEnsemble", "MedianEnsemble"
    )))

  base_score <- df %>%
    filter(model == "KIT-frozen_baseline") %>%
    pull(score)

  df <- df %>%
    filter(model != "KIT-frozen_baseline") %>% 
    mutate(model = fct_drop(model, only = "KIT-frozen_baseline"))

  scores <- df %>%
    select(-score) %>%
    pivot_longer(cols = c(underprediction, spread, overprediction), names_to = "penalty")

  if (add_ae) {
    df_ae <- load_scores(aggregate_scores = FALSE, load_baseline = FALSE, 
                         short_horizons = short_horizons, per_100k = per_100k)
    df_ae <- df_ae %>%
      mutate(model = fct_relevel(model, c(
        "Epiforecasts", "ILM", "KIT-frozen_baseline", "KIT",
        "LMU", "RIVM", "RKI", "SU", "SZ", "MeanEnsemble", "MedianEnsemble"
      )))
    df_ae <- filter_scores(df_ae, "median", level, by_horizon = FALSE) %>%
      filter(model != "KIT-frozen_baseline") %>% 
      mutate(model = fct_drop(model, only = "KIT-frozen_baseline"))
  }

  ggplot() +
    {
      if (add_ae) geom_point(data = df_ae, aes(x = model, y = score, fill = model), 
                             shape = 23, size = 0.5)
    } +
    geom_bar(data = df, aes(x = model, y = score), fill = "white", stat = "identity") + # so you can't see through bars
    geom_bar(data = scores, aes(x = model, y = value, fill = model, alpha = penalty, color = model), size = 0.1, stat = "identity") +
    geom_label(
      data = df, aes(x = model, y = 0.5 * score, label = sprintf("%0.1f", round(score, digits = 1))),
      fill = "white", alpha = 1, hjust = 0.5,
      label.r = unit(0.15, "lines"), # 0.25
      size = 5/.pt, # 9 * 5 / 14
      label.padding = unit(0.1, "lines") # 0.15
    ) +
    scale_fill_manual(values = MODEL_COLORS, guide = "none") +
    scale_color_manual(values = MODEL_COLORS, guide = "none") +
    scale_alpha_manual(
      values = c(0.5, 0.2, 1), labels = c("Overprediction", "Spread", "Underprediction"),
      guide = guide_legend(reverse = TRUE, title.position = "top", title.hjust = 0.5)
    ) +
    scale_x_discrete(limits = rev, drop = FALSE) +
    scale_y_continuous(
      name = paste0("Mean WIS", ifelse(add_ae, " / AE", "")),
      sec.axis = sec_axis(
        trans = ~ . / base_score,
        name = paste("Relative", "WIS")
      )
    ) +
    labs(
      # y = paste0("Mean WIS", ifelse(add_ae, "/AE", "")),
      x = NULL,
      color = "Model",
      alpha = "Decomposition of WIS"
    ) +
    coord_flip() +
    theme_bw() +
    theme(
      legend.position = "bottom"#,
      # legend.key.height = unit(0.3, "cm"),
      # legend.key.width = unit(0.3, "cm"),
      # legend.text = element_text(size = 7.5)
    )
}


df <- read_csv(paste0("data/wis_national.csv.gz"))

df <- df %>%
  mutate(model = fct_relevel(model, rev(c(
    "Epiforecasts", "ILM", "KIT-frozen_baseline", "KIT",
    "LMU", "RIVM", "RKI", "SU", "SZ", "MeanEnsemble", "MedianEnsemble"
  ))))

df <- df %>% 
  filter(model != "KIT-frozen_baseline",
         model != "KIT")

df2 <- df %>% 
  mutate(model = fct_drop(model, only = "KIT-frozen_baseline"))

unique(df2$model)

plot_wis("national")

p1 <- plot_wis("national")
p3 <- plot_wis("states")
p5 <- plot_wis("age")

p2 <- plot_scores("quantile", "national", by_horizon = TRUE)
p2b <- plot_scores("quantile", "national", by_horizon = TRUE, relative = TRUE)

p4 <- plot_scores("quantile", "states", by_horizon = TRUE)
p4b <- plot_scores("quantile", "states", by_horizon = TRUE, relative = TRUE)

p6 <- plot_scores("quantile", "age", by_horizon = TRUE)
p6b <- plot_scores("quantile", "age", by_horizon = TRUE, relative = TRUE)

wrap_elements(p1 + p2 + p2b + plot_annotation(title = "National level") & theme(plot.title = element_text(hjust = 0.5), aspect.ratio = 1)) /
  wrap_elements(p3 + p4 + p4b + plot_annotation(title = "States") & theme(plot.title = element_text(hjust = 0.5), aspect.ratio = 1)) /
  wrap_elements(p5 + p6 + p6b + plot_annotation(title = "Age groups") & theme(plot.title = element_text(hjust = 0.5), aspect.ratio = 1))

wrap_elements(p1 + p2 + p2b + plot_annotation(title = "National level") & theme(plot.title = element_text(hjust = 0.5))) /
  wrap_elements(p3 + p4 + p4b + plot_annotation(title = "States") & theme(plot.title = element_text(hjust = 0.5))) /
  wrap_elements(p5 + p6 + p6b + plot_annotation(title = "Age groups") & theme(plot.title = element_text(hjust = 0.5)))

ggsave("figures/scores_wis.pdf", width = 300, height = 350, unit = "mm", device = "pdf")



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
        plot.margin = unit(c(2, 2, 2, 2), "pt"), 
        legend.margin = margin(4, 0, 0, 0),
        legend.box.spacing = unit(0, "pt"),
        legend.background = element_rect(fill='transparent')))


(p1 + p2 + labs(title = "National level") + p2b) /
  (p3 + p4 + labs(title = "States") + p4b) /
  (p5 + p6 + labs(title = "Age groups") + p6b) + plot_annotation(theme = theme(plot.margin = margin())) & t 

ggsave("figures/scores_wis.pdf", width = 164, height = 200, unit = "mm", device = "pdf")


#### 0-7 days back

plot_wis("national", short_horizons = TRUE)

p1 <- plot_wis("national", short_horizons = TRUE) + theme(legend.position = "none") + labs(title = "National level")
p2 <- plot_wis("states", short_horizons = TRUE) + theme(legend.position = "none") + labs(title = "States")
p3 <- plot_wis("age", short_horizons = TRUE) + theme(legend.position = "right", legend.justification = "left") + labs(title = "Age groups")


df2 <- load_data(
  add_baseline = FALSE, add_median = FALSE, shorten_names = TRUE,
  fix_data = TRUE, add_truth = TRUE, exclude_missing = TRUE, eval_date = "2022-08-08"
)

df2 <- df2 %>%
  mutate(model = fct_relevel(model, c(
    "Epiforecasts", "ILM", "KIT",
    "LMU", "RIVM", "RKI", "SU", "SZ", "MeanEnsemble", "MedianEnsemble"
  )))

df2 <- df2 %>%
  filter(target %in% paste(0:7 * -1, "day ahead inc hosp"))

p4 <- plot_coverage_all(df2, "national") + theme(legend.position = "none")
p5 <- plot_coverage_all(df2, "states") + theme(legend.position = "none")
p6 <- plot_coverage_all(df2, "age") + theme(legend.position = "right", legend.justification = "left")


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
  plot.margin = unit(c(2, 2, 10, 2), "pt"), 
  legend.margin = margin(0, 0, 0, 4),
  legend.box.spacing = unit(0, "pt"),
  legend.background = element_rect(fill='transparent')))

((p1 + p2 + theme(axis.ticks.y = element_blank(), axis.text.y = element_blank()) + p3 + theme(axis.ticks.y = element_blank(), axis.text.y = element_blank())) /
  (p4 + p5 + theme(axis.ticks.y = element_blank(), axis.text.y = element_blank()) + p6 + theme(axis.ticks.y = element_blank(), axis.text.y = element_blank())) & t) + plot_annotation(theme = theme(plot.margin = margin()))



ggsave("figures/scores_0-7d.pdf", width = 164, height = 100, unit = "mm", device = "pdf")


# ggsave("figures/scores_0-7d.pdf", width = 350, height = 200, unit = "mm", device = "pdf")
