source("utils.R")
library(patchwork)

df <- read_csv("data/scores.csv.gz")

plot_scores_by_date <- function(df, level = "national", relative = TRUE) {
  df_temp <- df %>%
    filter(type == "quantile")

  df_temp <- filter_data(df_temp, level = level) %>%
    group_by(model, forecast_date) %>%
    summarize(score = mean(score))

  if (relative) {
    base_score <- df_temp %>%
      filter(model == "KIT-frozen_baseline") %>%
      rename(base_score = score) %>%
      ungroup() %>%
      select(-model)


    df_temp <- left_join(df_temp, base_score, by = c("forecast_date")) %>%
      mutate(score = score / base_score)
  }

  df_temp <- df_temp %>%
    filter(model != "KIT-frozen_baseline")

  ggplot(df_temp, aes(x = forecast_date, y = score, color = model)) +
    geom_line(size = 0.4) +
    scale_color_manual(values = MODEL_COLORS) +
    theme_bw() +
    labs(
      x = NULL,
      y = ifelse(relative, "Relative WIS", "Mean WIS"),
      color = "Model",
      title = TITLES[level]
    ) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b %Y")
}

p1 <- plot_scores_by_date(df, "national")
p2 <- plot_scores_by_date(df, "states")
p3 <- plot_scores_by_date(df, "age")


t <- list(theme(
  plot.title = element_text(size = 8, hjust = 0.5, margin = margin(10, 0, 5, 0), face = "bold"),
  legend.title = element_text(size = 7),
  legend.text = element_text(size = 6),
  legend.key.size = unit(0.4, "lines"),
  strip.text = element_text(size = 8),
  axis.title = element_text(size = 7),
  axis.text = element_text(size = 6),
  axis.ticks = element_line(colour = "black", size = 0.25),
  panel.grid.major = element_line(size = 0.15),
  panel.grid.minor = element_line(size = 0.1),
  plot.margin = unit(c(2, 2, 2, 20), "pt")
))

((p1 + theme(legend.position = "none")) /
  (p2 + scale_color_manual(
    breaks = c(
      "Epiforecasts", "ILM", "KIT", "LMU", "RIVM",
      "RKI", "SU", "SZ", "MeanEnsemble", "MedianEnsemble"
    ),
    values = MODEL_COLORS
  )) /
  (p3 + theme(legend.position = "none"))) + plot_annotation(theme = theme(plot.margin = margin())) & t


ggsave("figures/scores_by_date.pdf", width = 164, height = 200, unit = "mm", device = "pdf")







# library(lubridate)
#
# wday(df$forecast_date, label = TRUE)
#
# df_wday <- filter_data(df, level = "national") %>%
#   filter(target == "0 day ahead inc hosp") %>%
#   mutate(weekday = wday(forecast_date, label = TRUE)) %>%
#   group_by(model, weekday) %>%
#   summarize(score = mean(score))
#
#
# ggplot(df_wday, aes(x = weekday, y = score, fill = model)) +
#   facet_wrap("model") +
#   geom_col() +
#   theme_bw() +
#   scale_fill_manual(values = MODEL_COLORS, guide = "none") +
#   labs(
#     x = "Weekday",
#     y = "Mean WIS",
#     title = "National level, horizon: 0 days"
#   )
#
# ggsave("figures/wis_weekday_national.pdf", width = 300, height = 250, unit = "mm", device = "pdf")
#
#
# df_wday <- filter_data(df, level = "states") %>%
#   filter(target == "0 day ahead inc hosp") %>%
#   mutate(weekday = wday(forecast_date, label = TRUE)) %>%
#   group_by(model, weekday) %>%
#   summarize(score = mean(score))
#
#
# ggplot(df_wday, aes(x = weekday, y = score, fill = model)) +
#   facet_wrap("model") +
#   geom_col() +
#   theme_bw() +
#   scale_fill_manual(values = MODEL_COLORS, guide = "none") +
#   labs(
#     x = "Weekday",
#     y = "Mean WIS",
#     title = "States, horizon: 0 days"
#   )
#
# ggsave("figures/wis_weekday_states.pdf", width = 300, height = 250, unit = "mm", device = "pdf")
#
#
# df_wday <- filter_data(df, level = "age") %>%
#   filter(target == "0 day ahead inc hosp") %>%
#   mutate(weekday = wday(forecast_date, label = TRUE)) %>%
#   group_by(model, weekday) %>%
#   summarize(score = mean(score))
#
#
# ggplot(df_wday, aes(x = weekday, y = score, fill = model)) +
#   facet_wrap("model") +
#   geom_col() +
#   theme_bw() +
#   scale_fill_manual(values = MODEL_COLORS, guide = "none") +
#   labs(
#     x = "Weekday",
#     y = "Mean WIS",
#     title = "Age groups, horizon: 0 days"
#   )
#
# ggsave("figures/wis_weekday_age.pdf", width = 300, height = 250, unit = "mm", device = "pdf")
