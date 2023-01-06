library(tidyverse)
library(patchwork)
source("utils.R")

metrics <- setNames(
  c("absolute error", "squared error", "quantile score"),
  c("median", "mean", "quantile")
)

plot_scores <- function(df, type = "quantile", level = "national", by_horizon = FALSE, relative = FALSE) {
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
      geom_point() +
      geom_line(size = 1) +
      scale_color_manual(values = MODEL_COLORS) +
      labs(
        x = "Horizon (days)",
        y = ylabel,
        color = "Model"
      ) +
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

    ggplot(scores, aes(x = model, y = score, fill = model)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = round(score, digits = 2)), hjust = -0.25, size = 9 * 5 / 14) +
      scale_fill_manual(values = MODEL_COLORS) +
      labs(
        y = ylabel,
        x = NULL,
        color = "Model"
      ) +
      coord_flip() +
      expand_limits(y = 1.2 * max(scores$score)) +
      scale_x_discrete(limits = rev(unique(scores$model))) +
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

df <- load_scores(aggregate_scores = TRUE)



df <- data.frame()

# for (d in as.list(seq(as.Date("2022-05-10"), as.Date("2022-08-08"), by = 1))) {
#   print(d)
#   df_temp <- read_csv(paste0("data/scores_", d, ".csv.gz"), show_col_types = FALSE)
#   df_temp$eval_date <- d
#   df <- bind_rows(df, df_temp)
# }
# 
# for (d in as.list(seq(as.Date("2022-05-10"), as.Date("2022-10-02"), by = 1))) {
#   print(d)
#   df_temp <- read_csv(paste0("data/scores_", d, ".csv.gz"), show_col_types = FALSE)
#   df_temp$eval_date <- d
#   df <- bind_rows(df, df_temp)
# }

for (d in as.list(seq(as.Date("2022-05-10"), as.Date("2022-12-14"), by = 1))) {
  print(d)
  filename <- paste0("data/scores_by_date/scores_", d, ".csv.gz")

  if (file.exists(filename)) {
    df_temp <- read_csv(filename, show_col_types = FALSE)
    df_temp$eval_date <- d
    df <- bind_rows(df, df_temp)
  }
}

unique(df$model)



a <- df %>%
  filter(
    location == "DE",
    age_group == "00+",
    type == "quantile"
  ) %>%
  group_by(model, eval_date, type) %>%
  summarize(score = mean(score))

ggplot(a, aes(x = eval_date, y = score, color = model)) +
  geom_line()


a <- df %>%
  filter(
    location == "DE",
    age_group != "00+",
    type == "quantile",
    model != "RKI",
    model != "KIT-frozen_baseline"
  ) %>%
  group_by(model, eval_date, type) %>%
  summarize(score = mean(score))

a <- a %>%
  mutate(model = fct_relevel(model, (c(
    "Epiforecasts", "ILM", "KIT",
    "LMU", "RIVM", "RKI", "SU", "SZ", "MeanEnsemble", "MedianEnsemble"
  ))))

MODEL_COLORS <- setNames(
  c("#B30000", "#E69F00", "#56B4E9", "#F0E442", "#009E73", "#60D1B3", "#D55E00", "#CC79A7", "#000000"),
  c("Epiforecasts", "ILM", "KIT", "LMU", "MeanEnsemble", "MedianEnsemble", "RIVM", "SU", "SZ")
)

ggplot(a, aes(x = eval_date, y = score, color = model)) +
  geom_vline(xintercept = as.Date("2022-08-08"), color = "darkgray", linetype = "solid", size = 0.75) +
  annotate(geom = "label", x = as.Date("2022-08-08"), y = 0.1*max(a$score), label = "8 August 2022", size = 4, color = "darkgray", ) +
  geom_line(size = 1) +
  scale_x_date(date_breaks = "2 months", minor_breaks = "1 month", date_labels = "%b %Y") +
  expand_limits(y = 0) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(breaks = c(
    "Epiforecasts", "ILM", "KIT",
    "LMU", "RIVM", "RKI", "SU", "SZ", "MeanEnsemble", "MedianEnsemble"
  ), values = MODEL_COLORS) +
  labs(
    x = "Evaluation date",
    y = "Mean WIS",
    color = "Model",
    title = "Age groups"
  )

ggsave("figures/scores_by_eval_date_age_qs_12-14.pdf", width = 200, height = 100, unit = "mm", device = "pdf")



a <- df %>%
  filter(
    location == "DE",
    age_group == "00+",
    type == "quantile",
    model != "KIT-frozen_baseline",
    target %in% paste(0:7 * -1, "day ahead inc hosp")
  ) %>%
  group_by(model, eval_date, type) %>%
  summarize(score = mean(score))

ggplot(a, aes(x = eval_date, y = score, color = model)) +
  geom_line(size = 1) +
  scale_color_manual(values = MODEL_COLORS) +
  expand_limits(y = 0) +
  theme_bw() +
  labs(
    x = "Evaluation date",
    y = "Mean quantile score",
    color = "Model",
    title = "Age groups (horizon 0-7 days)"
  )

ggsave("figures/scores_age_horizon_0-7d_09-20.png", width = 200, height = 100, unit = "mm", device = "png")




titles <- setNames(
  c("National level", "States", "Age groups"),
  c("national", "states", "age")
)

plot_scores_by_date <- function(df, level = "age", short_horizon = FALSE) {
  title <- titles[level]
  df_temp <- filter_data(df, level = level)

  if (short_horizon) {
    df_temp <- df_temp %>%
      filter(target %in% paste(0:7 * -1, "day ahead inc hosp"))
  }

  df_temp <- df_temp %>%
    group_by(model, eval_date, type) %>%
    summarize(score = mean(score))

  df_temp <- df_temp %>%
    mutate(model = fct_relevel(model, (c(
      "Epiforecasts", "ILM", "KIT",
      "LMU", "RIVM", "RKI", "SU", "SZ", "MeanEnsemble", "MedianEnsemble"
    ))))
  
  ggplot(df_temp, aes(x = eval_date, y = score, color = model)) +
    geom_vline(xintercept = as.Date("2022-08-08"), color = "darkgray", linetype = "solid", size = 0.75) +
    annotate(geom = "label", x = as.Date("2022-08-08"), y = 0.05*max(df_temp$score), label = "8 August 2022", size = 3, color = "darkgray", ) +
    geom_line(size = 1) +
    scale_x_date(date_breaks = "2 months", minor_breaks = "1 month", date_labels = "%b %Y") +
    expand_limits(y = 0) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_color_manual(breaks = c(
      "Epiforecasts", "ILM", "KIT",
      "LMU", "RIVM", "RKI", "SU", "SZ", "MeanEnsemble", "MedianEnsemble"
    ), values = MODEL_COLORS) +
    labs(
      x = "Evaluation date",
      y = "Mean WIS",
      color = "Model",
      title = title
    )
}

p1 <- plot_scores_by_date(df, level = "national", short_horizon = FALSE)
p2 <- plot_scores_by_date(df, level = "states", short_horizon = FALSE)
p3 <- plot_scores_by_date(df, level = "age", short_horizon = FALSE)

p1 + p2 + p3

plot_scores_by_date(df, level = "national", short_horizon = TRUE)
plot_scores_by_date(df, level = "states", short_horizon = TRUE)
plot_scores_by_date(df, level = "age", short_horizon = TRUE)





df1 <- load_truth(as_of = "2022-08-08") %>% 
  mutate(as_of = "2022-08-08")

df2 <- load_truth(as_of = "2022-12-31") %>% 
  mutate(as_of = "2022-12-31")

df3 <- bind_rows(df1, df2)

df3 <- filter_data(df3, level = "national")

df3 <- df3 %>% 
  filter(date <= "2022-04-29",
         date >= "2021-11-22")

p4 <- ggplot(df3, aes(x = date, y = truth, color = as_of)) +
  geom_line(size = 1) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b %Y") +
  scale_color_manual(values = c("black", "darkorange2")) +
  theme_bw() +  
  labs(x = "Reporting date", 
       y = "7-day hospitalization incidence",
       color = "Data version")




(p1 + theme(legend.position = "none") + p2 + p3 + theme(legend.position = "none")  + p4)  + plot_layout(ncol = 2)

ggsave("figures/scores_by_eval_date.pdf", width = 300, height = 200, unit = "mm", device = "pdf")




p1 <- plot_scores_by_date(df, level = "national", short_horizon = TRUE)
p2 <- plot_scores_by_date(df, level = "states", short_horizon = TRUE)
p3 <- plot_scores_by_date(df, level = "age", short_horizon = TRUE)


p1 + theme(legend.position = "none") + p2 + theme(legend.position = "none")  + p3 & theme(aspect.ratio = 1)
ggsave("figures/scores_by_eval_date_7d.pdf", width = 300, height = 100, unit = "mm", device = "pdf")








