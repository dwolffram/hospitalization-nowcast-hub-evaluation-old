source("utils.R")
library(patchwork)

df <- load_data(add_baseline = TRUE, add_median = FALSE, shorten_names = TRUE, fix_data = TRUE,
                add_truth = FALSE, exclude_missing = TRUE)
df <- df %>% 
  filter(model != "ILM")

df_ILM <- read_csv("data/submissions_updated.csv.gz") %>% 
  filter(model == "ILM")

df <- bind_rows(df, df_ILM)

# truth <- load_frozen_truth(lead_time = 40, start_date = "2021-10-01") %>% 
#   rename(truth = frozen_value)
# 
# write_csv(truth, paste0("data/truth_40d.csv.gz"))

truth <- read_csv("data/truth_40d.csv.gz")


### Compute QS, AE and MSE

df_median <- df %>%
  filter(quantile == 0.5) %>%
  mutate(type = "median")
df_scores <- bind_rows(df, df_median)

df_scores <- df_scores %>%
  left_join(truth, by = c("location", "age_group", "target_end_date" = "date"))

df_scores <- df_scores %>%
  rowwise() %>%
  mutate(score = score(value, truth, type, quantile))

df_scores <- df_scores %>%
  select(-c(pathogen, value, truth))

write_csv(df_scores, paste0("data/scores_40d.csv.gz"))

#####


df <- df %>% 
  filter(type == "quantile")

df <- df %>%
  left_join(truth, by = c("location", "age_group", "target_end_date" = "date"))

df_median <- df %>% 
  filter(quantile == 0.5) %>% 
  rename(med = value) %>% 
  select(-c(quantile, pathogen, retrospective, truth))

df <- df %>%
  left_join(df_median)

df_scores <- df %>%
  rowwise() %>%
  mutate(score = score(value, truth, type, quantile),
         spread = score(value, med, type, quantile))

df_scores <- df_scores %>% 
  mutate(overprediction = ifelse(med > truth, score - spread, 0),
         underprediction = ifelse(med < truth, score - spread, 0))




df_national <- filter_data(df_scores, level = "national")

df_national <- df_national %>% 
  group_by(model) %>% 
  summarize(spread = mean(spread),
            overprediction = mean(overprediction),
            underprediction = mean(underprediction),
            score = mean(score))

write_csv(df_national, paste0("data/wis_40d_national.csv.gz"))



df_states <- filter_data(df_scores, level = "states")

df_states <- df_states %>% 
  group_by(model) %>% 
  summarize(spread = mean(spread),
            overprediction = mean(overprediction),
            underprediction = mean(underprediction),
            score = mean(score))

write_csv(df_states, paste0("data/wis_40d_states.csv.gz"))


df_age <- filter_data(df_scores, level = "age")

df_age <- df_age %>% 
  group_by(model) %>% 
  summarize(spread = mean(spread),
            overprediction = mean(overprediction),
            underprediction = mean(underprediction),
            score = mean(score))

write_csv(df_age, paste0("data/wis_40d_age.csv.gz"))




plot_wis <- function(level = "national", add_ae = TRUE, short_horizons = FALSE){
  df <- read_csv(paste0("data/wis_40d_", level, ifelse(short_horizons, "_7d", ""), ".csv.gz"))
  
  df <- df %>% 
    mutate(model = fct_relevel(model, rev(c(
      "Epiforecasts", "ILM", "KIT-frozen_baseline", "KIT",
      "LMU", "RIVM", "RKI", "SU", "SZ", "MeanEnsemble", "MedianEnsemble"
    ))))
  
  base_score <- df %>%
    filter(model == "KIT-frozen_baseline") %>%
    pull(score)
  
  df <- df %>%
    filter(model != "KIT-frozen_baseline",
           model != "RKI")
  
  scores <- df %>% 
    select(-score) %>% 
    pivot_longer(cols = c(underprediction, spread, overprediction), names_to = "penalty")
  
  df_ae <- load_scores(aggregate_scores = TRUE)
  df_ae <- df_ae %>% 
    mutate(model = fct_relevel(model, rev(c(
      "Epiforecasts", "ILM", "KIT-frozen_baseline", "KIT",
      "LMU", "RIVM", "RKI", "SU", "SZ", "MeanEnsemble", "MedianEnsemble"
    ))))
  df_ae <- df_ae %>%
    filter(target %in% paste(0:7*-1, "day ahead inc hosp"))
  df_ae <- filter_scores(df_ae, "median", level, by_horizon = FALSE) %>%
    filter(model != "KIT-frozen_baseline")
  
  
  ggplot() +
    {if (add_ae) geom_point(data = df_ae, aes(x = model, y = score, fill = model), shape = 23)} +
    geom_bar(data = df, aes(x = model, y = score), fill = "white", stat = "identity") + # so you can't see through bars
    geom_bar(data = scores, aes(x = model, y = value, fill = model, alpha = penalty, color = model), size = 0.1, stat = "identity") +
    # geom_label(data = df, aes(x = model, y = 0.1*base_score, label = sprintf("%0.1f", round(score, digits = 1))),
    #            fill = "white", alpha = 0.7, hjust = 1,
    #            label.size = NA, label.r = unit(0, "pt"), size = 9 * 5 / 14,
    #            label.padding = unit(0.1, "lines"),) +
    geom_label(data = df, aes(x = model, y = 0.5*score, label = sprintf("%0.1f", round(score, digits = 1))),
               fill = "white", alpha = 1, hjust = 0.5,
               label.r = unit(0.25, "lines"), size = 9 * 5 / 14,
               label.padding = unit(0.15, "lines")) +
    scale_fill_manual(values = MODEL_COLORS, guide = "none") +
    scale_color_manual(values = MODEL_COLORS, guide = "none") +
    scale_alpha_manual(values = c(0.5, 0.2, 1), labels = c("Overprediction", "Spread", "Underprediction"),
                       guide = guide_legend(reverse = TRUE, title.position="top", title.hjust = 0.5)) +
    scale_y_continuous(name = "Mean WIS / AE", 
                       sec.axis = sec_axis(trans = ~ . / base_score, 
                                           name = paste("Relative", "WIS"))) +
    labs(
      y = "Mean WIS/AE",
      x = NULL,
      color = "Model",
      alpha = "Decomposition of WIS"
    ) + 
    coord_flip() +
    theme_bw() +
    theme(legend.position = "bottom",
          legend.key.height= unit(0.3, 'cm'),
          legend.key.width= unit(0.3, 'cm'),
          legend.text = element_text(size = 7.5))
}


plot_wis <- function(level = "national", add_ae = TRUE) {
  df <- read_csv(paste0("data/wis_40d_", level, ".csv.gz"))
  
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
    df_ae <- read_csv("data/scores_40d.csv.gz")
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
      if (add_ae) geom_point(data = df_ae, aes(x = model, y = score, fill = model), shape = 23)
    } +
    geom_bar(data = df, aes(x = model, y = score), fill = "white", stat = "identity") + # so you can't see through bars
    geom_bar(data = scores, aes(x = model, y = value, fill = model, alpha = penalty, color = model), size = 0.1, stat = "identity") +
    geom_label(
      data = df, aes(x = model, y = 0.5 * score, label = sprintf("%0.1f", round(score, digits = 1))),
      fill = "white", alpha = 1, hjust = 0.5,
      label.r = unit(0.25, "lines"), size = 9 * 5 / 14,
      label.padding = unit(0.15, "lines")
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
      legend.position = "bottom",
      legend.key.height = unit(0.3, "cm"),
      legend.key.width = unit(0.3, "cm"),
      legend.text = element_text(size = 7.5)
    )
}


plot_wis("national", add_ae = TRUE)

p1 <- plot_wis("national") + theme(legend.position = "none") + labs(title = "National level")
p2 <- plot_wis("states") + theme(legend.position = "none") + labs(title = "States")
p3 <- plot_wis("age") + theme(legend.position = "right", legend.justification = "left") + labs(title = "Age groups")

df <- df %>% 
  filter(model != "KIT-frozen_baseline")

p4 <- plot_coverage_all(df, "national") + theme(legend.position = "none")
p5 <- plot_coverage_all(df, "states") + theme(legend.position = "none")
p6 <- plot_coverage_all(df, "age")  + theme(legend.position = "right", legend.justification = "left") 

(p1 + p2 + p3) /
  (p4 + p5 + p6) & theme(plot.title = element_text(hjust = 0.5), aspect.ratio = 1)

ggsave("figures/scores_40d.pdf", width = 350, height = 200, unit = "mm", device = "pdf")



#### 0-7 days
# 
# df7 <- df_scores %>% 
#   filter(target %in% paste(0:7*-1, "day ahead inc hosp"))
# 
# df_national <- filter_data(df7, level = "national")
# 
# df_national <- df_national %>% 
#   group_by(model) %>% 
#   summarize(spread = mean(spread),
#             overprediction = mean(overprediction),
#             underprediction = mean(underprediction),
#             score = mean(score))
# 
# write_csv(df_national, paste0("data/wis_national_7d.csv.gz"))
# 
# 
# 
# df_states <- filter_data(df7, level = "states")
# 
# df_states <- df_states %>% 
#   group_by(model) %>% 
#   summarize(spread = mean(spread),
#             overprediction = mean(overprediction),
#             underprediction = mean(underprediction),
#             score = mean(score))
# 
# write_csv(df_states, paste0("data/wis_states_7d.csv.gz"))
# 
# 
# df_age <- filter_data(df7, level = "age")
# 
# df_age <- df_age %>% 
#   group_by(model) %>% 
#   summarize(spread = mean(spread),
#             overprediction = mean(overprediction),
#             underprediction = mean(underprediction),
#             score = mean(score))
# 
# write_csv(df_age, paste0("data/wis_age_7d.csv.gz"))
