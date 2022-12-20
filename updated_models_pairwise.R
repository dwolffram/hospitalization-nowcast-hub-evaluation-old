library(tidyverse)
library(patchwork)
source("utils.R")


filter_data <- function(df, model, type = "quantile", level = "national") {
  
  if (!missing(model)){
    df <- df %>%
      filter(model == !!model)
  }
  
  if (!missing(type)){
    df <- df %>%
      filter(type == !!type)
  }
  
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
  return(df)
}


plot_scores_updated <- function(df, type, level){
  df_temp <- filter_data(df, type = type, level = level)
  
  df_temp <- df_temp %>% 
    mutate(model = ifelse(updated, paste0(model, " (updated)"), model))
  
  df_temp <- df_temp %>%
    mutate(horizon = as.numeric(str_extract(target, "-?\\d+"))) %>%
    arrange(model, location, age_group, horizon)
  
  df_temp <- df_temp %>%
    group_by(model, target, horizon, updated) %>%
    summarize(score = mean(score), .groups = "drop") %>%
    arrange(model, horizon)
  
  ggplot(df_temp, aes(x = horizon, y = score, color = model)) +
    geom_line(size = 1) +
    expand_limits(y = 0) +
    scale_x_continuous(breaks = 0:5*-5,
                       minor_breaks = -28:0) +
    scale_color_manual(values = COLORS_UPDATED) +
    labs(
      x = "Horizon (days)",
      y = "Mean WIS",
      color = "Model"
    ) +
    theme_bw()
}


plot_coverage_lines <- function(df, level) {
  TITLES <- setNames(
    c("National level", "States", "Age groups"),
    c("national", "states", "age")
  )
  
  df <- filter_data(df, type = "quantile", level = level) %>% 
    select(-score)
  
  df <- df %>% 
    mutate(model = ifelse(updated, paste0(model, " (updated)"), model))
  
  df <- df %>%
    mutate(horizon = as.numeric(str_extract(target, "-?\\d+"))) %>%
    arrange(model, location, age_group, horizon)
  
  df_wide <- df %>%
    pivot_wider(names_from = quantile, values_from = value, names_prefix = "quantile_")
  
  df_wide <- df_wide %>%
    mutate(
      c50 = (truth >= quantile_0.25 & truth <= quantile_0.75),
      c95 = (truth >= quantile_0.025 & truth <= quantile_0.975)
    )
  
  coverage_df <- df_wide %>%
    group_by(model, updated, horizon) %>%
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
    # scale_color_manual(values = MODEL_COLORS, limits = force) +
    geom_hline(data = nominal_levels, aes(yintercept = quantile_level), linetype = "dashed") +
    scale_x_continuous(breaks = 0:5*-5,
                       minor_breaks = -28:0) +
    scale_color_manual(values = COLORS_UPDATED) +
    labs(
      x = "Horizon (days)",
      y = "Empirical coverage",
      color = "Model"#,
      #title = TITLES[level]
    ) #+
    #theme(plot.title = element_text(hjust = 0.5))
}


COLORS_UPDATED <- setNames(
  c("#56B4E9", "#3c7da3", "#F0E442", "#a89f2e"),
  c("KIT", "KIT (updated)", "LMU", "LMU (updated)")
)


df <- read_csv("data/scores_updated_aggregated.csv.gz") %>% 
  mutate(model = str_sub(model, 1, 3)) %>% 
  filter(model != "ILM")

df2 <- read_csv("data/scores_updated.csv.gz") %>% 
  mutate(model = str_sub(model, 1, 3)) %>% 
  filter(model != "ILM")

plot_scores_updated(df, "quantile", "national")

plot_coverage_lines(df2, "national")





p1 <- plot_scores_updated(df, "quantile", "national") + 
  ggtitle("National level") + theme(plot.title = element_text(hjust = 0.5), legend.position = "none")
p2 <- plot_scores_updated(df, "quantile", "states") + 
  ggtitle("States") + theme(plot.title = element_text(hjust = 0.5), legend.position = "none")
p3 <- plot_scores_updated(df, "quantile", "age") + theme(legend.position = "none") + 
  ggtitle("Age groups") + theme(plot.title = element_text(hjust = 0.5), legend.position = "right") 

p4 <- plot_coverage_lines(df2, "national") + theme(legend.position = "none")
p5 <- plot_coverage_lines(df2, "states") + theme(legend.position = "none")
p6 <- plot_coverage_lines(df2, "age") + theme(legend.position = "none")

# (p1 + p2 + p3) /
#   (p4 + p5 + p6) 
# 
# (p1 + p2 + p3) /
#   (p4 + p5 + p6 & theme(aspect.ratio = 1)) 
# 
# ((p1 + p2 + p3) /
#   (p4 + p5 + p6) & theme(aspect.ratio = 1)) + 
#   plot_layout(heights = c(2, 1))

(p1 + p2 + p3) /
  (p4 + p5 + p6) + 
  plot_layout(heights = c(1.5, 1))


ggsave("figures/updated_models.pdf", width = 350, height = 150, unit = "mm", device = "pdf")
