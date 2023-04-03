source("utils.R")
library(patchwork)

df <- load_data(add_baseline = TRUE, add_median = TRUE, shorten_names = TRUE, fix_data = TRUE,
                add_truth = TRUE, exclude_missing = TRUE, eval_date = "2022-08-08")

df_population <- read_csv("../hospitalization-nowcast-hub/nowcast_viz_de/plot_data/population_sizes.csv")  %>% 
  select(-"...1")


df <- df %>% 
  left_join(df_population)

df <- df %>% 
  mutate(value = value*100000/population,
         truth = truth*100000/population)

df <- df %>%
  rowwise() %>%
  mutate(score = score(value, truth, type, quantile))

df <- df %>%
  select(-c(pathogen, value, truth))

write_csv(df, paste0("data/scores_100k.csv.gz"))



### WIS decomposition

df <- load_data(add_baseline = TRUE, add_median = TRUE, shorten_names = TRUE, fix_data = TRUE,
                add_truth = TRUE, exclude_missing = TRUE, eval_date = "2022-08-08")

df_population <- read_csv("../hospitalization-nowcast-hub/nowcast_viz_de/plot_data/population_sizes.csv")  %>% 
  select(-"...1")

df <- df %>% 
  left_join(df_population)

df <- df %>% 
  mutate(value = value*100000/population,
         truth = truth*100000/population)

df <- df %>% 
  filter(type == "quantile")

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

write_csv(df_national, paste0("data/wis_national_100k.csv.gz"))



df_states <- filter_data(df_scores, level = "states")

df_states <- df_states %>% 
  group_by(model) %>% 
  summarize(spread = mean(spread),
            overprediction = mean(overprediction),
            underprediction = mean(underprediction),
            score = mean(score))

write_csv(df_states, paste0("data/wis_states_100k.csv.gz"))


df_age <- filter_data(df_scores, level = "age")

df_age <- df_age %>% 
  group_by(model) %>% 
  summarize(spread = mean(spread),
            overprediction = mean(overprediction),
            underprediction = mean(underprediction),
            score = mean(score))

write_csv(df_age, paste0("data/wis_age_100k.csv.gz"))


plot_wis("age", per_100k = TRUE, add_ae = TRUE)


p1 <- plot_wis("national", per_100k = TRUE)
p3 <- plot_wis("states", per_100k = TRUE)
p5 <- plot_wis("age", per_100k = TRUE)

p2 <- plot_scores("quantile", "national", by_horizon = TRUE, per_100k = TRUE)
p2b <- plot_scores("quantile", "national", by_horizon = TRUE, relative = TRUE, per_100k = TRUE)

p4 <- plot_scores("quantile", "states", by_horizon = TRUE, per_100k = TRUE)
p4b <- plot_scores("quantile", "states", by_horizon = TRUE, relative = TRUE, per_100k = TRUE)

p6 <- plot_scores("quantile", "age", by_horizon = TRUE, per_100k = TRUE)
p6b <- plot_scores("quantile", "age", by_horizon = TRUE, relative = TRUE, per_100k = TRUE)


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

ggsave("figures/scores_wis_100k.pdf", width = 164, height = 200, unit = "mm", device = "pdf")
