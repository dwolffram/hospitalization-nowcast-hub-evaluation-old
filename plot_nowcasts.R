library(tidyverse)
source("load_truth.R")

Sys.setlocale("LC_ALL", "C")

LEAD_TIME <- 14

df <- read_csv("data/submissions.csv.gz")

df_truth <- load_truth(as_of = "2022-06-01")
truth_frozen <- load_frozen_truth(LEAD_TIME, "2021-11-01")

if (LEAD_TIME == 0){
  t <- "0 day ahead inc hosp"
} else {
  t <- paste0("-", LEAD_TIME, " day ahead inc hosp")
}

df1 <- df %>%
  filter(
    model == "NowcastHub-MeanEnsemble",
    target == t,
    type == "quantile",
    location == "DE"
  ) %>%
  pivot_wider(names_from = quantile, names_prefix = "quantile_")


df1 <- df1 %>%
  left_join(df_truth, by = c("target_end_date" = "date", "location", "age_group")) %>%
  left_join(truth_frozen, by = c("target_end_date" = "date", "location", "age_group")) %>%
  drop_na()

df1$age_group <- factor(df1$age_group, levels = c("00+", "00-04", "05-14", "15-34", "35-59", "60-79", "80+"))


# ggplot(df1) +
#   facet_wrap("age_group", scales = "free_y") +
#   geom_ribbon(aes(x = target_end_date, ymin = quantile_0.025, ymax = quantile_0.975),
#     fill = "skyblue3", alpha = 0.4
#   ) +
#   geom_ribbon(aes(x = target_end_date, ymin = quantile_0.25, ymax = quantile_0.75),
#     fill = "skyblue3", alpha = 0.5
#   ) +
#   geom_line(aes(x = target_end_date, y = quantile_0.5),
#     color = "skyblue3", linetype = "solid"
#   ) +
#   geom_line(aes(x = target_end_date, y = truth), color = "firebrick3") +
#   geom_line(aes(x = target_end_date, y = frozen_value), linetype = "dashed") +
#   labs(x = NULL, y = "7-day hospitalization incidence")



alphas <- setNames(c(0.7, 0.4), c("50%", "95%"))
line_colors <- setNames(c("firebrick3", "gray"), c("Latest", "Frozen"))

ggplot(df1) +
  facet_wrap("age_group", scales = "free_y", ncol = 3) +
  geom_ribbon(aes(x = target_end_date, ymin = quantile_0.025, ymax = quantile_0.975, alpha = "95%"),
              fill = "skyblue3"
  ) +
  geom_ribbon(aes(x = target_end_date, ymin = quantile_0.25, ymax = quantile_0.75, alpha = "50%"),
              fill = "skyblue3"
  ) +
  geom_line(aes(x = target_end_date, y = quantile_0.5),
            color = "skyblue3", linetype = "solid"
  ) +
  geom_line(aes(x = target_end_date, y = truth, color = "Latest")) +
  geom_line(aes(x = target_end_date, y = frozen_value, color = "Frozen"), linetype = "solid") +
  labs(x = NULL, y = "7-day hospitalization incidence", title = paste("Horizon:", LEAD_TIME, "days"))+
  scale_alpha_manual(name = "Nowcasts with \nprediction intervals:", values = alphas,
                     guide = guide_legend(order = 2, title.position = "top", title.hjust = 0)) +
  scale_color_manual(name = "Truth", values = line_colors,
                     guide = guide_legend(order = 1, title.position = "top", title.hjust = 0)) +
  theme(legend.position = c(0.35, 0.25), legend.justification = c(0, 1), legend.box.just = "left", 
        legend.direction = "vertical", legend.box = "horizontal")

ggsave("figures/ensemble_age_14days.pdf", width = 300, height = 200, unit = "mm", device = "pdf")



df2 <- df %>%
  filter(
    model == "NowcastHub-MeanEnsemble",
    target %in% c("0 day ahead inc hosp", paste0("-", c(2, 7, 14, 28), " day ahead inc hosp")),
    type == "quantile",
    location == "DE"
  ) %>%
  pivot_wider(names_from = quantile, names_prefix = "quantile_")

df2 <- df2 %>%
  left_join(df_truth, by = c("target_end_date" = "date", "location", "age_group")) %>%
  drop_na()

df2$age_group <- factor(df2$age_group, levels = c("00+", "00-04", "05-14", "15-34", "35-59", "60-79", "80+"))

ggplot(df2) +
  facet_wrap("age_group", scales = "free_y") +
  geom_line(aes(x = target_end_date, y = quantile_0.5, color = target), linetype = "solid"
  ) +
  geom_line(aes(x = target_end_date, y = truth), color = "firebrick3") +
  labs(x = NULL, y = "7-day hospitalization incidence")




df3 <- df %>%
  filter(
    model == "NowcastHub-MeanEnsemble",
    type == "quantile",
    location == "DE"
  ) %>%
  pivot_wider(names_from = quantile, names_prefix = "quantile_")

df3 <- df3 %>%
  left_join(df_truth, by = c("target_end_date" = "date", "location", "age_group")) %>%
  drop_na()

df3 <- df3 %>% 
  filter(target_end_date >= "2021-11-23")

df3 <- df3 %>% 
  mutate(error = truth - quantile_0.5,
         abs_error = abs(truth - quantile_0.5))

df3 <- df3 %>% 
  separate(target, sep = " ", into = c("horizon"), remove = FALSE, extra = "drop") %>% 
  mutate(horizon = as.factor(abs(as.numeric(horizon))))

df3 <- df3 %>% 
  filter(horizon %in% c(0, 2, 7, 14, 28))

ggplot(df3, aes(x = horizon, y = error, fill = horizon)) +
  facet_wrap("age_group", scales = "free_y") +
  geom_boxplot(outlier.shape = NA, width = 0.4) +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  theme(legend.position = "none") +
  labs(x = "Horizon", y = "Error of median nowcast")

ggplot(df3, aes(x = horizon, y = abs_error, fill = horizon)) +
  facet_wrap("age_group", scales = "free_y") +
  geom_boxplot(outlier.shape = NA, width = 0.4) +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  theme(legend.position = "none") +
  labs(x = "Horizon", y = "Absolute error of median nowcast")
  # scale_fill_brewer(palette="Set3")

ggsave("figures/absolute_error_boxplots_age.pdf", width = 300, height = 200, unit = "mm", device = "pdf")

