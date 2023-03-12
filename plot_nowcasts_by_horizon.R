library(tidyverse)
source("utils.R")

LEAD_TIME <- 0

df <- load_data(add_baseline = TRUE, add_median = FALSE, shorten_names = TRUE, fix_data = TRUE,
                add_truth = TRUE, exclude_missing = FALSE, eval_date = "2022-08-08")

truth_frozen <- load_frozen_truth(LEAD_TIME, "2021-11-01")

if (LEAD_TIME == 0){
  t <- "0 day ahead inc hosp"
} else {
  t <- paste0("-", LEAD_TIME, " day ahead inc hosp")
}

df1 <- df %>%
  filter(
    #model %in% c("NowcastHub-MeanEnsemble", "ILM-prop"),
    target == t,
    type == "quantile",
    location == "DE",
    age_group == "00+"
  ) %>%
  pivot_wider(names_from = quantile, names_prefix = "quantile_")


df1 <- df1 %>%
  #left_join(df_truth, by = c("target_end_date" = "date", "location", "age_group")) %>%
  left_join(truth_frozen, by = c("target_end_date" = "date", "location", "age_group")) %>%
  drop_na(frozen_value)

df1$age_group <- factor(df1$age_group, levels = c("00+", "00-04", "05-14", "15-34", "35-59", "60-79", "80+"))


alphas <- setNames(c(0.7, 0.4), c("50%", "95%"))
line_colors <- setNames(c("firebrick3", "gray"), c("Final", "At time of nowcast"))

df1 <- df1 %>% 
  mutate(model = fct_relevel(model, c(
    "Epiforecasts", "ILM", "KIT-frozen_baseline", "KIT",
    "LMU",
    "RIVM", "RKI", "SU", "SZ", "MeanEnsemble", "MedianEnsemble"
  )))

ggplot(df1) +
  facet_wrap("model", scales = "fixed", ncol = 3) +
  geom_line(aes(x = target_end_date, y = frozen_value, color = "At time of nowcast"), linetype = "solid") +
  geom_ribbon(aes(x = target_end_date, ymin = quantile_0.025, ymax = quantile_0.975, alpha = "95%"),
              fill = "skyblue3"
  ) +
  geom_ribbon(aes(x = target_end_date, ymin = quantile_0.25, ymax = quantile_0.75, alpha = "50%"),
              fill = "skyblue3"
  ) +
  geom_line(aes(x = target_end_date, y = quantile_0.5),
            color = "royalblue4", size = 0.25, linetype = "solid"
  ) +
  geom_line(aes(x = target_end_date, y = truth, color = "Final")) +
  
  labs(x = NULL, y = "7-day hospitalization incidence", title = paste("Horizon:", LEAD_TIME, "days (national level, all age groups)"))+
  scale_alpha_manual(name = "Nowcasts with \nprediction intervals:", values = alphas,
                     guide = guide_legend(order = 2, title.position = "top", title.hjust = 0)) +
  scale_color_manual(name = "Truth", values = line_colors,
                     guide = guide_legend(order = 1, title.position = "top", title.hjust = 0)) +
  # expand_limits(y = 0) +
  theme_bw() +
  coord_cartesian(ylim = c(NA, 17500))

ggsave(paste0("figures/nowcasts_", LEAD_TIME, "d.pdf"), width = 300, height = 200, unit = "mm", device = "pdf")
