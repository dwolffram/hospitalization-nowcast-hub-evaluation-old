source("R/load_data.R")

dates <- c("2021-12-01", "2022-02-01", "2022-04-01", "2022-06-01")

cbPalette <- rev(c("#000000", "#E69F00", "#56B4E9", "#009E73", "#0072B2", "#D55E00", "#CC79A7"))


dfs <- data.frame()
for (d in dates) {
  df_temp <- load_truth(location = "DE", age_group = "00+", as_of = d)
  df_temp$as_of <- d
  dfs <- bind_rows(dfs, df_temp)
}

dfs1 <- data.frame()
for (d in dates[-4]) {
  df_temp <- load_nowcast("NowcastHub-MeanEnsemble", d, "DE", "00+")
  df_temp$as_of <- d
  dfs1 <- bind_rows(dfs1, df_temp)
}


alphas <- setNames(c(0.7, 0.4), c("50%", "95%"))

dfs %>%
  filter(date >= "2021-07-01", as_of %in% c(dates, "2022-05-01")) %>%
  ggplot() +
  geom_vline(xintercept = as.Date(dates[-4]), size = 1.5, linetype = "dashed", color = "black") +
  # geom_label(aes(x = as.Date(dates[1]), y = 12500, label = dates[1])) +
  geom_line(aes(x = date, y = value_7d, color = as_of), size = 1.5) +
  geom_ribbon(data = dfs1, 
              aes(x = target_end_date, ymin = quantile_0.025, ymax = quantile_0.975, 
                  group = as_of, alpha = "95%"), 
              fill = "skyblue3") +
  geom_ribbon(data = dfs1, 
              aes(x = target_end_date, ymin = quantile_0.25, ymax = quantile_0.75, 
                  group = as_of, alpha = "50%"), 
              fill = "skyblue3") +
  geom_line(data = dfs1, aes(x = target_end_date, y = quantile_0.5, group = as_of), 
            color = "skyblue3", size = 1.5, linetype = "solid") +
  scale_x_date(date_breaks = "2 months", date_labels = "%b %Y", expand = c(0.02, 0)) +
  scale_color_manual(values = c("#009E73", "#E69F00", "#D55E00","#000000")) +
  theme_gray()  +   
  theme(text = element_text(size = 32)) +   
  labs(x = NULL, 
       y = "7-day hospitalization incidence",
       color = "Data as of") +
  scale_alpha_manual(name = "Nowcasts with \nprediction intervals:", values = alphas,
                     guide = guide_legend(order = 2, title.position = "top", title.hjust = 0)) +
  theme(legend.position = c(0.3, 0.05), legend.justification = c(0, 0), legend.box.just = "center", 
        legend.direction = "horizontal")

+
  guides(color = "none")

ggsave("figures/nowcast_example_1.pdf", width = 350, height = 200, unit = "mm", device = "pdf")


dfs %>%
  filter(date >= "2021-07-01", as_of %in% c(dates, "2022-05-01")) %>%
  ggplot() +
  geom_vline(xintercept = as.Date(dates[-4]), size = 1.5, linetype = "dashed", color = "black") +
  # geom_label(aes(x = as.Date(dates[1]), y = 12500, label = dates[1])) +
  geom_line(aes(x = date, y = value_7d, color = as_of), size = 1.5) +
  geom_ribbon(data = dfs1, 
              aes(x = target_end_date, ymin = quantile_0.025, ymax = quantile_0.975, 
                  group = as_of, alpha = "95%"), 
              fill = "skyblue3") +
  geom_ribbon(data = dfs1, 
              aes(x = target_end_date, ymin = quantile_0.25, ymax = quantile_0.75, 
                  group = as_of, alpha = "50%"), 
              fill = "skyblue3") +
  geom_line(data = dfs1, aes(x = target_end_date, y = quantile_0.5, group = as_of), 
            color = "skyblue3", size = 1.5, linetype = "solid") +
  scale_x_date(date_breaks = "2 months", date_labels = "%b %Y", expand = c(0.02, 0)) +
  scale_color_manual(values = c("#009E73", "#E69F00", "#D55E00","#000000"), guide = guide_legend(order = 1)) +
  theme_gray()  +   
  theme(text = element_text(size = 32),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 18),
        axis.text = element_text(size = 18)) +   
  labs(x = NULL, 
       y = "7-day hospitalization incidence",
       color = "Data as of") +
  scale_alpha_manual(name = "Nowcasts with \nprediction intervals:", values = alphas) +
  theme(legend.position = c(0.05, 0.95), legend.justification = c(0, 1), legend.box.just = "left")

ggsave("figures/nowcast_example_1.pdf", width = 300, height = 200, unit = "mm", device = "pdf")


#### For presentation

for(i in 1:4) {
  dfs %>%
    filter(date >= "2021-07-01", as_of %in% dates[1:i]) %>%
    ggplot() +
    # geom_vline(xintercept = as.Date(dates[-4]), size = 1.5, linetype = "dashed", color = "black") +
    # geom_label(aes(x = as.Date(dates[1]), y = 12500, label = dates[1])) +
    geom_line(aes(x = date, y = value_7d, color = as_of), size = 1.5) +
    scale_x_date(date_breaks = "2 months", date_labels = "%b %Y") +
    scale_color_manual(values = c("#009E73", "#E69F00", "#D55E00","#000000"), guide = guide_legend(order = 1)) +
    theme_gray()  +   
    theme(text = element_text(size = 32),
          legend.title = element_text(size = 18),
          legend.text = element_text(size = 18),
          axis.text = element_text(size = 18)) +   
    labs(x = NULL, 
         y = "7-day hospitalization incidence",
         color = "Data as of") +
    theme(legend.position = c(0.05, 0.95), legend.justification = c(0, 1), legend.box.just = "left") +
    expand_limits(x = as.Date("2022-06-01"), y = 12750)
  
  ggsave(paste0("figures/nowcast_example_", i, ".pdf"), width = 300, height = 200, unit = "mm", device = "pdf")
}

dfs %>%
  filter(date >= "2021-07-01", as_of %in% dates[1:i]) %>%
  ggplot() +
  # geom_vline(xintercept = as.Date(dates[-4]), size = 1.5, linetype = "dashed", color = "black") +
  # geom_label(aes(x = as.Date(dates[1]), y = 12500, label = dates[1])) +
  geom_line(aes(x = date, y = value_7d, color = as_of), size = 1.5) +
  geom_ribbon(data = dfs1, 
              aes(x = target_end_date, ymin = quantile_0.025, ymax = quantile_0.975, 
                  group = as_of, alpha = "95%"), 
              fill = "skyblue3") +
  geom_ribbon(data = dfs1, 
              aes(x = target_end_date, ymin = quantile_0.25, ymax = quantile_0.75, 
                  group = as_of, alpha = "50%"), 
              fill = "skyblue3") +
  geom_line(data = dfs1, aes(x = target_end_date, y = quantile_0.5, group = as_of), 
            color = "skyblue3", size = 1.5, linetype = "solid") +
  scale_x_date(date_breaks = "2 months", date_labels = "%b %Y") +
  scale_color_manual(values = c("#009E73", "#E69F00", "#D55E00","#000000"), guide = guide_legend(order = 1)) +
  theme_gray()  +   
  theme(text = element_text(size = 32),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 18),
        axis.text = element_text(size = 18)) +   
  labs(x = NULL, 
       y = "7-day hospitalization incidence",
       color = "Data as of") +
  scale_alpha_manual(name = "Nowcasts with \nprediction intervals:", values = alphas) +
  theme(legend.position = c(0.05, 0.95), legend.justification = c(0, 1), legend.box.just = "left") +
  expand_limits(x = as.Date("2022-06-01"), y = 12750)

ggsave(paste0("figures/nowcast_example_", 5, ".pdf"), width = 300, height = 200, unit = "mm", device = "pdf")

