library(patchwork)
source("utils.R")

df <- data.frame()

for (d in as.list(seq(as.Date("2022-05-10"), as.Date("2022-12-14"), by = 1))) {
  print(d)
  filename <- paste0("data/scores_by_date/scores_", d, ".csv.gz")
  
  if (file.exists(filename)) {
    df_temp <- read_csv(filename, show_col_types = FALSE)
    df_temp$eval_date <- d
    df <- bind_rows(df, df_temp)
  }
}


TITLES <- setNames(
  c("National level", "States", "Age groups"),
  c("national", "states", "age")
)

plot_scores_by_eval_date <- function(df, level = "national") {
  title <- TITLES[level]
  
  df_temp <- df %>% 
    filter(level == !!level)
  
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


p1 <- plot_scores_by_eval_date(df, "national")
p2 <- plot_scores_by_eval_date(df, "states")
p3 <- plot_scores_by_eval_date(df, "age")


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
