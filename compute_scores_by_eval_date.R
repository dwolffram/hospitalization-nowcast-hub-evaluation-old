source("load_truth.R")
source("utils.R")

load_submissions <- function(start_date = "2021-11-22", end_date = "2022-04-29"){
  df <- read_csv(paste0("data/submissions_", start_date, "_", end_date, ".csv.gz"))
  
  # Add baseline
  df_baseline <- read_csv(paste0("data/submissions_KIT-frozen_baseline.csv.gz")) %>%
    mutate(retrospective = FALSE)
  df <- bind_rows(df, df_baseline)
  
  df$model <- factor(df$model,
                     levels = sort(unique(df$model)),
                     labels = SHORT_NAMES
  )
  
  # replace erroneus nowcasts
  df <- fix_rki_errors(df)
  
  # Add median separately
  df_median <- df %>%
    filter(quantile == 0.5) %>%
    mutate(type = "median")
  df <- bind_rows(df, df_median)
}

compute_scores <- function(df, eval_date = "2022-08-08"){
  df_truth <- load_truth(as_of = eval_date)
  
  df <- df %>%
    left_join(df_truth, by = c("location", "age_group", "target_end_date" = "date"))
  
  df <- df %>%
    rowwise() %>%
    mutate(score = score(value, truth, type, quantile))
  
  # Aggregate scores
  df <- df %>%
    group_by(model, location, age_group, target, type) %>%
    summarize(score = mean(score))
  
  write_csv(df, paste0("data/scores_", eval_date, ".csv.gz"))
  return(df)
}

df <- load_submissions()

df <- load_data(add_baseline = FALSE, add_median = FALSE, shorten_names = TRUE, fix_data = TRUE,
                add_truth = FALSE, exclude_missing = TRUE)

df <- df %>% 
  filter(type == "quantile")

df_scores <- compute_scores(df)

as.Date(eval_date) - as.Date(end_date)
as.Date(end_date) + 100

compute_scores(df, as.Date(end_date) + 11)

for (d in as.list(seq(as.Date("2022-05-10"), as.Date("2022-08-08"), by=1))) {
  print(d)
  compute_scores(df, d)
}

for (d in as.list(seq(as.Date("2022-11-17"), as.Date("2022-12-20"), by=1))) {
  print(d)
  compute_scores(df, d)
}

# 11-06, 11-07

for (d in as.list(seq(as.Date("2022-09-21"), as.Date("2022-10-17"), by=1))) {
  print(d)
  compute_scores(df, d)
}


for (d in as.Date(end_date) + 1 + 1:10*10) {
  compute_scores(df, as.Date(d))
}

s <- df %>%
  filter(
    location == "DE",
    age_group == "00+"
  ) %>%
  group_by(model, type) %>%
  summarize(score = mean(score))

