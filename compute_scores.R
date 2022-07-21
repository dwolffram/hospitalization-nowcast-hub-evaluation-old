source("load_truth.R")

# simple implementation of quantile score
qs <- function(q, y, alpha) {
  (as.numeric(y < q) - alpha) * (q - y)
}

score <- function(prediction, observation, type, quantile) {
  if (type == "mean") {
    return(abs(prediction - observation))
  } else if (type == "quantile") {
    return(qs(prediction, observation, quantile))
  }
}

END_DATE <- "2022-04-29"
EVAL_DATE <- "2022-07-17"

df <- read_csv(paste0("data/submissions_", EVAL_DATE, ".csv.gz")) %>% 
  filter(forecast_date <= EVAL_DATE)


df <- read_csv(paste0("data/submissions_", END_DATE, ".csv.gz"))
df_truth <- load_truth(as_of = EVAL_DATE)

df <- df %>%
  left_join(df_truth, by = c("location", "age_group", "target_end_date" = "date"))

df <- df %>%
  rowwise() %>%
  mutate(score = score(value, truth, type, quantile))

write_csv(df, paste0("data/scores_", END_DATE, "_", EVAL_DATE, ".csv.gz"))
