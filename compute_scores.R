source("load_truth.R")
source("utils.R")

# Quantile score
qs <- function(q, y, alpha) {
  (as.numeric(y < q) - alpha) * (q - y)
}

score <- function(prediction, observation, type, quantile) {
  if (type == "mean") {
    return((prediction - observation)^2)
  } else if (type == "median") {
    return(abs(prediction - observation))
  } else if (type == "quantile") {
    return(qs(prediction, observation, quantile))
  }
}

START_DATE <- "2021-11-22" # first submission
END_DATE <- "2022-04-29" # last submission
EVAL_DATE <- "2022-08-08" # date of truth data

df <- read_csv(paste0("data/submissions_", START_DATE, "_", END_DATE, ".csv.gz"))

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

df_truth <- load_truth(as_of = EVAL_DATE)

df <- df %>%
  left_join(df_truth, by = c("location", "age_group", "target_end_date" = "date"))

df <- df %>%
  rowwise() %>%
  mutate(score = score(value, truth, type, quantile))

# d <- df %>%
#   group_by(model, location, age_group, target_end_date, forecast_date, target, type, retrospective) %>%
#   summarize(score = mean(score))
# 
# write_csv(d, paste0("data/scores_retro.csv.gz"))

df <- df %>%
  select(-c(pathogen, retrospective))

scores_baseline <- df %>%
  filter(model == "KIT-frozen_baseline")

write_csv(scores_baseline, paste0("data/scores_", START_DATE, "_", END_DATE, "_baseline.csv.gz"))



df <- df %>%
  filter(model != "KIT-frozen_baseline")

write_csv(df, paste0("data/scores_", START_DATE, "_", END_DATE, ".csv.gz"))

# Aggregate scores
df <- bind_rows(df, scores_baseline)
df <- df %>%
  group_by(model, location, age_group, target, type) %>%
  summarize(score = mean(score))

write_csv(df, paste0("data/scores_", START_DATE, "_", END_DATE, "_aggregated.csv.gz"))


s <- df %>%
  filter(
    location == "DE",
    age_group == "00+"
  ) %>%
  group_by(model, type) %>%
  summarize(score = mean(score))

### FROZEN BASELINE

# df <- read_csv(paste0("data/submissions_KIT-frozen_baseline.csv.gz"))
# df_truth <- load_truth(as_of = EVAL_DATE)
#
# df <- df %>%
#   left_join(df_truth, by = c("location", "age_group", "target_end_date" = "date"))
#
# df <- df %>%
#   rowwise() %>%
#   mutate(score = score(value, truth, type, quantile))
#
# write_csv(df, paste0("data/scores_KIT-frozen_baseline.csv.gz"))
#
# # Aggregate scores
# df <- df %>%
#   group_by(model, location, age_group, target, type) %>%
#   summarize(score = mean(score))
#
# write_csv(df, paste0("data/scores_KIT-frozen_baseline_aggregated.csv.gz"))
