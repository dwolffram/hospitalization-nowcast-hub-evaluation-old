library(tidyverse)
source("utils.R")

# Quantile score
qs <- function(q, y, alpha) {
  2 * (as.numeric(y < q) - alpha) * (q - y)
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


df <- load_data(add_baseline = TRUE, add_median = TRUE, shorten_names = TRUE, fix_data = TRUE,
                add_truth = TRUE, exclude_missing = TRUE, eval_date = "2022-08-08")

df <- df %>%
  rowwise() %>%
  mutate(score = score(value, truth, type, quantile))

df <- df %>%
  select(-c(pathogen, value, truth))

write_csv(df, paste0("data/scores.csv.gz"))

# Aggregate scores
df <- df %>%
  group_by(model, location, age_group, target, type) %>%
  summarize(score = mean(score))

write_csv(df, paste0("data/scores_aggregated.csv.gz"))


# s <- df %>%
#   filter(
#     location == "DE",
#     age_group != "00+"
#   ) %>%
#   group_by(model, type) %>%
#   summarize(score = mean(score))
