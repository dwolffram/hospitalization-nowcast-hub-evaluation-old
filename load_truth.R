library(tidyverse)
library(jsonlite)
library(runner)

load_reporting_triangle <- function(as_of) {
  # retrieve all commits on the given date
  commits <- fromJSON(paste0(
    "https://api.github.com/repos/KITmetricslab/hospitalization-nowcast-hub/commits?path=data-truth/COVID-19/COVID-19_hospitalizations.csv",
    "&since=", as.Date(as_of) - 1,
    "&until=", as_of
  ),
  simplifyDataFrame = TRUE, flatten = TRUE
  )

  # get sha of latest commit on the given date
  sha <- commits %>%
    mutate(date = as.Date(commit.author.date)) %>%
    filter(date == as_of) %>%
    filter(commit.author.date == max(commit.author.date)) %>%
    pull(sha)

  # load the corresponding data
  read_csv(paste0(
    "https://raw.githubusercontent.com/KITmetricslab/hospitalization-nowcast-hub/",
    sha,
    "/data-truth/COVID-19/COVID-19_hospitalizations_preprocessed.csv"
  ), show_col_types = FALSE)
}


load_truth <- function(as_of) {
  df <- load_reporting_triangle(as_of)

  # rowwise sum to aggregate the corrections across all delays
  df <- df %>%
    mutate(value = rowSums(across(starts_with("value")), na.rm = TRUE))

  # compute 7-day rolling sum within each stratum
  df %>%
    group_by(location, age_group) %>%
    mutate(truth = sum_run(value, 7, na_pad = TRUE)) %>%
    select(date, location, age_group, truth) %>%
    drop_na()
}

# df_truth <- load_truth("2022-02-27")
