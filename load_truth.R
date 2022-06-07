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


### Functions to load "frozen" truth values

INDICES_FROZEN <- lower.tri(diag(7), diag = TRUE)[7:1, ]

frozen_sum <- function(df) {
  if (nrow(df) != 7) {
    return(NA)
  } else {
    values <- df %>%
      ungroup() %>%
      select(value_0d:value_6d)

    sum(values[INDICES_FROZEN])
  }
}

load_frozen_truth <- function(start_date = "2021-11-01") {
  df <- read_csv("../hospitalization-nowcast-hub/data-truth/COVID-19/COVID-19_hospitalizations.csv",
    show_col_types = FALSE
  ) %>%
    filter(date >= start_date)

  df <- df %>%
    group_by(location, age_group) %>%
    # arrange(date) %>%
    run_by(idx = "date", k = "7 days") %>%
    mutate(frozen_value = runner(x = ., f = function(x) {
      frozen_sum(x)
    })) %>%
    select(c(date, location, age_group, frozen_value)) %>%
    drop_na()
}

# truth_frozen <- load_frozen_truth("2021-11-01")
