library(tidyverse)
library(RcppRoll)

Sys.setlocale("LC_ALL", "C")

lower_triangle_mask <- function(max_delay = 80) {
  # first 3 columns
  m1 <- matrix(FALSE, nrow = max_delay, ncol = 3)
  
  # because of the >80d column we create a larger triangle (82x82)
  # and then cut off the first and last row
  m2 <- matrix(0, nrow = max_delay + 2, ncol = max_delay + 2)
  m2 <- apply(lower.tri(m2, diag = TRUE), 1, rev)
  m2 <- m2[-c(1, max_delay + 2), ]
  
  # where to place NA in the tail
  cbind(m1, m2)
}

load_truth <- function(location = "DE", age_group = "00+", as_of, reload = TRUE) {
  if (reload == TRUE | !exists("df_truth", where = .GlobalEnv)) {
    df_truth <- read_csv("../hospitalization-nowcast-hub/data-truth/COVID-19/COVID-19_hospitalizations_preprocessed.csv",
                         show_col_types = FALSE)
    assign("df_truth", df_truth, envir = .GlobalEnv)
  }
  
  df <- df_truth %>%
    filter(
      location == {{ location }},
      age_group == {{ age_group }}
    )
  
  if (!missing(as_of)) {
    df <- df %>%
      filter(date <= as_of)
    
    m <- lower_triangle_mask()
    
    # remove entries that were not known at the given date (as_of)
    df[(nrow(df) - 79):nrow(df), ][m] <- NA
  }
  
  # aggregate across all delays and compute 7-day rolling sum
  df %>%
    group_by(location, age_group, date) %>%
    summarize(value = sum(c_across(starts_with("value")), na.rm = TRUE), .groups = "drop") %>%
    mutate(value_7d = roll_sumr(value, n = 7))
}

load_nowcast <- function(model_name, date, location = "DE", age_group = "00+") {
  read_csv(paste0("../hospitalization-nowcast-hub/data-processed/", model_name, "/", date, "-", model_name, ".csv"),
           show_col_types = FALSE) %>%
    filter(
      location == {{ location }},
      age_group == {{ age_group }},
      type == "quantile"
    ) %>%
    pivot_wider(names_from = quantile, names_prefix = "quantile_")
}