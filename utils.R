SHORT_NAMES <- c(
  "Epiforecasts", "ILM", "KIT-frozen_baseline", "KIT-simple_nowcast",
  "LMU", "MeanEnsemble", "MedianEnsemble",
  "RIVM", "RKI", "SU", "SZ"
)

MODEL_COLORS <- setNames(
  c("#B30000", "#E69F00", "#999999", "#56B4E9", "#F0E442", "#009E73", "#60D1B3", "#D55E00", "#3C4AAD", "#CC79A7", "#000000"),
  c("Epiforecasts", "ILM", "KIT-frozen_baseline", "KIT-simple_nowcast", "LMU", "MeanEnsemble", "MedianEnsemble", "RIVM", "RKI", "SU", "SZ")
)


filter_scores <- function(df, type = "quantile", level = "national", by_horizon = FALSE, average = TRUE) {
  df <- df %>%
    filter(type == !!type)

  if (level == "national") {
    df <- df %>%
      filter(
        location == "DE",
        age_group == "00+"
      )
  } else if (level == "states") {
    df <- df %>%
      filter(
        location != "DE",
        model != "ILM"
      )
  } else if (level == "age") {
    df <- df %>%
      filter(
        location == "DE",
        age_group != "00+",
        model != "RKI"
      )
  }


  if (by_horizon) {
    df <- df %>%
      mutate(horizon = as.numeric(str_extract(target, "-?\\d+"))) %>%
      arrange(model, location, age_group, horizon)

    if (average) {
      df <- df %>%
        group_by(model, target, horizon) %>%
        summarize(score = mean(score), .groups = "drop") %>%
        arrange(model, horizon)
    }
  } else {
    if (average) {
      df <- df %>%
        group_by(model) %>%
        summarize(score = mean(score), .groups = "drop")
    } else {
      df <- df %>%
        group_by(model, location, age_group) %>%
        summarize(score = mean(score), .groups = "drop")
    }
  }

  return(df)
}


load_scores <- function(start_date = "2021-11-22", end_date = "2022-04-29",
                        aggregate_scores = FALSE, shorten_names = TRUE,
                        load_baseline = TRUE) {
  if (aggregate_scores) {
    df <- read_csv(paste0("data/scores_", start_date, "_", end_date, "_aggregated.csv.gz"))
  } else {
    df <- read_csv(paste0("data/scores_", start_date, "_", end_date, ".csv.gz"))
    df_baseline <- read_csv(paste0("data/scores_", start_date, "_", end_date, "_baseline.csv.gz"))
    df <- bind_rows(df, df_baseline)
  }

  if (shorten_names) {
    df$model <- factor(df$model,
      levels = sort(unique(df$model)),
      labels = SHORT_NAMES
    )
  }

  if (!load_baseline) {
    df <- df %>%
      filter(model != "KIT-frozen_baseline")
  }

  return(df)
}

# some 0 day ahead nowcasts are unrealisticly large (>1 bio)
# we replace them with the respective -1 day ahead nowcasts
fix_rki_errors <- function(df) {
  df_error <- df %>%
    filter(
      model == "RKI",
      value > 1000000 | is.na(value)
    ) %>%
    select(c(location, age_group, forecast_date, type, quantile))

  df_replacement <- df %>%
    filter(
      model == "RKI",
      target %in% c("0 day ahead inc hosp", "-1 day ahead inc hosp")
    )

  df_fixed <- df_error %>%
    left_join(df_replacement) %>%
    group_by(location, age_group, forecast_date, type, quantile, pathogen, model, retrospective) %>%
    summarize(
      target_end_date = max(target_end_date),
      target = max(target),
      value = min(value, na.rm = TRUE)
    )

  df <- df %>%
    filter(!(model == "RKI" & (value > 1000000 | is.na(value))))
  df <- bind_rows(df, df_fixed)

  return(df)
}
