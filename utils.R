SHORT_NAMES <- c(
  "Epiforecasts", "ILM", "KIT-simple_nowcast",
  "LMU", "MeanEnsemble", "MedianEnsemble",
  "RIVM", "RKI", "SU", "SZ"
)

MODEL_COLORS <- setNames(
  c("#999999", "#E69F00", "#56B4E9", "#F0E442", "#009E73", "#60D1B3", "#D55E00", "#3C4AAD", "#CC79A7", "#000000"),
  c("Epiforecasts", "ILM", "KIT-simple_nowcast", "LMU", "MeanEnsemble", "MedianEnsemble", "RIVM", "RKI", "SU", "SZ")
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


load_scores <- function(aggregate_scores = FALSE, shorten_names = TRUE) {
  if (aggregate_scores) {
    df <- read_csv(paste0("data/scores_2022-08-08.csv.gz"))
  } else {
    df <- read_csv(paste0("data/scores_2022-04-29_2022-08-02_filled.csv.gz"))
  }

  if (shorten_names) {
    df$model <- factor(df$model,
      levels = sort(unique(df$model)),
      labels = SHORT_NAMES
    )
  }

  return(df)
}
