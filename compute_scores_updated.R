source("utils.R")
source("fix_submissions.R")

START_DATE <- "2021-11-22"
END_DATE <- "2022-04-29"

PATH_UPDATED <- "../hospitalization-nowcast-hub/data-processed_updated_models/"
files <- list.files(PATH_UPDATED, pattern = "*.csv", recursive = TRUE)

df_files <- data.frame(path = files) %>%
  mutate(model = str_split(path, "/", simplify = TRUE)[, 1]) %>%
  mutate(forecast_date = str_extract(path, "\\d{4}-\\d{2}-\\d{2}")) %>%
  filter(
    forecast_date >= START_DATE,
    forecast_date <= END_DATE
  )

df_files <- df_files %>% 
  filter(str_detect(path, "raw", negate = TRUE))

df_files <- df_files %>% 
  filter(model != "ILM-prop")

# df_files <- df_files %>% 
#   filter(!(model == "ILM-prop" & forecast_date %in% c("2021-11-27", "2021-11-28")))

# load all submissions into one dataframe in long format
df <- data.frame()
pb <- txtProgressBar(min = 0, max = nrow(df_files), style = 3)
for (i in 1:nrow(df_files)) {
  row <- df_files[i, ]
  df_temp <- read_csv(paste0(PATH_UPDATED, row$path),
                      show_col_types = FALSE, progress = FALSE
  )
  df_temp$model <- row$model
  df <- bind_rows(df, df_temp)
  
  setTxtProgressBar(pb, i)
}

df <- df %>% 
  mutate(model = str_sub(model, 1, 3),
         retrospective = FALSE) %>% # need column 'retrospective' for fix_RKI()
  fix_RKI() %>% 
  select(-retrospective)


files <- c("Epiforecasts-missing.csv", "MeanEnsemble-missing.csv", "MedianEnsemble-missing.csv", 
           "SZ-missing.csv")

df_missing <- data.frame()
for (f in files) {
  df_temp <- read_csv(paste0("data/submission_check/fixed/", f),
                      show_col_types = FALSE, progress = FALSE)
  df_missing <- bind_rows(df_missing, df_temp)
}

df <- anti_join(df, df_missing)

df <- df %>% 
  select(-forecast_date)

write_csv(df, paste0("data/submissions_updated.csv.gz"))


### Compute QS, AE and MSE

df <- read_csv("data/submissions_updated.csv.gz")

df_median <- df %>%
  filter(quantile == 0.5) %>%
  mutate(type = "median")
df <- bind_rows(df, df_median)

df_truth <- load_truth(as_of = "2022-08-08")

df <- df %>%
  left_join(df_truth, by = c("location", "age_group", "target_end_date" = "date"))

df <- df %>%
  rowwise() %>%
  mutate(score = score(value, truth, type, quantile))

df <- df %>%
  select(-c(pathogen, value, truth))

write_csv(df, paste0("data/scores_updated.csv.gz"))


### Compute WIS decomposition

df <- read_csv("data/submissions_updated.csv.gz")

df_truth <- load_truth(as_of = "2022-08-08")

df <- df %>%
  left_join(df_truth, by = c("location", "age_group", "target_end_date" = "date"))

df <- df %>% 
  filter(type == "quantile")

df_median <- df %>% 
  filter(quantile == 0.5) %>% 
  rename(med = value) %>% 
  select(-c(quantile, pathogen, truth))

df <- df %>%
  left_join(df_median)

df_scores <- df %>%
  rowwise() %>%
  mutate(score = score(value, truth, type, quantile),
         spread = score(value, med, type, quantile))

df_scores <- df_scores %>% 
  mutate(overprediction = ifelse(med > truth, score - spread, 0),
         underprediction = ifelse(med < truth, score - spread, 0))


df_national <- filter_data(df_scores, level = "national")

df_national <- df_national %>% 
  group_by(model) %>% 
  summarize(spread = mean(spread),
            overprediction = mean(overprediction),
            underprediction = mean(underprediction),
            score = mean(score))

write_csv(df_national, paste0("data/wis_updated_national.csv.gz"))



df_states <- filter_data(df_scores, level = "states")

df_states <- df_states %>% 
  group_by(model) %>% 
  summarize(spread = mean(spread),
            overprediction = mean(overprediction),
            underprediction = mean(underprediction),
            score = mean(score))

write_csv(df_states, paste0("data/wis_updated_states.csv.gz"))


df_age <- filter_data(df_scores, level = "age")

df_age <- df_age %>% 
  group_by(model) %>% 
  summarize(spread = mean(spread),
            overprediction = mean(overprediction),
            underprediction = mean(underprediction),
            score = mean(score))

write_csv(df_age, paste0("data/wis_updated_age.csv.gz"))
