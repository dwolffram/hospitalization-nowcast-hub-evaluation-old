source("utils.R")

PATH_UPDATED <- "../hospitalization-nowcast-hub/data-processed_updated_models/"
files <- list.files(PATH_UPDATED, pattern = "*.csv", recursive = TRUE)

df_files <- data.frame(path = files) %>%
  mutate(model = str_split(path, "/", simplify = TRUE)[, 1]) %>%
  mutate(forecast_date = str_extract(path, "\\d{4}-\\d{2}-\\d{2}"))

df_files <- df_files %>% 
  filter(model %in% c("NowcastHub-MeanEnsemble40", "NowcastHub-MedianEnsemble40")) %>% 
  mutate(model = str_sub(model, 12, -3))

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

write_csv(df, "data/ensembles_updated.csv.gz")
