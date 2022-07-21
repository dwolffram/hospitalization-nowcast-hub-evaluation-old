library(tidyverse)

HUB_PATH <- "../hospitalization-nowcast-hub/data-processed/"
START_DATE <- "2021-11-22"
END_DATE <- "2022-04-29"

# models <- list.dirs(HUB_PATH, full.names = FALSE, recursive = FALSE)
files <- list.files(HUB_PATH, pattern = "*.csv", recursive = TRUE)

df_files <- data.frame(path = files) %>%
  mutate(model = str_split(path, "/", simplify = TRUE)[, 1]) %>%
  mutate(forecast_date = str_extract(path, "\\d{4}-\\d{2}-\\d{2}")) %>%
  filter(
    forecast_date >= START_DATE,
    forecast_date <= END_DATE
  )

# load all submissions into one dataframe in long format
df <- data.frame()
pb <- txtProgressBar(min = 0, max = nrow(df_files), style = 3)
for (i in 1:nrow(df_files)) {
  row <- df_files[i, ]
  df_temp <- read_csv(paste0(HUB_PATH, row$path),
    show_col_types = FALSE, progress = FALSE
  )
  df_temp$model <- row$model
  df <- bind_rows(df, df_temp)

  setTxtProgressBar(pb, i)
}

write_csv(df, paste0("data/submissions_", END_DATE, ".csv.gz"))


### LOAD RETROSPECTIVE SUBMISSIONS

PATH_RETRO <- "../hospitalization-nowcast-hub/data-processed_retrospective/"
files <- list.files(PATH_RETRO, pattern = "*.csv", recursive = TRUE)

df_files <- data.frame(path = files) %>%
  mutate(model = str_split(path, "/", simplify = TRUE)[, 1]) %>%
  mutate(forecast_date = str_extract(path, "\\d{4}-\\d{2}-\\d{2}")) %>%
  filter(
    forecast_date >= START_DATE,
    forecast_date <= END_DATE
  )

df_files <- df_files %>% 
  filter(str_detect(path, "Check", negate = TRUE))

# load all submissions into one dataframe in long format
df <- data.frame()
pb <- txtProgressBar(min = 0, max = nrow(df_files), style = 3)
for (i in 1:nrow(df_files)) {
  row <- df_files[i, ]
  df_temp <- read_csv(paste0(PATH_RETRO, row$path),
                      show_col_types = FALSE, progress = FALSE
  )
  df_temp$model <- row$model
  df <- bind_rows(df, df_temp)
  
  setTxtProgressBar(pb, i)
}

write_csv(df, paste0("data/submissions_", END_DATE, "_retrospective.csv.gz"))
