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

df_files2 <- data.frame(path = files) %>%
  mutate(model = str_split(path, "/", simplify = TRUE)[, 1]) %>%
  mutate(forecast_date = str_extract(path, "\\d{4}-\\d{2}-\\d{2}")) %>%
  filter(
    forecast_date >= START_DATE,
    forecast_date <= END_DATE
  )

df_files2 <- df_files2 %>% 
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


### Fill in retrospective submissions

df_files <- df_files %>% 
  mutate(retrospective = FALSE)

df_files2 <- df_files2 %>% 
  mutate(retrospective = TRUE)

d <- bind_rows(df_files, df_files2)

# d %>% 
#   count(model, forecast_date) %>% 
#   filter(n > 1)

d <- d %>% 
  group_by(model, forecast_date) %>% 
  mutate(duplicated = n() > 1)

d <- d %>% 
  filter(!(!retrospective & duplicated))

d <- d %>% 
  mutate(folder = if(retrospective) PATH_RETRO else HUB_PATH)

# load all submissions into one dataframe in long format
df <- data.frame()
pb <- txtProgressBar(min = 0, max = nrow(d), style = 3)
for (i in 1:nrow(d)) {
  row <- d[i, ]
  df_temp <- read_csv(paste0(row$folder, row$path),
                      show_col_types = FALSE, progress = FALSE
  )
  df_temp$model <- row$model
  df_temp$retrospective = row$retrospective
  df <- bind_rows(df, df_temp)
  
  setTxtProgressBar(pb, i)
}

write_csv(df, paste0("data/submissions_", END_DATE, "_filled.csv.gz"))

df %>% 
  count(model)

df %>% 
  group_by(model) %>% 
  distinct(forecast_date) %>% 
  group_by(model) %>% 
  summarize(n = n())

# SU: covered 156 out of 159 days

df %>% 
  group_by(model) %>% 
  summarize(n = n_distinct(forecast_date))

d %>% 
  group_by(model) %>% 
  summarize(n = n_distinct(forecast_date))


a <- df %>% 
  group_by(model, location) %>% 
  summarize(n = n_distinct(forecast_date))

# Epiforecasts-independent, DE-HH, 156 (3 missing!)

b <- df %>% 
  group_by(model, age_group) %>% 
  summarize(n = n_distinct(forecast_date))

u <- df %>% 
  filter(model == "KIT-simple_nowcast",
         type == "mean")

v <- df %>% 
  filter(model == "NowcastHub-MeanEnsemble",
         type == "mean")

u %>% 
  group_by(model, location) %>% 
  summarize(n = n())

v %>% 
  group_by(model, location) %>% 
  summarize(n = n())

t <- df %>% 
  group_by(model, target) %>% 
  summarize(n = n())

# Ensembles are missing 0 and -1 ahead in the beginning (two entries)
# SZ: -23 to -28 are missing some entries (28704 instead of 29256)

l <- df %>% 
  group_by(model, location) %>% 
  summarize(n = n())

# LMU: DE-HB, 8671 and DE-SL, 4814 (instead of 36888) -- quantiles mostly missing