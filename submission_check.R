library(tidyverse)
source("utils.R")
source("fix_submissions.R")

# Load all submissions
df <- read_csv("data/submissions_2021-11-22_2022-04-29.csv.gz")
df <- read_csv("data/submissions.csv.gz")

# Add baseline
df_baseline <- read_csv(paste0("data/submissions_KIT-frozen_baseline.csv.gz")) %>% 
  mutate(retrospective = FALSE)
df <- bind_rows(df, df_baseline)

# Use shortened model names
df$model <- factor(df$model,
                   levels = sort(unique(df$model)),
                   labels = SHORT_NAMES)

# fix incomplete and erroneus nowcasts
df <- fix_RKI(df)
df <- fix_epiforecasts(df)
df <- fix_ILM(df)
df <- fix_LMU(df)

# All targets that should be present
template <- df_baseline %>% 
  select(- c(value, model, retrospective, pathogen))

# Models that cover all locations and age groups
models_all <- c("Epiforecasts", "KIT-frozen_baseline", "KIT-simple_nowcast", 
                "LMU", "MeanEnsemble", "MedianEnsemble", "RIVM", "SU", "SZ")

for (m in models_all) {
  df_temp <- df %>% 
    filter(model == m)
  
  df_temp <- template %>% 
    anti_join(df_temp, by = c("location", "age_group", "forecast_date", "target_end_date", 
                              "target", "type", "quantile"))
  
  print(paste(m, ": ", nrow(df_temp), " rows missing."))
  write_csv(df_temp, paste0("data/submission_check/fixed/", m, "-missing.csv"))
  
}


# Does not cover states
m <- "ILM"

df_temp <- df %>% 
  filter(model == m)

df_temp <- template %>% 
  filter(location == "DE") %>% 
  anti_join(df_temp, by = c("location", "age_group", "forecast_date", "target_end_date", 
                            "target", "type", "quantile"))

print(paste(m, ": ", nrow(df_temp), " rows missing."))
write_csv(df_temp, paste0("data/submission_check/fixed/", m, "-missing.csv"))

# Does not cover age groups
m <- "RKI"

df_temp <- df %>% 
  filter(model == m)

df_temp <- template %>% 
  filter(age_group == "00+") %>% 
  anti_join(df_temp, by = c("location", "age_group", "forecast_date", "target_end_date", 
                            "target", "type", "quantile"))

print(paste(m, ": ", nrow(df_temp), " rows missing."))
write_csv(df_temp, paste0("data/submission_check/fixed/", m, "-missing.csv"))
