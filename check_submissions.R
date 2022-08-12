library(tidyverse)
source("utils.R")
df <- read_csv("data/submissions_2021-11-22_2022-04-29.csv.gz")

# Add baseline
df_baseline <- read_csv(paste0("data/submissions_KIT-frozen_baseline.csv.gz")) %>% 
  mutate(retrospective = FALSE)
df <- bind_rows(df, df_baseline)

df$model <- factor(df$model,
                   levels = sort(unique(df$model)),
                   labels = SHORT_NAMES)


# View(subset(df, type == "mean" &
#             forecast_date == "2022-04-05" &
#             location == "DE-HH"))
# 
# View(subset(df, 
#               forecast_date == "2022-04-05" &
#               location == "DE-HH"))
# 
# View(df %>% 
#        filter(model == "RKI" &
#               value > 1000000))


# problematic rows
df_error <- df %>% 
  filter(model == "RKI",
         value > 1000000 | is.na(value)) %>% 
  select(c(location, age_group, forecast_date, type, quantile))

df_replacement <- df %>% 
  filter(model == "RKI",
         target %in% c("0 day ahead inc hosp", "-1 day ahead inc hosp"))

df_fixed <- df_error %>% 
  left_join(df_replacement) %>% 
  group_by(location, age_group, forecast_date, type, quantile, pathogen, model, retrospective) %>% 
  summarize(target_end_date = max(target_end_date),
            target = max(target),
            value = min(value, na.rm = TRUE))

nrow(df)

df <- df %>% 
  filter(!(model == "RKI" & (value > 1000000 | is.na(value))))
df <- bind_rows(df, df_fixed)

nrow(df)


# some 0 day ahead nowcasts are unrealisticly large (>1 bio)
# we replace them with the respective -1 day ahead nowcasts
fix_rki_errors <- function(df) {
  df_error <- df %>% 
    filter(model == "RKI",
           value > 1000000 | is.na(value)) %>% 
    select(c(location, age_group, forecast_date, type, quantile))
  
  df_replacement <- df %>% 
    filter(model == "RKI",
           target %in% c("0 day ahead inc hosp", "-1 day ahead inc hosp"))
  
  df_fixed <- df_error %>% 
    left_join(df_replacement) %>% 
    group_by(location, age_group, forecast_date, type, quantile, pathogen, model, retrospective) %>% 
    summarize(target_end_date = max(target_end_date),
              target = max(target),
              value = min(value, na.rm = TRUE))
  
  df <- df %>% 
    filter(!(model == "RKI" & (value > 1000000 | is.na(value))))
  df <- bind_rows(df, df_fixed)
  
  return(df)
}


View(df %>%
       filter(model == "RKI" &
              value > 1000000))

df <- fix_rki_errors(df)

View(df %>%
       filter(model == "RKI" &
                value > 1000000))

a <- df %>% 
  filter(value <= 1000000)

nrow(df)

b <- subset(df, value >  1000000)
c <- subset(df, value < 1000000)

nrow(b) + nrow(c)

nrow(df)




View(subset(df, model == "RKI-weekly_report" & forecast_date == "2021-11-24"))

d <- df %>% 
  group_by(model, forecast_date) %>% 
  mutate(duplicated = n() > 1)

# if duplicated (prospective + retrospective) remove prospective submission
d <- d %>% 
  filter(!(!retrospective & duplicated))



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


# Epiforecast: missing 0.1 quantile
# RKI: only two quantiles provided