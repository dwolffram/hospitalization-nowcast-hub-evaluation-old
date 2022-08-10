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