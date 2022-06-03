library(runner)

t <- read_csv("../hospitalization-nowcast-hub/data-truth/COVID-19/COVID-19_hospitalizations_preprocessed.csv",
              show_col_types = FALSE)

INDICES_FROZEN <- lower.tri(diag(7), diag = TRUE)[7:1, ]

frozen_sum <- function(df){
  if (nrow(df) != 7){
    return(NA)
  }
  else {
    values <- df %>% 
      ungroup() %>% 
      select(value_0d:value_6d)
  
    sum(values[INDICES_FROZEN])
  }
  
}

a <- t %>% 
  filter(location == "DE",
         age_group == "00+")

b <- a %>% 
  filter(date <= "2021-04-24") %>% 
  tail(7)

frozen_sum(b)

b <- b %>% 
  select(value_0d:value_6d)

sum(b[indices_frozen])



t1 <- t %>% 
  filter(#location == "DE",
         date >= "2021-11-01")

r <- t1 %>% 
  group_by(location, age_group) %>% 
  arrange(date) %>% 
  run_by(idx = "date", k = "7 days") %>%
  mutate(frozen_value = runner(x = ., f = function(x){frozen_sum(x)}))

r1 <- r %>% 
  select(c(date, location, age_group, frozen_value))

