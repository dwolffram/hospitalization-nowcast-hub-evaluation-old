library(runner)
source("R/load_data.R")


t <- read_csv("../hospitalization-nowcast-hub/data-truth/COVID-19/COVID-19_hospitalizations.csv",
  show_col_types = FALSE
)

INDICES_FROZEN <- lower.tri(diag(7), diag = TRUE)[7:1, ]

frozen_sum <- function(df) {
  if (nrow(df) != 7) {
    return(NA)
  } else {
    values <- df %>%
      ungroup() %>%
      select(value_0d:value_6d)

    sum(values[INDICES_FROZEN])
  }
}

a <- t %>%
  filter(
    location == "DE",
    age_group == "00+"
  )

b <- a %>%
  filter(date <= "2021-04-24") %>%
  tail(7)

frozen_sum(b)

b <- b %>%
  select(value_0d:value_6d)

sum(b[indices_frozen])



t1 <- t %>%
  filter( # location == "DE",
    date >= "2021-11-01"
  )

r <- t1 %>%
  group_by(location, age_group) %>%
  arrange(date) %>%
  run_by(idx = "date", k = "7 days") %>%
  mutate(frozen_value = runner(x = ., f = function(x) {
    frozen_sum(x)
  }))

r1 <- r %>%
  select(c(date, location, age_group, frozen_value))

load_frozen_truth <- function(start_date = "2021-11-01") {
  df <- read_csv("../hospitalization-nowcast-hub/data-truth/COVID-19/COVID-19_hospitalizations.csv",
    show_col_types = FALSE
  ) %>%
    filter(date >= start_date)

  df <- df %>%
    group_by(location, age_group) %>%
    # arrange(date) %>%
    run_by(idx = "date", k = "7 days") %>%
    mutate(frozen_value = runner(x = ., f = function(x) {
      frozen_sum(x)
    })) %>%
    select(c(date, location, age_group, frozen_value)) %>% 
    drop_na()
}

truth_frozen <- load_frozen_truth()
truth_frozen2 <- load_frozen_truth()




INDICES_FROZEN <- lower.tri(diag(7), diag = TRUE)[7:1, ]
INDICES_FROZEN

x <- INDICES_FROZEN
k <- 1
x
col(x) == row(x) + k


m3 <- matrix(NA, nrow = 7, ncol = 7 + 2)
m <- col(m3) <= row(m3) + 2

m[7:1, ]


m3 <- matrix(NA, nrow = 7, ncol = 7)
m <- col(m3) <= row(m3)
m[7:1, ]



make_frozen_indices <- function(lead_time = 0){
  m <- matrix(NA, nrow = 7, ncol = 7 + lead_time)
  m <- col(m) <= row(m) + lead_time
  m[7:1, ]
}

make_frozen_indices(4)
