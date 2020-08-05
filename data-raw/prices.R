library(readr)
library(janitor)
library(dplyr)
library(tidyr)

ts <- read_csv(
  "SharePriceIncrease/SharePriceIncrease_TRAIN.arff", comment = "@",
  col_names = FALSE
)

ts_clean <- ts %>%
  mutate(id = row_number()) %>%
  pivot_longer(
    X1:X60,
    names_to = "day", names_transform = list(day = parse_number),
    values_to = "ts"
  ) %>%
  rename(class = X61) %>%
  mutate(class = factor(
    ifelse(class == 0, "no_increase", "increase"),
    levels = c("no_increase", "increase")
  ))


prices <- ts_clean %>%
  group_by(id, class) %>%
  summarise(ts = list(ts), .groups = "drop")

usethis::use_data(prices, overwrite = TRUE)
