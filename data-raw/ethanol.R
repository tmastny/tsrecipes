# http://www.timeseriesclassification.com/description.php?Dataset=EthanolLevel
library(readr)
library(janitor)
library(dplyr)
library(tidyr)

ts <- read_csv(
  "data-raw/EthanolLevel_TRAIN.arff", comment = "@", skip = 28,
  col_names = FALSE
)

ts_clean <- ts %>%
  mutate(id = row_number()) %>%
  rename(class = X1752) %>%
  mutate(class = case_when(
    class == 1 ~ "e35",
    class == 2 ~ "e38",
    class == 3 ~ "e40",
    class == 4 ~ "e45"
  )) %>%
  pivot_longer(
    c(-id, -class),
    names_to = "day", names_transform = list(day = parse_number),
    values_to = "ts"
  )

ethanol <- ts_clean %>%
  group_by(id, class) %>%
  summarise(ts = list(ts), .groups = "drop")

usethis::use_data(ethanol, overwrite = TRUE)
