library(tidyverse)
library(lubridate)

# Change working directory - change appropriately to where rds files are
setwd("C:/Users/Carlos/Documents/Dublin Bikes Project/dublin_bikes/saved_data_frames")

# Read previously processed data
dup_df <- as.tibble(read_rds("duplicate_data.rds"))

# Calculate difference in number of bikes between periods
dup_df <- dup_df %>%
  # Convert POSIXct to date and split into each col
  mutate(
    last_update = as_datetime(last_update/1000, tz = "GMT"),
    Year = year(last_update),
    Month = month(last_update),
    Day = day(last_update),
    Hour = as.character(hour(last_update)),
    Min = minute(last_update),
    Sec = second(last_update)
  ) %>%  
  arrange(Year, Month, Day, Hour, Min, Sec)

# Group up each duplicate and count their number
grouped_dup <- dup_df %>%
  group_by(number, name, last_update) %>%
  summarise(
    count = n()
  )
  
# View data with largest amount of duplicates
grouped_dup %>%
  filter(count >1) %>%
  arrange(desc(count)) %>%
  View()

