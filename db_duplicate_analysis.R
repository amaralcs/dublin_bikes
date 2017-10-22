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
    Sec = second(last_update),
    Date = ymd(paste(Year, Month, Day, sep = "-")),
    Time = paste(Hour, Min, Sec, sep = ":"),
    Weekday = weekdays(Date, abbreviate = TRUE)
  ) %>%  
  arrange(Year, Month, Day, Hour, Min, Sec)
# Add factor level information to weekdays
days_level <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
dup_df <- dup_df %>%
  mutate(Weekday = factor(Weekday, levels = days_level))


# Group up each duplicate and count their number
grouped_dup <- dup_df %>%
  group_by(number, name, last_update, Year, Month, Day, Weekday) %>%
  summarise(
    count = n()
  )
  
# View data with largest amount of duplicates
small_grouped_dup <- grouped_dup %>%
  filter(count >1)

grouped_dup %>%
  ggplot(aes(number)) +
  geom_bar(aes(fill = "red"))

grouped_dup %>%
  ggplot(aes(Day)) +
  geom_bar(aes(fill = "red"))

grouped_dup %>%
  ggplot(aes(Month)) +
  geom_bar(aes(fill = "red"))

grouped_dup %>%
  ggplot(aes(Weekday)) +
  geom_bar(aes(fill = "red"))
