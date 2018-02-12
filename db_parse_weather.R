library(tidyverse)
library(lubridate)

wdf <- read_csv("./weather_data/weather_hourly.csv")

# Note a couple of rows are missing rain data, can we use random forest to predict these?
wdf %>% filter(ind != 0) %>% View()
wdf %>%
  filter(month(date) == 4) %>%
  View()
# But note the previous readings for that day indicate that it wasn't raining at those times
# So we can set them to 0
wdf_fix <- wdf %>%
  mutate(
    rain = if_else(
      is.na(rain),
      0,
      rain
    )
  )
wdf_fix %>% filter(ind != 0) %>% View()

# Split date into individual parts so we can use this to match the target df
wdf_time <- wdf_fix %>%
  mutate(
    month = month(date),
    date = as.character(date),
    year = str_extract(date, "\\d{4}"),
    day = str_extract(date, "\\d{2}"),
    hour = str_replace(str_extract(date, "\\d{2}:"), ":", ""),
    minute = str_replace(str_extract(date, ":\\d{2}"), ":", "")
  ) %>%
  select(-ind, -ind_1, -ind_2) %>%
  filter(!is.na(date))

wdf_time %>% group_by(year, month, day) %>% summarise(r = mean(rain)) %>% View()

summary(wdf_time)

# Save this data frame
write_rds(wdf_time, "./saved_data_frames/db_weather.rds")
