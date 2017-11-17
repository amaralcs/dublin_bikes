library(tidyverse)
library(lubridate)

" Prepares the data to be analyses by the queing theory model "

################################ Support Functions ###################################
# Support function to calculate number of checked in bikes
# Args: diff <- difference in # of bikes from previous period to current period
c_check_in <- function(diff){
  # Check if there was a check in
  if(diff > 0){
    return (diff)
  }
  # else return 0 as there was no check in
  else{
    return (0)
  }
}

# Support function to calculate number of checked out bikes
# Args: diff <- difference in # of bikes from previous period to current period
c_check_out <- function(diff){
  # Check if there was a check out
  if(diff < 0){
    return (abs(diff))
  }
  # else return 0 as there was no check out
  else{
    return (0)
  }
}

############################ Initial data Prepation##################################
# Change working directory - change appropriately to where csv files are
setwd("../data_dump")

# Get all file names in the directory
file_names <- dir()
# Skip the first one as is problematic
file_names <- file_names[-1]

# Read all files into data frame
base_df <- do.call(rbind, lapply(file_names, read_csv))

# Remove duplicate columns (noted especially station 16 has duplicates)
base_df <- distinct(base_df)

# Calculate difference in number of bikes between periods
df <- base_df %>%
  # Convert POSIXct to date and split into each col
  mutate(
    last_update = as_datetime(last_update/1000, tz = "GMT"),
    year = year(last_update),
    month = month(last_update),
    day = day(last_update),
    hour = as.character(hour(last_update)),
    min = minute(last_update),
    sec = "00"
  ) %>%  
  # Standardise minutes i.e. group them in 10 minute slots and make new time
  mutate(
    min = ifelse(min < 10, "00",
                 ifelse(min < 20, "10",
                        ifelse(min < 30, "20",
                               ifelse(min < 40, "30",
                                      ifelse(min < 50, "40",
                                             ifelse(min < 60, "50",
                                                    NA)))))
    ),
    date = ymd(paste(year, month, day, sep = "-")),
    time = paste(hour, min, sec, sep = ":"),
    weekday = weekdays(date, abbreviate = TRUE)
  ) %>%
  # Sort in order so we can calculate the differences
  group_by(number) %>%
  arrange(year, month, day, hour, min, sec) %>%
  # Calculate differences
  mutate(
    prev_period_diff = 
      available_bike_stands - lag(available_bike_stands, default = available_bike_stands[1])
  ) %>%
  # Apply functions to determine checked in/out 
  rowwise() %>%
  mutate(
    check_in = c_check_in(prev_period_diff),
    check_out = c_check_out(prev_period_diff)
  ) %>%
  ungroup()

# Temporary data frame that calculates interarrival for periods of time 
# with at least 1 check in. It is essential to remove the rows with zeroes
# since they do not affect the inter-arrival 
# Note that the first value for each station is going to be NA (since there's no previous time)
# But this is corrected later when this data is joined back
df_in <- df %>%
  filter(check_in != 0) %>%
  group_by(number, name, address) %>%
  arrange(number, last_update) %>%
  # Calculate time difference between each row
  mutate(
    cust_ia = difftime(
                last_update,
                lag(last_update,default = NA),
                units = "mins"
              )
  ) %>%
  ungroup() %>%
  select(number, last_update, cust_ia)

# Temporary data frame that calculates interarrival for periods of time 
# with at least 1 check in. It is essential to remove the rows with zeroes
# since they do not affect the inter-arrival 
# Note that the first value for each station is going to be NA (since there's no previous time)
# But this is corrected later when this data is joined back
df_out <- df %>%
  filter(check_out != 0) %>%
  group_by(number, name, address) %>%
  arrange(number, last_update) %>%
  mutate(
    # Calculate time difference between each row
    service_ia = difftime(
      last_update,
      lag(last_update,default = NA),
      units = "mins"
    )
  ) %>%
  ungroup() %>%
  select(number, last_update, service_ia)

# Join the interarrival times to the original data frams and fix the issue
# with the first observation. Note that after the groupings, times with
# NA interarrival are marked as NaN
df <- df %>%
  left_join(df_in) %>%
  left_join(df_out) %>%
  group_by(number) %>%
  arrange(last_update) %>%
  # Calculate time diff between first check in/out and first observation
  mutate(
    cust_ia = if_else(
      check_in != 0 & is.na(cust_ia),
      difftime(
        last_update, last_update[1], units = "mins"
      ),
      cust_ia
    ),
    service_ia = if_else(
      check_out != 0 & is.na(service_ia),
      difftime(
        last_update, last_update[1], units = "mins"
      ),
      service_ia
    )
  ) %>%
  group_by(number, name, address, date, time) %>%
  # Add up results
  summarise(
    bike_stands = min(bike_stands),
    available_stands = last(available_bike_stands),
    prev_period_diff = sum(prev_period_diff),
    check_in = sum(check_in),
    cust_ia = mean(cust_ia, na.rm = TRUE),
    check_out = sum(check_out),
    service_ia = mean(service_ia, na.rm = TRUE)
  ) %>%
  ungroup()

# Some checks to test if the data is ok
df %>% group_by(number) %>%
  summarise(
    cust_ia = max(cust_ia, na.rm = TRUE),
    service_ia = max(service_ia, na.rm = TRUE)
  ) %>%
  left_join(df, by = c("number", "cust_ia")) %>% 
  View()

" From this you can see that there's a long time between the 7th of October
  and the 13th of october, i.e. there's missing data. What to do?
"

df %>%
  group_by(number, name) %>%
  summarise(
    cust_ia = mean(cust_ia, na.rm = TRUE),
    service_ia = mean(service_ia, na.rm = TRUE)
  ) %>% View()


# Save data frame
write_rds(df, "../saved_data_frames/db_queue_data.rds")
