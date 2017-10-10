library(tidyverse)
library(lubridate)
library(ggmap)
library(stringr)

# Analysis for a single station
 
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

# Support function to cumulatively sum check in/out
# Args: 
# df <- the data fram being used
# col <- the column for which the sum is beinc calculated
run_sum <- function(df, col){
  len <- nrow(df)
  r_sum <- c(col[1])
  
  # Loop through all rows
  for(i in 2:len){
    # If there was no check in/out, append zero
    if(col[i] == 0){
      r_sum <- append(r_sum, 0)
    }
    # Else append the sum of previous r_sum + current check_in/out value
    else{
      r_sum <- append(r_sum, r_sum[i-1] + col[i])
    }
  }
  # return list with running sum
  return(r_sum)
}

############################ Initial data Prepation##################################

# Change working directory - change appropriately to where csv files are
setwd("C:/Users/Carlos/Documents/Dublin Bikes Project/dublin_bikes/data_dump")

# Read in csv file as tibble
df <- read_csv("export5.csv")

# Remove duplicate columns (noted especially station 16 has duplicates)
df <- distinct(df)

# Calculate difference in number of bikes between periods
df <- df %>%
  # Convert POSIXct to date and split into each col
  mutate(
    last_update = as_datetime(last_update/1000, tz = "GMT"),
    Year = year(last_update),
    Month = month(last_update),
    Day = day(last_update),
    Hour = as.character(hour(last_update)),
    Min = minute(last_update),
    Sec = "00"
  ) %>%
  # Standardise minutes i.e. group them in 10 minute slots and make new time
  mutate(
    Min = ifelse(Min < 10, "00",
            ifelse(Min < 20, "10",
             ifelse(Min < 30, "20",
              ifelse(Min < 40, "30",
               ifelse(Min < 50, "40",
                ifelse(Min < 60, "50",
                 NA)))))
     ),
    Date = ymd(paste(Year, Month, Day, sep = "-")),
    Time = paste(Hour, Min, Sec, sep = ":"),
    Weekday = weekdays(Date, abbreviate = TRUE)
   ) %>%
  # Sort in order so we can calculate the differences
  arrange(Year, Month, Day, Hour, Min, Sec) %>%
  # Calculate differences
  mutate(
    prev_period_diff = 
      available_bike_stands - lag(available_bike_stands, default = available_bike_stands[1])) %>%
  # Apply functions to determine checked in/out 
  rowwise() %>%
  mutate(
    check_in = c_check_in(prev_period_diff),
    check_out = c_check_out(prev_period_diff)
  ) %>%
  # Group results in slots of 10 minutes
  group_by(number, name, address, Date, Time, Weekday) %>%
  # Add up results
  summarise(
    bike_stands = min(bike_stands),
    prev_period_diff = sum(prev_period_diff),
    check_in = sum(check_in),
    check_out = sum(check_out)
  ) %>%
  ungroup()

# Rename columns
df <- df %>%
  rename(
    Number = number,
    Name = name,
    Address = address,
    Bike_stands = bike_stands,
    Prev_period_diff = prev_period_diff,
    Check_in = check_in,
    Check_out = check_out
  )

# Add factor level information to weekdays
days_level <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
df <- df %>%
  mutate(Weekday = factor(Weekday, levels = days_level))
  
  

# Write output to excel file so code doesn't have to be re-run
write_rds(df, "db_single_station_data.rds")

################################# Exploratory Analysis #####################################


# Read in geospatial data for stations 
geo <- as.tibble(read_csv("db_geo.csv"))

# Change working directory - change appropriately to where rds files are
setwd("C:/Users/Carlos/Documents/Dublin Bikes Project/dublin_bikes/data_dump")

# Read previously processed data
df <- as.tibble(read_rds("db_single_station_data.rds"))

################################# Exploratory plots #############################
# Monday check ins plotted treating data in hourly min periods
all_mondays_60m_in <- df %>%
  mutate(
    t_hour = as.numeric(str_extract(Time, "^\\d{1,2}")),
    t_min = as.numeric(str_extract(Time, "\\d{2}\\b"))
  ) %>%
  filter(
    #t_hour >= 5 & 
    #(t_hour == 0 & t_min < 30) &
    Weekday == "Mon"
  ) %>%
  ggplot(aes(t_hour, Check_in)) + 
  geom_col(fill = "blue") +
  scale_x_time(breaks = c(0:23), labels = c(0:23)) +
  xlab("Hour") +
  ylab("Check ins") +
  #facet_wrap(~month(Date, label = TRUE)) +
  ggtitle("Monday check ins at Charlemont Place (hourly period)")
all_mondays_60m_in
# This seems to indicate there is a high number of check ins at around 7am, 10am and 5pm

# Monday check ins plotted treating data in 10 min periods
all_mondays_10m_in <- df %>%
  mutate(
    t_hour = as.numeric(str_extract(Time, "^\\d{1,2}")),
    t_min = as.numeric(str_extract(Time, "\\d{2}\\b"))
  ) %>%
  filter(
    #t_hour >= 5 & 
    #(t_hour == 0 & t_min < 30) &
    Weekday == "Mon"
  ) %>%
  ggplot(aes(hms(Time), Check_in)) + 
  geom_col(fill = "blue") +
  scale_x_time() +
  xlab("Hour") +
  ylab("Check ins") +
  #facet_wrap(~month(Date, label = TRUE)) +
  ggtitle("Monday check ins at Charlemont Place (10 min period)")
all_mondays_10m_in

# Monday check outs plotted treating data in hourly min periods
all_mondays_60m_out <- df %>%
  mutate(
    t_hour = as.numeric(str_extract(Time, "^\\d{1,2}")),
    t_min = as.numeric(str_extract(Time, "\\d{2}\\b"))
  ) %>%
  filter(
    #t_hour >= 5 & 
    #(t_hour == 0 & t_min < 30) &
    Weekday == "Mon"
  ) %>%
  ggplot(aes(t_hour, Check_out)) + 
  geom_col(fill = "green") +
  scale_x_time(breaks = c(0:23), labels = c(0:23)) +
  xlab("Hour") +
  ylab("Check outs") +
  #facet_wrap(~month(Date, label = TRUE)) +
  ggtitle("Monday check outs at Charlemont Place (hourly period)")
all_mondays_60m_out
# This seems to indicate there is a high number of check ins at around 7am, 10am and 5pm

# Monday check outs plotted treating data in 10 min periods
all_mondays_10m_out <- df %>%
  mutate(
    t_hour = as.numeric(str_extract(Time, "^\\d{1,2}")),
    t_min = as.numeric(str_extract(Time, "\\d{2}\\b"))
  ) %>%
  filter(
    #t_hour >= 5 & 
    #(t_hour == 0 & t_min < 30) &
    Weekday == "Mon"
  ) %>%
  ggplot(aes(hms(Time), Check_out)) + 
  geom_col(fill = "green") +
  scale_x_time() +
  xlab("Hour") +
  ylab("Check outs") +
  #facet_wrap(~month(Date, label = TRUE)) +
  ggtitle("Monday check outs at Charlemont Place (10 min period)")
all_mondays_10m_out

# Monday activity plotted treating data in hourly min periods
all_mondays_60m_activity <- df %>%
  mutate(
    t_hour = as.numeric(str_extract(Time, "^\\d{1,2}")),
    t_min = as.numeric(str_extract(Time, "\\d{2}\\b")),
    total_checks = Check_in + Check_out
  ) %>%
  filter(
    #t_hour >= 5 & 
    #(t_hour == 0 & t_min < 30) &
    Weekday == "Mon"
  ) %>%
  ggplot(aes(t_hour, total_checks)) + 
  geom_col(fill = "red") +
  scale_x_time(breaks = c(0:23), labels = c(0:23)) +
  xlab("Hour") +
  ylab("Activity") +
  #facet_wrap(~month(Date, label = TRUE)) +
  ggtitle("Monday activity at Charlemont Place (hourly period)")
all_mondays_60m_activity
# This seems to indicate there is a high number of check ins at around 7am, 10am and 5pm

# Monday check outs plotted treating data in 10 min periods
all_mondays_10m_activity <- df %>%
  mutate(
    t_hour = as.numeric(str_extract(Time, "^\\d{1,2}")),
    t_min = as.numeric(str_extract(Time, "\\d{2}\\b")),
    total_checks = Check_in + Check_out
  ) %>%
  filter(
    #t_hour >= 5 & 
    #(t_hour == 0 & t_min < 30) &
    Weekday == "Mon"
  ) %>%
  ggplot(aes(hms(Time), total_checks)) + 
  geom_col(fill = "red") +
  scale_x_time() +
  xlab("Hour") +
  ylab("Activity") +
  #facet_wrap(~month(Date, label = TRUE)) +
  ggtitle("Monday activity at Charlemont Place (10 min period)")
all_mondays_10m_activity
