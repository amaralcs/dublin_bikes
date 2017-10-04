library(tidyverse)
library(lubridate)

# Analysis for all stations

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

# Get all file names in the directory
file_names <- dir()
# Skip the first one as is problematic
file_names <- file_names[-1]

# Read all files into data frame
df <- do.call(rbind, lapply(file_names, read_csv))

# Remove duplicate columns (noted especially station 16 has duplicates)
df <- distinct(df)

# Calculate difference in number of bikes between periods
df <- df %>%
  # Convert POSIXct to date and split into each col
  mutate(
    last_update = as_datetime(last_update/1000, tz = "GMT"),
    year = year(last_update),
    month = month(last_update),
    day = day(last_update),
    hour = hour(last_update),
    min = minute(last_update),
    secs = seconds(last_update)
     ) %>%
  # Sort in order so we can calculate the differences
  group_by(number) %>%
  arrange(year, month, day, hour, min, secs) %>%
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
  # Maybe I don't need the split date
  select(-year, -month, -day, -hour, -min, -secs)

# Calculate running sum for check_in/out (NOT NECESSARY)
#df$run_check_in <- run_sum(df, df$check_in)
#df$run_check_out <- run_sum(df, df$check_out)

# Create column for weekday
df$weekday <- weekdays(df$last_update, abbreviate = TRUE)

# Create update start/finish columns (NOT NECESSARY)
#df$last_update_start <- df$last_update
#df$last_update_end <- lead(df$last_update) # note last row will be NA

# Creates a df calculating total check in/out per day
day_frame <- df %>%
  select(number, last_update, weekday, check_in, check_out) %>%
  mutate(
    year = year(last_update),
    month = month(last_update),
    day = day(last_update), 
    date = make_date(year, month, day)
  ) %>%
  group_by(number, date, weekday) %>% 
  summarise(
    tot_check_in = sum(check_in),
    tot_check_out = sum(check_out)
  )

# Rename columns
df <- df %>%
  rename(
    Number = number,
    Name = name,
    Address = address,
    Bike_stands = bike_stands,
    Available_stands = available_bike_stands,
    Last_update = last_update,
    Prev_period_diff = prev_period_diff,
    Check_in = check_in,
    Check_out = check_out,
    Weekday = weekday
  )
day_frame <- day_frame %>%
  rename(
    Number = number,
    Date = date, 
    Weekday = weekday,
    Tot_check_in = tot_check_in,
    Tot_check_out = tot_check_out
  )

# Group times into slots of 10 minutes each
df <- df %>%
  mutate(
    Hour = hour(Last_update),
    Min = minute(Last_update),
    Sec = second(Last_update)
  )
  

# Create field with time of day
df <- df %>%
  mutate(Time = strftime(Last_update, format = "%H:%M:%S", tz = "GMT"))

# Write output to excel file so code doesn't have to be re-run
write_rds(df, "db_all_data.rds")
write_rds(day_frame, "db_date_info.rds")

################################# Exploratory Analysis #####################################
library(ggmap)

# Change working directory - change appropriately to where rds files are
setwd("C:/Users/Carlos/Documents/Dublin Bikes Project/dublin_bikes/data_dump")

# Read previously processed data
df <- as.tibble(read_rds("db_all_data.rds"))
day_frame <- as.tibble(read_rds("db_date_info.rds"))

# Read in geospatial data for stations 
geo <- as.tibble(read_csv("db_geo.csv"))

# Add geospatial info to previous data frames
df <- geo %>%
  select(-Name, -Address) %>%
  right_join(df, by = "Number")

sort(g_quay$Time)

# Plot data for all stations (takes too long, maybe just plot for one to start)
ggplot(df) + 
  geom_line(
    mapping = aes(x = Time, y = Check_in),
    na.rm = TRUE
  ) +
  facet_wrap( ~ Weekday)
  
# Plot for one station
george_quay <- df %>% 
  filter(Number == 16) %>% 
  ggplot() +
  geom_line(
    mapping = aes(Time, Check_in),
    na.rm = TRUE
  ) +
  facet_wrap(~ Weekday)
  