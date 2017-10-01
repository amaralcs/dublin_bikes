library(tidyverse)
library(lubridate)

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

# Change working directory - change appropriately to where csv files are
setwd("C:/Users/Carlos/Documents/Dublin Bikes Project/data_dump")

# Read in csv file as tibble
df <- read_csv("export5.csv")

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
  arrange(year, month, day, hour, min, secs) %>%
  # Calculate differences
  mutate(prev_period_diff = available_bike_stands - lag(available_bike_stands, default = available_bike_stands[1])) %>%
  # Apply functions to determine checked in/out 
  rowwise() %>%
  mutate(
    check_in = c_check_in(prev_period_diff),
    check_out = c_check_out(prev_period_diff)
     ) %>%
  # Maybe I don't need the split date
  select(-year, -month, -day, -hour, -min, -secs)

# Function to cumulatively sum check in/out
run_sum <- function(col){
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

# Calculate running sum for check_in/out
df$run_check_in <- run_sum(df$check_in)
df$run_check_out <- run_sum(df$check_out)

# Create column for weekday
df$weekday <- weekdays(df$last_update, abbreviate = TRUE)

# Create update start/finish columns
df$last_update_start <- df$last_update
df$last_update_end <- lead(df$last_update) # note last row will be NA

# Creates a df calculating total check in/out per day
day_frame <- df %>%
  select(number, last_update, weekday, check_in, run_check_in, check_out, run_check_out) %>%
  mutate(
    year = year(last_update),
    month = month(last_update),
    day = day(last_update), 
    date = make_date(year, month, day)
  ) %>%
  group_by(date, weekday) %>% 
  summarise(
    tot_check_in = sum(check_in),
    tot_check_out = sum(check_out)
  )

y_range <- range(0, df$check_out)
y_range <- c(0, 250)
plot(day_frame$tot_check_out, type="o", col="blue", ylim=y_range ,axes=FALSE,ann=FALSE)
axis(1, at = 1:length(day_frame$tot_check_out))
axis(2, las = 1, at = 25*0:y_range[2])
box()
lines(day_frame$tot_check_in, type = "o", pch = 22, lty = 2, col = "red")
title(main = "Daily Data", col.main = "black", font.main = 4)
title(xlab = "Days", col.lab = "black")
title(ylab = "Values", col.lab = "black")
legend(1, y_range[2], c("Check Out", "Check In"), cex=0.8, 
       col = c("blue","red"), pch = 21:22, lty = 1:2)

day_frame %>%
  ggplot() + 
  geom_smooth(mapping = aes(x = day, y = tot_check_in) )
  