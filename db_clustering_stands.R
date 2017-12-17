" 
  Author: Carlos Amaral
Date: 
Last Modified: 17/12/17
Description: 
This uses k-means clustering to cluster the data based on the number of available stands
at each station for each period of time. 
The tricky part is to get the dataset into the desired format. In particular, the name of
the columns at the desired format become the times of the day (i.e. 00:00:00, 00:10:00, etc..)
"

library(tidyverse)
library(cluster)
library(fpc)
library(stringr)
############################ Data re-shaping #############################
# Read previously processed data
df <- as.tibble(read_rds("./saved_data_frames/db_all_data.rds"))

# Filter unwanted data
df <- df %>%
  filter(Date >= "2016-10-14" & Date <= "2017-10-14")

prep_df <- df %>% 
  mutate(
    day = ifelse(Weekday == "Mon", 1,
                 ifelse(Weekday == "Tue", 2,
                        ifelse(Weekday == "Wed", 3,
                               ifelse(Weekday == "Thu", 4,
                                      ifelse(Weekday == "Fri", 5,
                                             ifelse(Weekday == "Sat", 6,
                                                    ifelse(Weekday == "Sun", 7, -1))))))),
    Time = str_replace_all(Time, ":", "_")
  ) %>%
  select(Number, day, Date,  Time, Available_stands) %>%
  group_by(Number, Date) %>%
  spread(key = Time, value = Available_stands) %>%
  ungroup() %>%
  select(-Number, -Date)
##################################### Data sanitisation ###############################
"
Due to the way the data is collected, there are periods of 10 minutes for which there is no entry
To correct this, I implement the following code, which replaced NA entried by the previoues read
or the following read in case the missing value is in the first row
"

fix_na <- function(column){
  # Iterate through rows
  for(i in 1:length(column)){
    # Apply different result if it is first row
    ifelse(
      i == 1,
      ifelse(is.na(column[i]), column[i] <- column[i+1], column[i] <- column[i]), 
      ifelse(is.na(column[i]), column[i] <- column[i-1], column[i] <- column[i])
    )
  }
  return(column)
} 

prep_df <- as.tibble(lapply(prep_df, fix_na))

########################## Label Adjustment ###################
"A series of operations to make the labels in the graph look pretty "
# Create levels for the times of the day
time_lvl_df <- df %>%
  select(Time) %>%
  distinct() %>%
  mutate(
    t_hour = as.numeric(str_extract(Time, "^\\d{1,2}")),
    t_min = as.numeric(str_extract( str_extract(Time, ":\\d+:"), "\\d+") )
  ) %>%
  arrange(t_hour,t_min)

# Vector with the levels for time i.e. order time from 00:00 to 23:50
time_lvls <- time_lvl_df$Time

# Support variables to capture time breaks for the plots
time_breaks <- time_lvl_df %>%
  filter(t_min == 0)
time_breaks <- time_breaks$Time

break_labels <- str_extract(time_breaks, "\\d+:\\d+")
########################### k-means plot for prep df ################
# k-means fit the data
n <- nrow(prep_df)
wss <- rep(0,15)
wss[1] <- (n-1) * sum(sapply(prep_df,var))

# Subsequent values can be found using this loop, note '15' is arbitrary 
for (i in 2:15)  wss[i] <- sum(
  kmeans(prep_df, centers=i)$withinss
)

# Plot wss for each number of clusters and use elbow method
cl_exam <- plot(1:15, wss, type="b", xlab="Number of Clusters",
                ylab="Within groups sum of squares")

# 4-means clustering
kfit <- kmeans(prep_df, centers = 4)

# Change format of data for plotting
k_centers <- as.tibble(kfit$centers) %>%
  select(-day) %>%
  mutate(cluster = as.factor(row_number())) %>%
  gather(key = time, value = available_stands, -cluster) %>%
  mutate( 
    time = str_replace_all(time, "_", ":"),
    time = factor(time, levels = time_lvls)
  )

## Plot the data
k_centers %>% 
  ggplot(aes(x = time, y = available_stands, colour = cluster)) +
  geom_line() +   geom_point() +
  scale_x_discrete(
    breaks = time_breaks,
    labels = break_labels
  ) + 
  #geom_point(data = centers, aes(Time, Available_stands), size = 10, alpha = 0.3) +
  xlab("Hour of day") +
  ylab("Available stands") +
  ggtitle("Clustering for all stations")



