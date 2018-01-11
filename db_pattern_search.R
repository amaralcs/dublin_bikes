"
    Author: Carlos Amaral
    Date: 22/10/17
    Last modified:17/11/17
    Description: 
      This files attempts to offer an alternative to the clustering solution. It plots
      a heatmap for each station highlighting the number of available stands at each time interval
      per day of the week, this allows us to look at the plots and identify stations that
      behave similarly.
"

library(tidyverse)
library(lubridate)
library(forcats)
library(stringr)
library(ggExtra)
library(viridis)

############################ Heatmap Plot ##############################
# Change working directory - change appropriately to where rds files are
setwd("./saved_data_frames")

# Read previously processed data
df <- as.tibble(read_rds("db_all_data.rds"))

# Filter unwanted data
df <- df %>%
  filter(Date >= "2016-10-14" & Date <= "2017-10-14")

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

# heatmap usage plot for charlemont station
ch_htmap <- df %>%
  filter(Number == 5) %>%
  mutate(    
    Time = factor(Time, levels = time_lvls),
    available_bikes = Bike_stands - Available_stands
  ) %>%
  group_by(Time, Weekday) %>%
  mutate(avg_bikes = mean(available_bikes)) %>%
  ggplot(aes(Time, Weekday)) +
  geom_tile(aes(fill = avg_bikes), height = 0.98) +
  scale_fill_viridis(
    option = "C",
    name = "Bikes available",
    limits = c(0,40)
  ) +
  removeGrid()+
  scale_x_discrete(
    breaks = time_breaks,
    labels = break_labels
  ) + 
  xlab("Hour") +
  ylab("Day") +
  theme(axis.text.x = element_text(angle = 90))+
  ggtitle("Mean available bikes (Charlemont)")
ch_htmap  

# heatmap usage plot for all stations
all_htmap <- df %>%
  mutate(    
    Time = factor(Time, levels = time_lvls)
  ) %>%
  group_by(Time, Weekday) %>%
  mutate(avg_stands = mean(Available_stands)) %>%
  ggplot(aes(Time, Weekday)) +
  geom_tile(aes(fill = avg_stands), height = 0.98) +
  scale_fill_viridis(
    option = "C",
    name = "Free stands"
  ) +
  removeGrid()+
  scale_x_discrete(
    breaks = time_breaks,
    labels = break_labels
  ) + 
  xlab("Hour of day") +
  ylab("Day of the week") +
  theme(axis.text.x = element_text(angle = 90))+
  ggtitle("Mean available stands (all stations)")

setwd("../plots/heatmaps")
ggsave("heatmap_charlemont.png", ch_htmap)
ggsave("heatmap_all.png", all_htmap)

############################## plot a heatmap for each each station #################
setwd("../plots/heatmaps")
# Create list with number of all stations
stations <- df %>%
  select(Number) %>%
  distinct()
stations <- stations$Number

station_names<- df %>%
  select(Name) %>%
  distinct()
station_names <- station_names$Name

# Plot a heatmap for each station
for(i in 1:length(stations)){
  the_number <- i
  the_name <- station_names[i]
    
  htmap <- df %>%
    filter(Number == i) %>%
    mutate(    
      Time = factor(Time, levels = time_lvls)
    ) %>%
    group_by(Time, Weekday, Name) %>%
    mutate(avg_stands = mean(Available_stands)) %>%
    ggplot(aes(Time, Weekday)) +
    geom_tile(aes(fill = avg_stands), height = 0.98) +
    scale_fill_viridis(
      option = "C",
      name = "Free stands"
    ) +
    removeGrid()+
    scale_x_discrete(
      breaks = time_breaks,
      labels = break_labels
    ) + 
    xlab("Hour of day") +
    ylab("Day of the week") +
    theme(axis.text.x = element_text(angle = 90)) +
    ggtitle(paste("Mean available stands (", the_name, ")") )
  
  ggsave(paste("heatmap_", the_number, ".png"), htmap)
}
  