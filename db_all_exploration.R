"
  Author: Carlos Amaral
  Date: 4/10/17
  Last modified: 17/03/17
  Description: 
    Initial exploration of the dataset. 
    There is analysis on the days the data was collected (note the missing days),
    Variance of check ins/check outs at all stations
    Comparison between Charlemont (top station overall) and Mountjoy Square (top station on the
    north side of the city)
    Analysis of usage of least used 10 stations
    Investigates seasonality on usage of bikes.
"

library(tidyverse)
library(lubridate)
library(stringr)
library(ggmap)

################################# Exploratory Analysis #####################################

# Read previously processed data
df <- as.tibble(read_rds("./saved_data_frames/db_all_data.rds"))

# Look at the missing dates
df %>% 
  group_by(y = year(Date), m = month(Date), d = day(Date)) %>%
  summarise( n() ) %>%
  arrange(y, m, d) %>%
  View()
# Note how days 8,9,10,11,12 of October are missing, plus there's a random entry for July

# This is a random value found in the data, perhaps remove it as is shouldn't be here
df %>%
  filter(month(Date) == 7) %>% View()

# Filter out data for top 10 most used stations
top_check_ins <- df %>%
  group_by(Name) %>%
  summarise(tot_check_in = sum(Check_in)) %>%
  top_n(10, tot_check_in)
top_check_ins
# Note how there are two stations with number = 1 and no station 50

# Filter out data for top 10 most and least used stations
top_activity <- df %>%
  mutate(act = Check_in + Check_out) %>%
  group_by(Address) %>%
  summarise(tot_act = sum(act)) %>%
  top_n(10, tot_act)
top_activity

bot_activity <- df %>%
  mutate(act = Check_in + Check_out) %>%
  group_by(Address) %>%
  summarise(tot_act = sum(act)) %>%
  top_n(-10, tot_act)
bot_activity


df %>%
  group_by(Name) %>%
  summarise(tot_check_in = sum(Check_in)) %>%
  top_n(10, tot_check_in) %>%
  View()

# Semi-join to obtain the full data
top_check_ins <- semi_join(df, top_check_ins, by = "Name")

# Filter out data for top 10 most used stations
top_check_outs <- df %>%
  group_by(Name) %>%
  summarise(tot_check_out = sum(Check_out)) %>%
  top_n(10, tot_check_out)
top_check_outs

# Semi-join to obtain the full data
top_check_outs <- semi_join(df, top_check_outs, by = "Name")

# Average Usage per day
avg_usage_day <- df %>%
  group_by(Number, Name, Weekday) %>%
  summarise(
    avg_cin = mean(Check_in),
    avg_cout = mean(Check_out)
  )

# Usage per day
usage_day <- df %>%
  mutate( act = Check_in + Check_out) %>%
  group_by(Number, Name, Weekday) %>%
  summarise(
    tot_act = sum(act)
  )

# Boxplots to understand the avg distribution
avg_usage_day %>%
  ggplot(aes(Weekday, avg_cin)) +
  geom_boxplot()

avg_usage_day %>%
  ggplot(aes(Weekday, avg_cout)) +
  geom_boxplot()

usage_day %>%
  ggplot(aes(Weekday, tot_act)) +
  geom_boxplot() +
  ylab("Activity")

# Examine usage in 4 periods, morning, afternoon, evening , night
day_periods <- df %>%
  mutate(
    Hour = as.numeric(str_extract(Time, "^\\d{1,2}")),
    Period = ifelse(Hour >= 5 & Hour < 12, "Morning",
              ifelse(Hour >= 12 & Hour < 19, "Afternoon",
               ifelse(Hour >= 19, "Night", 
                "Late night")))
  ) %>%
  group_by(Number, Name, Period, Weekday) %>%
  summarise(
    tot_in = sum(Check_in),
    tot_out = sum(Check_out),
    avg_in = mean(Check_in),
    avg_out = mean(Check_out)
  )

day_periods %>%
  ggplot(aes(Period)) +
  geom_col(aes(y = tot_in), width = 0.7, fill = "blue") +
  geom_col(aes(y = tot_out), width = 0.5, fill = "red") +
  coord_flip()

# Analysing variance and deviation for each time of day
top_station <- df %>%
  filter(Number == 5)

top_station %>%  
  mutate(
  t_hour = as.numeric(str_extract(Time, "^\\d{1,2}"))
  ) %>%
  group_by(Number, t_hour) %>%
  arrange(t_hour) %>%
  summarise(
    mean_in = mean(Check_in),
    sd_in = sd(Check_in),
    mean_out = mean(Check_out),
    sd_out = sd(Check_out)
  ) %>%
  ggplot(aes(t_hour)) +
  geom_line(aes(y=mean_in), colour = "green") +
  geom_line(aes(y=mean_out), colour = "red") +
  geom_line(aes(y=sd_in), colour = "blue") +
  geom_line(aes(y=sd_out), colour = "magenta")

# Analyse top variance for stations
sd_10m <- df %>%
  group_by(Number, Name, Time) %>%
  summarise( 
    sd_in = sd(Check_in),
    mean_in = mean(Check_in),
    max_in = max(Check_in),
    min_in = min(Check_in),
    sd_out = sd(Check_out),
    mean_out = mean(Check_out),
    max_out = max(Check_out),
    min_out = min(Check_out)
  )

sd_10m_in <- sd_10m %>%
  mutate(
    t_hour = as.numeric(str_extract(Time, "^\\d{1,2}"))
  ) %>%
  filter(t_hour >= 5) %>%
  arrange(desc(sd_in), mean_in) %>%
  top_n(10, sd_in)
sd_10m_in

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
  ggtitle("Monday check ins (hourly period)")
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
  ggtitle("Monday check ins (10 min period)")
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
  ggtitle("Monday check outs (hourly period)")
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
  ggtitle("Monday check outs (10 min period)")
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
  ggtitle("Monday activity at (hourly period)")
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
  ggtitle("Monday activity at (10 min period)")
all_mondays_10m_activity


################################# Analysis for Mountjoy Square West #########################
# Dataframe containing data for both mountjoy square and Charlemont Place
comp_df <- df %>%
  filter(Number == 28 | Number == 5)

station_name <- comp_df %>%
  group_by(Name) %>%
  distinct(Name) %>%
  as.vector()

# Compare overall mean check in
ov_in <- comp_df %>% 
  group_by(Name, Time) %>%
  summarise(
    mean_in = mean(Check_in)
  ) %>%
  ggplot(aes(hms(Time), mean_in, color = Name, group = Name)) +
  geom_line(size = 1) +
  scale_x_time() +
  scale_color_discrete(
    name = "Station",
    labels = c("Charlemont", "Mountjoy")
  ) +
  ylab("Mean check in") +
  xlab("Time of day") +
  ggtitle("Overall check in comparison")
ov_in

# Compare overall mean check out
ov_out <- comp_df %>% 
  group_by(Name, Time) %>%
  summarise(
    mean_out = mean(Check_out)
  ) %>%
  ggplot(aes(hms(Time), mean_out, color = Name, group = Name)) +
  geom_line(size = 1) +
  scale_x_time()+
  scale_color_discrete(
    name = "Station",
    labels = c("Charlemont", "Mountjoy")
  ) +
  ylab("Mean check out") +
  xlab("Time of day") +
  ggtitle("Overall check out comparison")
ov_out

# Compare overall mean check in across each day
ov_in_day <- comp_df %>% 
  group_by(Name, Time, Weekday) %>%
  summarise(
    mean_in = mean(Check_in)
  ) %>%
  ggplot(aes(hms(Time), mean_in, color = Name, group = Name)) +
  geom_line(size = 1) +
  facet_wrap(~ Weekday, nrow = 2) + 
  scale_x_time() +
  scale_color_discrete(
    name = "Station",
    labels = c("Charlemont", "Mountjoy")
  ) +
  ylab("Mean check in") +
  xlab("Time of day") +
  ggtitle("Overall check in comparison")
ov_in_day

# Compare overall mean check out across each day
ov_out_day <- comp_df %>% 
  group_by(Name, Time, Weekday) %>%
  summarise(
    mean_in = mean(Check_in)
  ) %>%
  ggplot(aes(hms(Time), mean_in, color = Name, group = Name)) +
  geom_line(size = 1) +
  facet_wrap(~ Weekday, nrow = 2) + 
  scale_x_time() +
  scale_color_discrete(
    name = "Station",
    labels = c("Charlemont", "Mountjoy")
  ) +
  ylab("Mean check out") +
  xlab("Time of day") +
  ggtitle("Overall check out comparison")
ov_out_day

# Investigating usage across Saturday and Sunday morning
sat_sun <- comp_df %>%
  mutate(
    t_hour = as.numeric(str_extract(Time, "^\\d{1,2}")),
    t_min = as.numeric(str_extract(Time, "\\d{2}\\b"))
  ) %>%
  filter(Weekday == "Sun" | Weekday == "Sat") %>%
  filter(t_hour >5 & t_hour <10) %>%
  group_by(Name, Time, Weekday)  %>%
  summarise(
    max_in = max(Check_in),
    max_out = max(Check_out),
    mean_in = mean(Check_in),
    mean_out = mean(Check_out)
  ) %>%
  ggplot(aes(hms(Time))) +
  scale_x_time() +
  #geom_point(aes(y=max_in, fill=Name), size=2, shape=24) +
  geom_line(aes(y=mean_in, colour=Name), size = 1)  +
  #geom_point(aes(y=max_out, colour=Name), size=2, shape=25) +
  geom_line(aes(y=mean_out, colour=Name), linetype= "dashed", size = 1)  +
  scale_colour_discrete(
    name = "Mean of station",
    labels = c("Charlemont", "Mountjoy")
  ) +
  facet_wrap(~Weekday, nrow = 2)
sat_sun

setwd("./plots")
ggsave("mjoy_charlemont_in.png", ov_in, width = 30, units = "cm")
ggsave("mjoy_charlemont_out.png", ov_out, width = 30, units = "cm")
ggsave("mjoy_charlemont_in_per_day.png", ov_in_day, width = 30, units = "cm")
ggsave("mjoy_charlemont_out_per_day.png", ov_out_day, width = 30, units = "cm")
ggsave("mjoy_charlemont_sat_sun.png", sat_sun, width = 30, units = "cm")

"Overall conclusion is that Mountjoy has a more constant check out rate throughout the day
 while Charlemont has busier peaks.
 On the check in side, the same pattern appears with charlemont having more check ins at peak
 times."
################################# Locations of top/bottom stations ####################################
# Data frame with stations of interest (i.e busiest and least busy)
act_df <- df %>%
  inner_join(rbind(top_activity, bot_activity), by = "Address") %>%
  group_by(Number, Address, tot_act) %>%
  summarise() %>%
  ungroup() %>%
  mutate(
    rank = if_else(tot_act > 50000, "Most Active", "Least Active")
  )

geo_df <- read_csv("./geo_data/db_geo.csv")
act_df <- act_df %>%
  left_join(geo_df)

dub_map <- ggmap(
  get_googlemap(
    center = c(-6.270,53.345),
    scale = 2,
    zoom = 13,
    size = c(540,400)
  )
) +
  coord_fixed(ratio = 1.3)

top_bot_act <- dub_map +
  geom_point(
    data = act_df,
    aes(Longitude, Latitude, colour = rank),
    size = 5,
    alpha = 0.8
  ) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position = "bottom"
  ) +
  scale_colour_manual(
    breaks = rank,
    values = c("#FF0000", "#3399FF")
  )
ggsave("./plots/geoplots/top_bottom_activity.png", top_bot_act)

################################# Seasonality effects ###########################
library(reshape)
# Investigate wheter seasonality has any effect on usage
season_df <- df %>%
  filter(Date >= "2016-10-14" & Date <= "2017-10-14") %>%
  #filter(Number == 5) %>%
  mutate(
    season = factor(
      if_else(month(Date) == 11 |month(Date) == 12 | month(Date) == 1, "Winter",
              if_else(month(Date) == 2 |month(Date) == 3 | month(Date) == 4, "Spring",
                      if_else(month(Date) == 5 |month(Date) == 6 |month(Date) == 7, "Summer",
                              "Autumn"))),
      levels = c("Winter", "Spring", "Summer", "Autumn") 
    ),
    activity = Check_in + Check_out
  ) %>%
  select(Weekday, season, activity) %>%
  melt(id = c("Weekday", "season")) %>%
  group_by(Weekday, season) %>%
  summarise( 
    activity = sum(value)
  ) %>%
  ungroup()

season_plot <- season_df %>%
  ggplot(aes(season, activity, fill=season)) +
  geom_col() +
  facet_wrap(~ Weekday, nrow = 2) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
season_plot

ggsave("./plots/season_lplot.png", season_plot)
