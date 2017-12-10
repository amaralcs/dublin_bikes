"
  Author: Carlos Amaral
  Date: 10/12/17
  Last modified: 10/12/17
  Description: 
    This plots the usage of bikes relative to the geo-spatial position of the stations in Dublin
"

library(tidyverse)
library(ggmap)
library(stringr)

############################# Import and Prep ###############################
df <- read_rds("./saved_data_frames/db_all_data.rds")
geo <- read_csv("./geo_data/db_geo.csv")

# Drop unnecessary columns for geo
geo <- geo %>%
  select(Number, Latitude, Longitude)

# Test the join, see if all data is still there
df %>%
  left_join(geo, by = "Number") %>%
  group_by(Number) %>%
  filter(
    is.na(Latitude) | is.na(Longitude)
  ) %>%
  summarise( count = n()) %>%
  View()
" Note how station 20 (James Street East) doesn't match anything. 
  Maybe station was merged with station 75 (James Street)
"

# Do the join
df <- df %>%
  left_join(geo, by = "Number") 

############################### Plot of Dublin ################################
# Map of dublin, this is the initial plot, we add on to it as we desire
dub_map <- ggmap(
  get_googlemap(
    center = c(-6.270,53.345),
    scale = 2,
    zoom = 13,
    size = c(540,400)
    )
  ) +
  coord_fixed(ratio = 1.3)

################################ Various Analysis ##############################
" Plot Mean available stands for weekdays/weekends during morning, afternoon, evening"
# Weekdays
week_morning <- df %>%  
  mutate(
    t_hour = as.numeric(str_extract(Time, "^\\d{1,2}")),
    t_min = as.numeric(str_extract(Time, "\\d{2}\\b"))
  ) %>%
  filter(Weekday != "Sun" & Weekday != "Sat") %>%
  filter(t_hour >= 5 & t_hour <=10) %>%
  group_by(Number, Name, Latitude, Longitude) %>%
  summarise(
    stands = mean(Available_stands)
  )
week_afternoon <- df %>%  
  mutate(
    t_hour = as.numeric(str_extract(Time, "^\\d{1,2}")),
    t_min = as.numeric(str_extract(Time, "\\d{2}\\b"))
  ) %>%
  filter(Weekday != "Sun" & Weekday != "Sat") %>%
  filter(t_hour >= 11 & t_hour <=16) %>%
  group_by(Number, Name, Latitude, Longitude) %>%
  summarise(
    stands = mean(Available_stands)
  )
week_evening <- df %>%  
  mutate(
    t_hour = as.numeric(str_extract(Time, "^\\d{1,2}")),
    t_min = as.numeric(str_extract(Time, "\\d{2}\\b"))
  ) %>%
  filter(Weekday != "Sun" & Weekday != "Sat") %>%
  filter(t_hour >= 17 & t_hour <=22) %>%
  group_by(Number, Name, Latitude, Longitude) %>%
  summarise(
    stands = mean(Available_stands)
  )

" Plot Weekday Morning"
wk_mor_plot <- dub_map + 
  geom_point(
    data = week_morning,
    aes(x=Longitude , y = Latitude),
    col = "red",
    alpha = 0.3,
    size = week_morning$stands *.4 # .4 is a scaling amount so circles aren't too big
  ) +
  scale_size_continuous(range = range(week_morning$stands),
                        name = "Available stands") + 
  ggtitle(label = "Mean available stands (Weekday - Morning)") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()
        ) 

" Plot Weekday afternoon"
wk_aft_plot <- dub_map + 
  geom_point(
    data = week_afternoon,
    aes(x=Longitude , y = Latitude),
    col = "red",
    alpha = 0.3,
    size = week_afternoon$stands *.4 # .4 is a scaling amount so circles aren't too big
  ) +
  scale_size_continuous(range = range(week_afternoon$stands),
                        name = "Available stands") + 
  ggtitle(label = "Mean available stands (Weekday - Afternoon)") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()
  ) 

" Plot Weekday evening"
wk_eve_plot <- dub_map + 
  geom_point(
    data = week_evening,
    aes(x=Longitude , y = Latitude),
    col = "red",
    alpha = 0.3,
    size = week_evening$stands *.4 # .4 is a scaling amount so circles aren't too big
  ) +
  scale_size_continuous(range = range(week_evening$stands),
                        name = "Available stands") + 
  ggtitle(label = "Mean available stands (Weekday - Evening)") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()
  ) 

# Save plots for weekday
ggsave("./plots/geoplots/1_week_morning.png", wk_mor_plot)
ggsave("./plots/geoplots/2_week_afternoon.png", wk_aft_plot)
ggsave("./plots/geoplots/3_week_evening.png", wk_eve_plot)


# Weekends
weekend_morning <- df %>%  
  mutate(
    t_hour = as.numeric(str_extract(Time, "^\\d{1,2}")),
    t_min = as.numeric(str_extract(Time, "\\d{2}\\b"))
  ) %>%
  filter(Weekday == "Sun" | Weekday == "Sat") %>%
  filter(t_hour >= 5 & t_hour <=10) %>%
  group_by(Number, Name, Latitude, Longitude) %>%
  summarise(
    stands = mean(Available_stands)
  )
weekend_afternoon <- df %>%  
  mutate(
    t_hour = as.numeric(str_extract(Time, "^\\d{1,2}")),
    t_min = as.numeric(str_extract(Time, "\\d{2}\\b"))
  ) %>%
  filter(Weekday == "Sun" | Weekday == "Sat") %>%
  filter(t_hour >= 11 & t_hour <=16) %>%
  group_by(Number, Name, Latitude, Longitude) %>%
  summarise(
    stands = mean(Available_stands)
  )
weekend_evening <- df %>%  
  mutate(
    t_hour = as.numeric(str_extract(Time, "^\\d{1,2}")),
    t_min = as.numeric(str_extract(Time, "\\d{2}\\b"))
  ) %>%
  filter(Weekday == "Sun" | Weekday == "Sat") %>%
  filter(t_hour >= 17 & t_hour <=22) %>%
  group_by(Number, Name, Latitude, Longitude) %>%
  summarise(
    stands = mean(Available_stands)
  )

" Plot for Weekend Morning"
wkend_mor_plot <- dub_map + 
  geom_point(
    data = weekend_morning,
    aes(x=Longitude , y = Latitude),
    col = "Blue",
    alpha = 0.3,
    size = weekend_morning$stands *.4 # .4 is a scaling amount so circles aren't too big
  ) +
  scale_size_continuous(range = range(weekend_morning$stands)) +
  ggtitle(label = "Mean available stands (Weekend - Morning)") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()
  ) 

" Plot for Weekend Afternoon "
wkend_aft_plot <- dub_map + 
  geom_point(
    data = weekend_afternoon,
    aes(x=Longitude , y = Latitude),
    col = "Blue",
    alpha = 0.3,
    size = weekend_afternoon$stands *.4 # .4 is a scaling amount so circles aren't too big
  ) +
  scale_size_continuous(range = range(weekend_afternoon$stands)) +
  ggtitle(label = "Mean available stands (Weekend - Afternoon)") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()
  )

" Plot for Weekend Evening "
wkend_eve_plot <- dub_map + 
  geom_point(
    data = weekend_evening,
    aes(x=Longitude , y = Latitude),
    col = "Blue",
    alpha = 0.3,
    size = weekend_evening$stands *.4 # .4 is a scaling amount so circles aren't too big
  ) +
  scale_size_continuous(range = range(weekend_evening$stands)) +
  ggtitle(label = "Mean available stands (Weekend - Evening)") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()
  )

# Save plots for weekend
ggsave("./plots/geoplots/1_weekend_morning.png", wkend_mor_plot)
ggsave("./plots/geoplots/2_weekend_afternoon.png", wkend_aft_plot)
ggsave("./plots/geoplots/3_weekend_evening.png", wkend_eve_plot)

wkend_mor_plot
wkend_aft_plot
wkend_eve_plot

" Top/Bottom 15 usage Overall"
top_all <- df %>% 
  group_by(Number, Latitude, Longitude) %>%
  summarise(
    check_in = sum(check_in),
    check_out = sum(check_out)
  )

