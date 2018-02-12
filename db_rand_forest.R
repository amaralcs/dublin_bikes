"
  Author: Carlos Amaral
  Date: 30/01/18
  Last modified: 30/01/18
  Description: 
    
"
####################### Import packages, load data ###################################
library(tidyverse)
library(lubridate)
library(randomForest)
library(Metrics)
library(corrplot)

df <- read_rds("./saved_data_frames/db_all_data.rds") %>%
  filter(Number >1 & Number <15) %>%
  filter(Date >= "2016-10-14" & Date <= "2017-10-14")

# set a seed so that we can reproduce results
set.seed(3482)

####################### Prepare the data for random forest #################################
# Group time into 48 factors
time_df <- df %>%
  mutate(
    t_hour = as.numeric(str_extract(Time, "^\\d{1,2}")),
    t_min = as.numeric(str_extract( str_extract(Time, ":\\d+:"), "\\d+")),
    Time = as.factor(if_else( 
      t_min < 30, 
      if_else(
        t_hour < 10, 
        paste(paste("0", t_hour, sep = ""), "00", sep=":"), 
        paste(t_hour, "00", sep=":")
      ),
      if_else(
        t_hour < 10, 
        paste(paste("0", t_hour, sep = ""), "30", sep=":"), 
        paste(t_hour, "30", sep=":")
      )
    )),
    Name = as.factor(Name),
    season = as.factor(
              if_else(month(Date) == 11 |month(Date) == 12 | month(Date) == 1, "Winter",
                if_else(month(Date) == 2 |month(Date) == 3 | month(Date) == 4, "Spring",
                  if_else(month(Date) == 5 |month(Date) == 6 |month(Date) == 7, "Summer",
                    "Autumn")))
              )
  ) %>%
  group_by(
    Number, Name, Address, Bike_stands, Date, season, Weekday, Time
  ) %>%
  summarise(
    av_stands = round(mean(Available_stands))
  ) %>%
  mutate( 
    prev_stands = lag(av_stands), # number of bike stands in previous time period
    prev_stands = if_else(is.na(prev_stands), av_stands, prev_stands)
  ) %>% 
  ungroup()

# DF containing cluster info and geographical info
clusters <- read_rds("./saved_data_frames/db_clustered_stations.rds")
geo <- read_csv("./geo_data/db_geo.csv")

# Join the clusters/lat/long/holiday to the original dataset
rf_df <- time_df %>%
  left_join(clusters, by = "Number") %>%
  mutate(cluster = factor(cluster)) %>%
  rename(Name = Name.x) %>%
  select(-Name.y)

rf_df <- rf_df %>%
  left_join(geo, by = "Number") %>%
  rename(Name = Name.x, Address = Address.x) %>%
  select(-Name.y, -Address.y)

####################### Add weather information #########################
wdf <- read_rds("./saved_data_frames/db_weather.rds")

wrf_df <- rf_df %>% mutate(
  day = as.character(day(Date)),
  month = month(Date),
  year = as.character(year(Date)),
  hour = str_extract(Time, "\\d{2}")
  ) %>%
  inner_join(wdf, by = c("year", "month", "day", "hour")) %>%
  select(-date, -day, -month, -year, -hour, - minute)

wrf_df %>% filter(is.na(rain)) %>% View()

# Matrix with numerical variables to evaluate correlations
num_matrix <- wrf_df %>%
  select(av_stands,Bike_stands, Latitude, Longitude, prev_stands, rain, temp, wetb, dewpt, vappr, rhum, msl)

summary(num_matrix)
cor(num_matrix) %>% View()
corrplot(cor(num_matrix), method = "circle")
"There does not seem to be much correlation, between the weather variables and av_stands
 So for performance, I might as well not include them"
####################### Random forest model #############################

# Create training and test samples
n = floor(nrow(rf_df)/ 10)
lb = 7*n +1
ub = 10*n
subset = lb:ub

train <- rf_df[-subset,]
test <- rf_df[subset,]

"Current run time is about 1:40 hrs with these predictors"
# Run forest model and evaluate results
rf <- randomForest(av_stands ~  Weekday + Time + Name + prev_stands +
                                cluster + Latitude + Longitude + season,
                   data = train, importance = TRUE, ntree = 150)
rf
plot(rf)
rf$importance
varImpPlot(rf)

# Predict target data fram and evaluate error
test$pred <- predict(rf, test)
rmsle(test$pred, test$av_stands)
rmse(test$pred, test$av_stands)
