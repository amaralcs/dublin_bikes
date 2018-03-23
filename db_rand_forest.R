"
  Author: Carlos Amaral
  Date: 30/01/18
  Last modified: 23/03/18
  Description: 
    Loads previous data and implements a random forest algorithm to predict the number of bikes at a station
    Tests the prediction on an unseen set of data
    Also produces plots with a bound for the predicted number of bikes
    And plots the results in two ways: one for service level analysis and a geographical plot
    to display the location of the sufficient/insufficient stations
"
####################### Import packages set seed ###################################
library(tidyverse)
library(lubridate)
library(randomForest)
library(Metrics)
library(corrplot)
library(grid)
library(gridExtra)

# set a seed so that we can reproduce results
set.seed(3482)

###################### Analyse top/bottom stations in each cluster ###################
clusters <- read_rds("./saved_data_frames/db_clustered_stations.rds")
all <- read_rds("./saved_data_frames/db_all_data.rds") %>%
  filter(Date >= "2016-10-14" & Date <= "2017-10-14")

all_clust <- all %>%
  left_join(clusters, by = "Number") %>%
  mutate(cluster = factor(cluster)) %>%
  select(-Name.y, Name = Name.x)

# Find the most active station per cluster
top_stations <- all_clust %>%
  mutate( activity = Check_in + Check_out) %>%
  group_by(Number, cluster) %>%
  summarise( tot_act = sum(activity)) %>%
  ungroup() %>%
  group_by(cluster) %>%
  top_n(1, tot_act) %>%
  ungroup()

# find the least active station per cluster
bot_stations <- all_clust %>%
  mutate( activity = Check_in + Check_out) %>%
  group_by(Number, cluster) %>%
  summarise( tot_act = sum(activity)) %>%
  ungroup() %>%
  group_by(cluster) %>%
  top_n(-1, tot_act) %>%
  ungroup()

# gets the station numbers for most active and least active into a list
selected <- rbind(top_stations, bot_stations) %>%
  pull(Number)

# Randomly select some other 3 stations in each cluster for the random forest modelling
for(clust_num in 1:4){
  n <- all_clust %>%
    filter(cluster == clust_num) %>%
    filter(!(Number %in% selected)) %>%
    group_by(Number, cluster) %>%
    summarise() %>%
    ungroup() %>%
    sample_n(3) %>% 
    pull(Number)
  selected <- append(selected, n)
}
####################### Prepare the data for random forest #################################
# filter the full dataset based on the selection above
df <- all_clust %>%
  filter(Number %in% selected)

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
    #Name = as.factor(Name),
    season = as.factor(
              if_else(month(Date) == 11 |month(Date) == 12 | month(Date) == 1, "Winter",
                if_else(month(Date) == 2 |month(Date) == 3 | month(Date) == 4, "Spring",
                  if_else(month(Date) == 5 |month(Date) == 6 |month(Date) == 7, "Summer",
                    "Autumn")))
              ),
    av_bikes = Bike_stands - Available_stands
  ) %>%
  group_by(
    Number, Name, Address, Bike_stands, Date, season, Weekday, Time
  ) %>%
  summarise(
    av_bikes = round(mean(av_bikes)),
    cluster = first(cluster)
  ) %>%
  mutate( 
    cluster = as.factor(cluster), 
    prev_bike_num = lag(av_bikes), # number of bikes in previous time period
    prev_bike_num = if_else(is.na(prev_bike_num), av_bikes, prev_bike_num)
  ) %>% 
  ungroup()

# DF containing geographical info
geo <- read_csv("./geo_data/db_geo.csv")

# Join the geographical info to the original dataset
rf_df <- time_df %>%
  left_join(geo, by = "Number") %>%
  select(-Name.y, -Address.y, Name = Name.x, Address = Address.x)

####################### Examine weather information #########################
# wdf <- read_rds("./saved_data_frames/db_weather.rds")
# 
# wrf_df <- rf_df %>% mutate(
#   day = as.character(day(Date)),
#   month = month(Date),
#   year = as.character(year(Date)),
#   hour = str_extract(Time, "\\d{2}")
#   ) %>%
#   inner_join(wdf, by = c("year", "month", "day", "hour")) %>%
#   select(-date, -day, -month, -year, -hour, - minute)
# 
# wrf_df %>% filter(is.na(rain)) %>% View()
# 
# # Matrix with numerical variables to evaluate correlations
# num_matrix <- wrf_df %>%
#   select(av_bikes, rain, temp, 
#          wetb,  dewpt, vappr, 
#          rhum, msl)
# 
# summary(num_matrix)
# cor(num_matrix) %>% View()
# col_pal <- colorRampPalette(c("#67001F", "#B2182B", "#D6604D", "#F4A582",
#                               "#FDDBC7", "#a8a8a8", "#D1E5F0", "#92C5DE",
#                               "#4393C3", "#2166AC", "#053061"))
# corrplot(cor(num_matrix), type= "upper",  method = "number", tl.pos = 'd', cl.pos = 'b', col = col_pal(50))
# "There does not seem to be much correlation, between the weather variables and av_stands
#  So for performance, I might as well not include them"
####################### Random forest model #############################

# Create training and test samples
n = floor(nrow(rf_df)/ 10)
lb = 7*n +1
ub = 10*n
subset = lb:ub

train <- rf_df[-subset,]
test <- rf_df[subset,]

"Current run time is about 10:10 hrs with these predictors"
# Run forest model and evaluate results
start.time <- Sys.time()
rf <- randomForest(av_bikes ~  Weekday + Time + prev_bike_num +
                                cluster + Latitude + Longitude + season,
                   data = train, importance = TRUE, ntree = 200)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

rf
plot(rf)
importance(rf)
varImpPlot(rf)
write_rds(rf, "./saved_data_frames/rf_model.rds") # Save rf to avoid running it again

rf <- read_rds("./saved_data_frames/rf_model.rds") # Import model

# Predict target data fram and evaluate error
test$pred <- predict(rf, test)
rmsle(test$pred, test$av_bikes)
rmse(test$pred, test$av_bikes)

######################################## Plot results ############################
# Plot prediction to compare it against actual results
test$pred <- round(test$pred)

rf_err <- rmse(test$pred, test$av_bikes)
rf_err

time_breaks <- test %>%
  filter(str_detect(Time, "\\d{2}:00")) %>%
  group_by(Time) %>%
  summarise() %>%
  pull(Time)

####################################################################################
# Label for the error
err_label <- paste("Error margin:", round(rf_err, 2))

# plot means per stations
base_plot <- test %>%
  group_by(Number, Address, Time) %>%
  summarise(
    mean_av_bikes = mean(av_bikes),
    mean_pred = mean(pred),
    tot_stands = max(Bike_stands)
  ) %>%
  ggplot(aes(Time, mean_av_bikes, group = 1)) +
  theme_minimal() +
  geom_line(aes(y = tot_stands, colour = "Total stands"), linetype = 6) +
  geom_line(aes(colour = "Actual"), size = 1.1) +
  geom_line(aes(y = mean_pred, group = 1, colour = "Predicted"), size = 1.1) +
  geom_ribbon(aes(ymin = mean_pred - rf_err, ymax = mean_pred + rf_err), fill = "grey30", alpha = 0.2) +
  theme(axis.text.x = element_text(angle = 90, size = 7)) + 
  scale_x_discrete(
    breaks = time_breaks,
    labels = time_breaks
  ) + 
  ggtitle("Random forest prediction and actual number") +
  ylab("Mean available bikes") +
  scale_colour_manual(
    breaks = c("Predicted", "Actual", "Total"),
    values = c("Predicted"="red", "Actual"="blue", "Total stands"="black")
  ) +
  facet_wrap(~ Address) +
  theme(
    legend.box.background = element_rect(),
    legend.position = "bottom"
  )
base_plot

# Pull the station names as a factor level and create a label df for the annotation
ann_levels <- test %>% group_by(Address) %>% summarise() %>% pull(Address) %>% as.factor()
ann_text <- tibble(Time = "09:00", mean_av_bikes = 33, 
                   Address = factor("Royal Hospital", levels = ann_levels))
test_set_mean_pred <- base_plot + 
  geom_label(data = ann_text, label = err_label, size = 4)
test_set_mean_pred

ggsave("./plots/predictions/test_mean_prediction.png", test_set_mean_pred)

####################################################################################
# Label for the error
err_label <- paste("Error margin:", round(rf_err, 2))

# plot means per date for one of the stations
base_weekday <- test %>%
  filter(Number == 93) %>% # 93 is houston central
  group_by(Address, Weekday, Time) %>%
  summarise(
    mean_av_bikes = mean(av_bikes),
    mean_pred = mean(pred),
    tot_stands = max(Bike_stands)
  ) %>%
  ggplot(aes(Time, mean_av_bikes, group = 1)) +
  theme_minimal() +
  geom_line(aes(y = tot_stands, colour = "Total stands"), linetype = 6) +
  geom_line(aes(colour = "Actual"), size = 1.1) +
  geom_line(aes(y = mean_pred, group = 1, colour = "Predicted"), size = 1.1) +
  geom_ribbon(aes(ymin = mean_pred - rf_err, ymax = mean_pred + rf_err), fill = "grey30", alpha = 0.2) +
  theme(axis.text.x = element_text(angle = 90, size = 9)) +
  scale_x_discrete(
    breaks = time_breaks,
    labels = time_breaks
  ) + 
  ggtitle("Random forest prediction and actual number for Houston Station (Central)") +
  ylab("Mean available bikes") +
  scale_colour_manual(
    breaks = c("Predicted", "Actual", "Total"),
    values = c("Predicted"="red", "Actual"="blue", "Total stands"="black")
  ) +
  facet_wrap(~ Weekday, nrow = 2, ncol = 5) +
  theme(
    legend.box.background = element_rect()
  )
base_weekday

# Pull the station names as a factor level and create a label df for the annotation
wk_ann_levels <- test %>% group_by(Weekday) %>% summarise() %>% pull(Weekday)
wk_ann_text <- tibble(Time = "12:00", mean_av_bikes = 0, 
                   Weekday = factor("Sat", levels = wk_ann_levels))
wk_mean_pred <- base_weekday + 
  geom_label(data = wk_ann_text, label = err_label, size = 3)

wk_mean_pred

ggsave("/plot/predictions/mean_day_pred_houston.png", wk_mean_pred)
######################################################################
# Filter data for the last week available and see how the prediction fares
unseen_all <- read_rds("./saved_data_frames/db_all_data.rds") %>%
  filter(Date >= "2017-11-06" & Date < "2017-11-13") 

clusters <- read_rds("./saved_data_frames/db_clustered_stations.rds")
uns_clust <- unseen_all %>%
  left_join(clusters, by = "Number") %>%
  mutate(cluster = factor(cluster)) %>%
  select(-Name.y, Name = Name.x)

# Group time into 48 factors
unseen_time <- uns_clust %>%
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
    #Name = as.factor(Name),
    season = factor(
      if_else(month(Date) == 11 |month(Date) == 12 | month(Date) == 1, "Winter",
              if_else(month(Date) == 2 |month(Date) == 3 | month(Date) == 4, "Spring",
                      if_else(month(Date) == 5 |month(Date) == 6 |month(Date) == 7, "Summer",
                              "Autumn"))),
      levels = c("Winter", "Spring", "Summer", "Autumn") # complete all factor levels in case there's not enough
    ),
    av_bikes = Bike_stands - Available_stands
  ) %>%
  group_by(
    Number, Name, Address, Bike_stands, Date, season, Weekday, Time
  ) %>%
  summarise(
    av_bikes = round(mean(av_bikes)),
    cluster = first(cluster)
  ) %>%
  mutate( 
    cluster = as.factor(cluster), 
    prev_bike_num = lag(av_bikes), # number of bikes in previous time period
    prev_bike_num = if_else(is.na(prev_bike_num), av_bikes, prev_bike_num)
  ) %>% 
  ungroup()

# DF containing geographical info
geo <- read_csv("./geo_data/db_geo.csv")

# Join the geographical info to the original dataset
uns_rf <- unseen_time %>%
  left_join(geo, by = "Number") %>%
  select(-Name.y, -Address.y, Name = Name.x, Address = Address.x)

rf <- read_rds("./saved_data_frames/rf_model.rds")

# Predict and calculate error
uns_rf$pred <- predict(rf, uns_rf)
uns_err <- rmse(uns_rf$av_bikes, uns_rf$pred)
uns_rf$pred <- round(uns_rf$pred)

# Seems there are negative values in the data, must've been a reading error
uns_rf$av_bikes <- abs(uns_rf$av_bikes)
rmsle(uns_rf$av_bikes, uns_rf$pred)

# Examining Heuston
# uns_rf %>%
#   filter(
#     Number == 93 | Number == 92 | Number == 94,
#     Weekday == "Tue"
#   ) %>%
#   group_by(Address, cluster) %>% summarise() %>%
#   View()
################################ Plot for single week ################################
time_breaks <- uns_rf %>%
  filter(str_detect(Time, "\\d{2}:00")) %>%
  group_by(Time) %>%
  summarise() %>%
  pull(Time)

uns_err_label <- paste("Error margin:", round(uns_err, 2))

# num <- 5 you can set this number if you wish to test the loop for a specific station
# plot means per stations
for(num in 1:102){
  if(num == 20 | num == 50) num = num+1
  
  stat_name <- uns_rf %>% filter(Number == num) %>% group_by(Address) %>% summarise(addr = first(Address)) %>% pull(addr)
  plot_title <- paste("Predictions for unseen data: ", stat_name, " week of 06/11/17", sep ="")
  
  # plot means per stations
  base <- uns_rf %>%
    filter(Number == num) %>%
    # group_by(Weekday, Address, Time) %>%
    # summarise(
    #   mean_av_bikes = mean(av_bikes),
    #   mean_pred = mean(pred),
    #   tot_stands = max(Bike_stands)
    # ) %>%
    ggplot(aes(Time, av_bikes, group = 1)) +
    theme_minimal() +
    geom_line(aes(y = Bike_stands, colour = "Total stands"), linetype = 6) +
    geom_line(aes(colour = "Actual"), size = 1) +
    geom_line(aes(y = pred, group = 1, colour = "Predicted"), size = 1) +
    geom_ribbon(aes(ymin = pred - uns_err, ymax = pred + uns_err), fill = "grey30", alpha = 0.2) +
    theme(axis.text.x = element_text(angle = 90, size = 7)) + 
    scale_x_discrete(
      breaks = time_breaks,
      labels = time_breaks
    ) + 
    ggtitle(plot_title) +
    ylab("Available bikes") +
    scale_colour_manual(
      breaks = c("Predicted", "Actual", "Total"),
      values = c("Predicted"="red", "Actual"="blue", "Total stands"="black")
    ) +
    facet_wrap(~ Weekday) +
    theme(
      legend.box.background = element_rect(),
      legend.position = "bottom"
    )
  base
  
  label_pos <- uns_rf %>% filter(Number == num) %>% pull(Bike_stands)
  label_pos <- label_pos[1]
  
  # Pull the station names as a factor level and create a label df for the annotation
  wk_ann_levels <- uns_rf %>% group_by(Weekday) %>% summarise() %>% pull(Weekday)
  wk_ann_text <- tibble(Time = "12:00", av_bikes = label_pos+3, 
                        Weekday = factor("Sun", levels = wk_ann_levels))
  unseen_pred <- base + 
    geom_label(data = wk_ann_text, label = uns_err_label, size = 3)
  unseen_pred
  
  fname <- paste("./plots/predictions/unseen/predict_", num, ".png", sep = "")
  ggsave(fname, unseen_pred)
}
" NOTE THERE'S AN ERROR WITH STATION 20, SEEMS LIKE THE GEOGRAPHICAL SET DOESN'T HAVE STATION 20 NOR 50"


################################ Hourly bound plot #####################################
#Data frame for hourly bound plot
hr_uns <- uns_rf %>%
  filter(Weekday == "Mon" & Time == "08:00") %>%
  #filter(Number > 20 & Number < 50) %>%
  mutate(
    error = uns_err,
    up_bound = pred + error,
    low_bound = if_else(
      pred - error < 0,
      0,
      pred - error
    ),
    acc_pred = if_else(
      (av_bikes <= up_bound & av_bikes >= low_bound),
      "Sufficient",
      if_else(
        av_bikes > up_bound,
        "Oversupply",
        "Insufficient"
      )
    ),
    st_range = if_else(Number <26, "1-25",
                       if_else(Number <51, "26-50",
                               if_else(Number < 76, "51-75",
                                       "76-102"))),
    st_range = as.factor(st_range)
  )

err_range_label <- paste("Error range: ", round(uns_err*2), " bikes", sep = "")

# Hourly bound plot
hour_bound <- hr_uns %>%
  group_by(Number) %>%
  ggplot(aes(Number, av_bikes)) +
  #geom_errorbar(aes(ymax = Bike_stands, ymin = 0), colour = "grey", linetype = 5, size = .8) +
  geom_errorbar(aes(ymax= up_bound, ymin = low_bound), colour = "black", size = 1) +
  geom_point(aes(shape = factor(acc_pred), fill = acc_pred), size = 3) +
  theme_bw() +
  scale_fill_manual("Starting Inventory", values = c("#D55E00", "#009E73", "#172cc6"))+
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 10),
    legend.text = element_text(size = 8),
    #axis.text.x = element_blank(),
    #axis.ticks.x = element_blank(),
    axis.title = element_text(size = 15),
    plot.title = element_text(size = 20)
  ) +
  xlab("Stations") +
  ylim(0, 43) +
  #xlim(0, 103) +
  ylab("Inventory Bounds (bikes)") +
  scale_shape_manual("Starting Inventory", values = c(25,24,21)) +
  geom_label(x = 50, y = 42, label = err_range_label, size = 5) +
  ggtitle("Prediction bounds for 8:00am, 6/11/17") 
#guides(title = "Starting inventory")

hour_bound
#ggsave("./plots/predictions/8am_bounds.png", hour_bound , width = 30, height = 10, units = "in") 

# Plot the sufficient/insufficient stations on a map to identify locations
library(ggmap)
# Map of dublin, this is the initial plot, we add on to it as we desire
dub_map <- ggmap(
  get_googlemap(
    center = c(-6.270,53.345),
    scale = 2,
    zoom = 13,
    size = c(540,400)
  )
) +
  coord_fixed(ratio = 1.3) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()
  ) 

serv_analysis <- dub_map + geom_point(
  data = hr_uns,
  aes(x = Longitude, y = Latitude,
      shape = factor(acc_pred), 
      fill = acc_pred, 
      size = if_else(
         acc_pred == "Oversupply",
         abs(av_bikes - up_bound),
         if_else(
             acc_pred == "Insufficient",
             abs(av_bikes - low_bound),
             3
           )
        )
    )
  
  ) +
  scale_size_continuous("", guide = "none") +
  scale_fill_manual("Starting Inventory", values = c("#D55E00", "#009E73", "#172cc6")) +
  scale_shape_manual("Starting Inventory", values = c(25,24,21)) +
  theme( legend.position = "bottom")
ggsave("plots/service_analysis.png", serv_analysis)
