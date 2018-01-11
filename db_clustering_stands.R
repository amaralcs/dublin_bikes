" 
  Author: Carlos Amaral
Date: 
Last Modified: 09/01/18
Description: 
This uses k-means clustering to cluster the data based on the number of available stands
at each station for each period of time. 
The tricky part is to get the dataset into the desired format. In particular, the name of
the columns at the desired format become the times of the day (i.e. 00:00:00, 00:10:00, etc..)
"

library(tidyverse)
library(stringr)
########################## Data re-shaping #############################
# Read previously processed data
df <- as.tibble(read_rds("./saved_data_frames/db_all_data.rds"))

# Filter unwanted data
df <- df %>%
  filter(Date >= "2016-10-14" & Date <= "2017-10-14")

prep_df <- df %>% 
  mutate(
    Time = str_replace_all(Time, ":", "_")
  ) %>%
  select(Number, Date,  Time, Available_stands) %>%
  group_by(Number, Date) %>%
  spread(key = Time, value = Available_stands) %>%
  ungroup() %>%
  select(-Number, -Date)
########################## Data sanitisation ###############################
"
Due to the way the data is collected, there are periods of 10 minutes for which there is no entry
To correct this, I implement the following code, which replaced NA entried by the previoues read
or the following read in case the missing value is in the first row
"

fix_na <- function(column){
  # Iterate through rows
  for(i in 1:length(column)){
    # Apply different result if it is first row
    if(i == 1 & is.na(column[i])){
      # If first row is NA, we need to check the next non NA entry and take that
      j = i+1
      while( is.na(column[j]) ) j=j+1
      column[i] <- column[j]
      }
    else{
      # Else replace the NA by the previous value
      ifelse(is.na(column[i]), column[i] <- column[i-1], column[i] <- column[i])
    }
    
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
########################## k-means plot for prep df ################
# k-means fit the data
n <- nrow(prep_df)
wss <- rep(0,10)
wss[1] <- (n-1) * sum(sapply(prep_df,var))

# Subsequent values can be found using this loop, note '15' is arbitrary 
for (i in 2:10)  wss[i] <- sum(
  kmeans(prep_df, centers=i)$withinss
)

png(filename = "./plots/clustering/wss.png")
# Plot wss for each number of clusters and use elbow method
cl_exam <- plot(1:10, wss, type="b", xlab="Number of Clusters",
                ylab="Within groups sum of squares")
dev.off()

# 4-means clustering
kfit <- kmeans(prep_df, centers = 4)

# Specific colours for the clusters
cluster_colours <- as.character(c("#000099", "#cc99ff", "#00cc00", "#cc3300"))

# Change format of data for plotting
k_centers <- as.tibble(kfit$centers) %>%
  mutate(cluster = as.factor(row_number())) %>%
  gather(key = time, value = available_stands, -cluster) %>%
  mutate( 
    time = str_replace_all(time, "_", ":"),
    time = factor(time, levels = time_lvls)
  )

## Plot the data
all_centroid_plot <- k_centers %>% 
  ggplot(aes(x = time, y = available_stands, colour = cluster, group = cluster)) +
  geom_line() +
  scale_x_discrete(
    breaks = time_breaks,
    labels = break_labels
  ) + 
  xlab("Hour of day") +
  ylab("Available stands") +
  ggtitle("Clustering for all stations")+ 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_colour_manual(
    breaks = k_centers$cluster,
    values = cluster_colours
  )

all_centroid_plot
ggsave("plots/clustering/Clustering for all stations.png", all_centroid_plot)

########################## Comparison between Weekday / Weekend ####################
"######## Filter out Weekends ##############"
week_df <- df %>% 
  filter(
    Weekday %in% c("Mon", "Tue", "Wed", "Thur")
  ) %>%
  mutate(
    Time = str_replace_all(Time, ":", "_")
  ) %>%
  select(Number, Date,  Time, Available_stands) %>%
  group_by(Number, Date) %>%
  spread(key = Time, value = Available_stands) %>%
  ungroup() %>%
  select(-Number, -Date)

week_df <- as.tibble(lapply(week_df, fix_na))

# k-means fit the data
n <- nrow(week_df)
wss <- rep(0,15)
wss[1] <- (n-1) * sum(sapply(week_df,var))

# Subsequent values can be found using this loop, note '15' is arbitrary 
for (i in 2:15)  wss[i] <- sum(
  kmeans(week_df, centers=i)$withinss
)

# Plot wss for each number of clusters and use elbow method
plot(1:15, wss, type="b", xlab="Number of Clusters",
                ylab="Within groups sum of squares")

# 4-means clustering
kfit <- kmeans(week_df, centers = 4)

# Change format of data for plotting
week_centers <- as.tibble(kfit$centers) %>%
  mutate(cluster = as.factor(row_number())) %>%
  gather(key = time, value = available_stands, -cluster) %>%
  mutate( 
    time = str_replace_all(time, "_", ":"),
    time = factor(time, levels = time_lvls)
  )

## Plot the data
week_centroid_plot <- week_centers %>% 
  ggplot(aes(x = time, y = available_stands, colour = cluster, group = cluster)) +
  geom_line() +
  scale_x_discrete(
    breaks = time_breaks,
    labels = break_labels
  ) + 
  xlab("Hour of day") +
  ylab("Available stands") +
  ggtitle("Weekday clusters (Mon - Thu)")+ 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
week_centroid_plot
ggsave("plots/clustering/weekday clustering.png", week_centroid_plot)  

"########### Filter out Weekdays ##########"
end_df <- df %>% 
  filter(
    Weekday %in% c("Fri", "Sat", "Sun")
  ) %>%
  mutate(
    Time = str_replace_all(Time, ":", "_")
  ) %>%
  select(Number, Date,  Time, Available_stands) %>%
  group_by(Number, Date) %>%
  spread(key = Time, value = Available_stands) %>%
  ungroup() %>%
  select(-Number, -Date)

end_df <- as.tibble(lapply(end_df, fix_na))

# k-means fit the data
n <- nrow(end_df)
wss <- rep(0,15)
wss[1] <- (n-1) * sum(sapply(end_df,var))

# Subsequent values can be found using this loop, note '15' is arbitrary 
for (i in 2:15)  wss[i] <- sum(
  kmeans(end_df, centers=i)$withinss
)

# Plot wss for each number of clusters and use elbow method
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

# 4-means clustering
kfit <- kmeans(end_df, centers = 4)

# Change format of data for plotting
end_centers <- as.tibble(kfit$centers) %>%
  mutate(cluster = as.factor(row_number())) %>%
  gather(key = time, value = available_stands, -cluster) %>%
  mutate( 
    time = str_replace_all(time, "_", ":"),
    time = factor(time, levels = time_lvls)
  )

## Plot the data
end_centroid_plot <- end_centers %>% 
  ggplot(aes(x = time, y = available_stands, colour = cluster, group = cluster)) +
  geom_line() +
  scale_x_discrete(
    breaks = time_breaks,
    labels = break_labels
  ) + 
  xlab("Hour of day") +
  ylab("Available stands") +
  ggtitle("Weekends clusters (Fri - Sun)")+ 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
end_centroid_plot
ggsave("plots/clustering/weekend clustering.png", end_centroid_plot)  
########################## Clustering for bottom 10 stations ########################
"
 An idea I had was to compare the clustering method between the top 10 and bottom 10
 stations to see if there is a big discrepancy between them
"
"######## Filter top stations ##############"
# Create a list with the top stations
top_stations <- df %>%
  mutate(activity = Check_in + Check_out) %>%
  group_by(Number) %>%
  summarise(
    activity = sum(activity)
  ) %>%
  top_n(10, activity) %>%
  arrange(desc(activity))
top_stations <- top_stations$Number

top_df <- df %>% 
  filter(
    Number %in% top_stations
  ) %>%
  mutate(
    Time = str_replace_all(Time, ":", "_")
  ) %>%
  select(Number, Date,  Time, Available_stands) %>%
  group_by(Number, Date) %>%
  spread(key = Time, value = Available_stands) %>%
  ungroup() %>%
  select(-Number, -Date)

top_df <- as.tibble(lapply(top_df, fix_na))

# k-means fit the data
n <- nrow(top_df)
wss <- rep(0,10)
wss[1] <- (n-1) * sum(sapply(top_df,var))

# Subsequent values can be found using this loop, note '15' is arbitrary 
for (i in 2:10)  wss[i] <- sum(
  kmeans(top_df, centers=i)$withinss
)

# Plot wss for each number of clusters and use elbow method
plot(1:10, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

# 4-means clustering
kfit <- kmeans(top_df, centers = 4)

# Change format of data for plotting
top_centers <- as.tibble(kfit$centers) %>%
  mutate(cluster = as.factor(row_number())) %>%
  gather(key = time, value = available_stands, -cluster) %>%
  mutate( 
    time = str_replace_all(time, "_", ":"),
    time = factor(time, levels = time_lvls)
  )

## Plot the data
top_centroid_plot <- top_centers %>% 
  ggplot(aes(x = time, y = available_stands, colour = cluster, group = cluster)) +
  geom_line() +
  scale_x_discrete(
    breaks = time_breaks,
    labels = break_labels
  ) + 
  xlab("Hour of day") +
  ylab("Available stands") +
  ggtitle("Clustering of top 10 stations")+ 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
top_centroid_plot
ggsave("plots/clustering/Top 10 clustering.png", top_centroid_plot)  

# Create a list with the bottom stations
bottom_stations <- df %>%
  mutate(activity = Check_in + Check_out) %>%
  group_by(Number) %>%
  summarise(
    activity = sum(activity)
  ) %>%
  top_n(-10, activity) %>%
  arrange(desc(activity))
bottom_stations <- bottom_stations$Number

bottom_df <- df %>% 
  filter(
    Number %in% bottom_stations
  ) %>%
  mutate(
    Time = str_replace_all(Time, ":", "_")
  ) %>%
  select(Number, Date,  Time, Available_stands) %>%
  group_by(Number, Date) %>%
  spread(key = Time, value = Available_stands) %>%
  ungroup() %>%
  select(-Number, -Date)

bottom_df <- as.tibble(lapply(bottom_df, fix_na))

# k-means fit the data
n <- nrow(bottom_df)
wss <- rep(0,15)
wss[1] <- (n-1) * sum(sapply(bottom_df,var))

# Subsequent values can be found using this loop, note '15' is arbitrary 
for (i in 2:15)  wss[i] <- sum(
  kmeans(bottom_df, centers=i)$withinss
)

# Plot wss for each number of clusters and use elbow method
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

# 4-means clustering
kfit <- kmeans(bottom_df, centers = 4)

# Change format of data for plotting
bottom_centers <- as.tibble(kfit$centers) %>%
  mutate(cluster = as.factor(row_number())) %>%
  gather(key = time, value = available_stands, -cluster) %>%
  mutate( 
    time = str_replace_all(time, "_", ":"),
    time = factor(time, levels = time_lvls)
  )

## Plot the data
bottom_centroid_plot <- bottom_centers %>% 
  ggplot(aes(x = time, y = available_stands, colour = cluster, group = cluster)) +
  geom_line() +
  scale_x_discrete(
    breaks = time_breaks,
    labels = break_labels
  ) + 
  xlab("Hour of day") +
  ylab("Available stands") +
  ggtitle("Clustering of bottom 10 stations")+ 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
bottom_centroid_plot
ggsave("plots/clustering/Bottom 10 clustering.png", bottom_centroid_plot)  


########################## Aggregate Clustering results with data #################
" Add clustering results back to the original data frame "
# Create df with the centroid
centers <- as.tibble(kfit$centers) %>%
  mutate(cluster = paste("c", row_number())) %>%
  gather((`0_00_00`:`9_50_00`), key = "time", value = "available_stands") %>%
  group_by(cluster) %>%
  summarise(
    sum_of_stands = sum(available_stands)
  ) %>% 
  ungroup()

# Compare each station with each centroid
cluster_df <- df %>%  
  mutate(
    t_hour = as.numeric(str_extract(Time, "^\\d{1,2}")),
    t_min = as.numeric(str_extract( str_extract(Time, ":\\d+:"), "\\d+") )
  ) %>%
  group_by(Number, Name, Time, t_hour, t_min) %>%
  summarise(stands = mean(Available_stands)) %>%
  arrange(t_hour, t_min) %>%
  ungroup() %>%
  select(-t_hour, -t_min) %>%
  arrange(Number) %>%
  mutate(
    cluster = "none"
  )

# Loop through each station and find the cluster which is closer to it
for(i in 1:102){
  # Get the sum of stands for the stands
  st <- as.numeric(cluster_df %>%
    filter(Number == i) %>%
    summarise( 
      sum_of_stands = sum(stands)
    ))

  # Subtract it from centroid sum of stands and find minimal one
  min_diff <- centers %>%
    mutate(
      station_sum = st,
      diff = abs(sum_of_stands - station_sum)
    ) %>%
    arrange(diff) %>%
    filter(row_number() == 1)
  
  # add result back to the df
  cluster_df <- cluster_df %>%
    mutate(
      cluster = if_else(
        Number == i,
        min_diff$cluster,
        cluster
      )
    )
}

# Of stations in each cluster
cluster_df %>%
  group_by(cluster) %>%
  summarise(
    count = n()/144 # Each station had 144 observations (24 hours x 6 ten mins groups)
  )

c1 <- cluster_df %>%
  filter( cluster == "c 1") %>%
  group_by(Number, Name) %>%
  summarise() %>% ungroup()
write_csv(c1, "./plots/clustering/cluster1.csv")

c2 <- cluster_df %>%
  filter( cluster == "c 2") %>%
  group_by(Number, Name) %>%
  summarise() %>% ungroup()
write_csv(c2, "./plots/clustering/cluster2.csv")

c3 <- cluster_df %>%
  filter( cluster == "c 3") %>%
  group_by(Number, Name) %>%
  summarise() %>% ungroup()
write_csv(c3, "./plots/clustering/cluster3.csv")

c4 <- cluster_df %>%
  filter( cluster == "c 4") %>%
  group_by(Number, Name) %>%
  summarise() %>% ungroup()
write_csv(c4, "./plots/clustering/cluster4.csv")

############################### Plot the clusters on a map ################################
library(ggmap)
geo <- read_csv("./geo_data/db_geo.csv")

# Drop unnecessary columns for geo
geo <- geo %>%
  select(Number, Latitude, Longitude)

# Do the join
plot_df <- cluster_df %>%
  left_join(geo, by = "Number") 
plot_df <- plot_df %>%
  group_by(Number, Name, Latitude, Longitude, cluster) %>%
  summarise() %>% ungroup()

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

cluster_plot <- dub_map + 
  geom_point(
    data = plot_df,
    aes(
      x = Longitude,
      y = Latitude,
      colour = cluster
    ),
    size = 3
  ) +
  scale_colour_manual(
    breaks = plot_df$cluster,
    values = cluster_colours
  )
cluster_plot
ggsave("plots/clustering/map_clusters.png", cluster_plot)
