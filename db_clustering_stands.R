library(tidyverse)
library(cluster)
library(fpc)
########################## Charlemont Place investigation #######################
"The first objective is to validate the clustering analysis done for Charlemont station.
  If I can replicate what was done previously I will know how to proceed with the kmeans"

# Change working directory - change appropriately to where rds files are
setwd("./saved_data_frames")

# Read previously processed data
df <- as.tibble(read_rds("db_all_data.rds"))

df %>%
  filter(Number == 5) %>%
  filter(Date == "2017-01-16") %>% View()

# Data preparation for charlemont
ch_df <- df %>% 
  filter(Number == 5) %>%
  mutate(
    day = ifelse(Weekday == "Mon", 1,
            ifelse(Weekday == "Tue", 2,
              ifelse(Weekday == "Wed", 3,
                ifelse(Weekday == "Thu", 4,
                  ifelse(Weekday == "Fri", 5,
                    ifelse(Weekday == "Sat", 6,
                      ifelse(Weekday == "Sun", 7, -1)))))))
  ) %>%
  select(Number, day, Date,  Time, Available_stands) %>%
  group_by(Number, Date) %>%
  spread(key = Time, value = Available_stands) %>%
  ungroup() %>%
  select(-Number, -Date)

# Look at the variance of the data, if they seem similar its ok, otherwise have to be scaled
sapply(ch_df, var, na.rm = TRUE)
"Looks like theres a lot of variance between the values, but I'll proceed with this anyway"

# Create a datafram without null values for the analysis
ch_df_complete <- ch_df[complete.cases(ch_df),]
"I noticed there are very few complete cases for charlemont, I will try to find a better station"

# Data preparation for all stations
kdf <- df %>% 
  mutate(
    day = ifelse(Weekday == "Mon", 1,
           ifelse(Weekday == "Tue", 2,
            ifelse(Weekday == "Wed", 3,
             ifelse(Weekday == "Thu", 4,
              ifelse(Weekday == "Fri", 5,
               ifelse(Weekday == "Sat", 6,
                ifelse(Weekday == "Sun", 7, -1)))))))
  ) %>%
  select(Number, day, Date,  Time, Available_stands) %>%
  group_by(Number, Date) %>%
  spread(key = Time, value = Available_stands) %>%
  ungroup()

# Filter only complete cases
full_kdf <- kdf[complete.cases(kdf),]

full_kdf %>%
  group_by(Number) %>%
  summarise(count = n()) %>% View()
"Seems like station 93 and 94 have the most complete dataset, 
  however it is still very little data, I will continue with the all stations"
full_kdf <- full_kdf %>% 
  select(-Number, -Date)

sapply(full_kdf, var)
"Variance looks better now"

# Scaling each measure by dividing it by the range of values
#rge <- sapply(df_s[,c("Check_in", "Check_out", "Available_stands")], function(x) diff(range(x)))
#df_s$Check_in <- df_s$Check_in / rge[1]
#df_s$Check_out <- df_s$Check_out / rge[2]
#df_s$Available_stands <- df_s$Available_stands / rge[3]
#sapply(df_s[,c("Check_in", "Check_out", "Available_stands")], var)

# Determine number of clusters
"Fist step of the algorithm is to plot the weighted sum of squares find an 'elbow' 
  on the plot. The elbow should give the ideal number of clusters"
# First sum of squares
n <- nrow(full_kdf)
wss <- rep(0,15)
wss[1] <- (n-1) * sum(sapply(full_kdf,var))
# Subsequent values can be found using this loop, note '15' is arbitrary 
for (i in 2:15)  wss[i] <- sum(kmeans(full_kdf, 
                                     centers=i)$withinss)

cl_exam <- plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

#ggsave(
#  "cluster_analysis.png", 
#  cl_exam,
#  path = "C:/Users/Carlos/Documents/Dublin Bikes Project/dublin_bikes/plots"
 # )

#########################

"It seems that 5 is the ideal number of clusters, lets use that"
fit_4 <- kmeans(full_kdf, 4)
fit_5 <- kmeans(full_kdf, 5)


" continue here"
# Plot the different clusters and see the divide
four_clusters <- plot(full_kdf, col = fit_4$cluster)
five_clusters <- plot(full_kdf, col = fit_5$cluster)
four_clusters

#ggsave(
#  "four_clusters.png", 
#  four_clusters,
#  path = "C:/Users/Carlos/Documents/Dublin Bikes Project/dublin_bikes/plots"
#)

# Centroid Plot against 1st 2 discriminant functions
plotcluster(df_s[,c("Check_in", "Check_out", "Available_stands")], fit$cluster)

# Aggregate data
aggregate(df_s[,c("Check_in", "Check_out", "Available_stands")], by = list(fit$cluster), FUN = mean)

# Create a new data frame with the clustered data
clust_df <- data.frame(df_s, fit$cluster)
clust_df <-  as.tibble(clust_df) %>%
  rename(Cluster = fit.cluster)

clust_df$Check_in <- clust_df$Check_in * rge[1]
clust_df$Check_out <- clust_df$Check_out * rge[2]
clust_df$Available_stands <- clust_df$Available_stands * rge[3]

# plot cluster means for check in over time
clust_in <- clust_df %>%
  mutate(
    Cluster = as.factor(Cluster)
  ) %>%
  group_by(Number, Time, Cluster) %>%
  mutate(
    cluster_mean_in = mean(Check_in),
    cluster_mean_out = mean(Check_out)
  ) %>% 
  ggplot(aes(Time, cluster_mean_in, colour = Cluster, group = Cluster )) +
  geom_line(size = 1) +
  scale_x_discrete() +
  ylab("Mean centroid value") +
  xlab("Time") +
  ggtitle("Overall check in comparison") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
clust_in


# plot cluster means over time
clust_out <- clust_df %>%
  mutate(
    Cluster = as.factor(Cluster)
  ) %>%
  group_by(Number, Time, Cluster) %>%
  mutate(
    cluster_mean_in = mean(Check_in),
    cluster_mean_out = mean(Check_out)
  ) %>% 
  ggplot(aes(Time, cluster_mean_out, colour = Cluster, group = Cluster )) +
  geom_line(size = 1) +
  scale_x_discrete() +
  ylab("Mean centroid value") +
  xlab("Time") +
  ggtitle("Overall check out comparison") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
clust_out

# plot cluster means over time
clust_stands <- clust_df %>%
  mutate(
    Cluster = as.factor(Cluster)
  ) %>%
  group_by(Number, Time, Cluster) %>%
  mutate(
    cluster_mean_stand = mean(Available_stands)
  ) %>% 
  ggplot(aes(Time, cluster_mean_stand, colour = Cluster, group = Cluster )) +
  geom_line(size = 1) +
  scale_x_discrete() +
  ylab("Mean centroid value") +
  xlab("Time") +
  ggtitle("Overall available stands comparison") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
clust_stands



#ggsave(
#  "mean_cl_in.png", 
#  clust_in,
#  path = "C:/Users/Carlos/Documents/Dublin Bikes Project/dublin_bikes/plots"
#)
#ggsave(
#  "cmean_cl_out.png", 
#  clust_out,
#  path = "C:/Users/Carlos/Documents/Dublin Bikes Project/dublin_bikes/plots"
#)


" From these plots I gather cluster 1: lots of ins and few outs
                            cluster 2: few ins and few outs
                            cluster 3: consistent outs and few ins
                            cluster 4: few ins and lots of outs
"
