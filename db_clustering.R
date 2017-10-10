library(tidyverse)
library(cluster)
library(fpc)
########################## Charlemont Place investigation #######################
"The first objective is to validate the clustering analysis done for Charlemont station.
  If I can replicate what was done previously I will know how to proceed with the kmeans"

# Change working directory - change appropriately to where rds files are
setwd("C:/Users/Carlos/Documents/Dublin Bikes Project/dublin_bikes/saved_data_frames")

# Read previously processed data
df <- as.tibble(read_rds("db_all_data.rds"))

# Filter charlemont place
df <- df %>% filter(Number == 5)

# Look at the variance of the data, if they seem similar its ok, otherwise have to be scaled
sapply(df[,c("Check_in", "Check_out")], var)

# Determine number of clusters
"Fist step of the algorithm is to plot the weighted sum of squares find an 'elbow' 
  on the plot. The elbow should give the ideal number of clusters"
# First sum of squares
wss <- (nrow(kdf)-1)*sum(apply(df[,c("Check_in", "Check_out")],2,var))
# Subsequent values can be found using this loop, note '15' is arbitrary 
for (i in 2:15) wss[i] <- sum(kmeans(df[,c("Check_in", "Check_out")], 
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

"It seems that 4 is the ideal number of clusters, lets use that"
fit <- kmeans(df[,c("Check_in", "Check_out")], 4)

# Plot the different clusters and see the divide
plot(df[,c("Check_in", "Check_out")], col =fit$cluster)

# Centroid Plot against 1st 2 discriminant functions
plotcluster(df[,c("Check_in", "Check_out")], fit$cluster)

# Aggregate data
aggregate(df[,c("Check_in", "Check_out")], by = list(fit$cluster), FUN = mean)

# Create a new data frame with the clustered data
clust_df <- data.frame(df, fit$cluster)
clust_df <-  as.tibble(clust_df) %>%
  rename(Cluster = fit.cluster)

# plot cluster means for check in over time
clust_df %>%
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

# plot cluster means over time
clust_df %>%
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

" From these plots I gather cluster 1: lots of ins and few outs
                            cluster 2: few ins and few outs
                            cluster 3: consistent outs and few ins
                            cluster 4: few ins and lots of outs
"
