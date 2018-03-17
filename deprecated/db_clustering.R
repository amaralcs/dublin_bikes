" ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ DEPRECATED ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"

" 
  Author: Carlos Amaral
  Date: 10/10/17
  Last modified: 14/12/17
  Description:
    This clustering is applied to the number of check in and check outs, with a small 
    number of data points. For a different analysis, check db_clustering_stands
"

library(tidyverse)
library(cluster)
library(fpc)

########################## Charlemont Place investigation #######################
"The first objective is to validate the clustering analysis done for Charlemont station.
  If I can replicate what was done previously I will know how to proceed with the kmeans"

# Read previously processed data
df <- as.tibble(read_rds("./saved_data_frames/db_all_data.rds"))

# Filter charlemont place
df <- df %>% filter(Number == 5)

# Look at the variance of the data, if they seem similar its ok, otherwise have to be scaled
sapply(df[,c("Check_in", "Check_out", "Available_stands")], var)

rge <- sapply(df[,c("Check_in", "Check_out", "Available_stands")], function(x) diff(range(x)))
df_s <- sweep(df[,c("Check_in", "Check_out", "Available_stands")], 2, rge, FUN = "/")
sapply(df_s, var)

scale(df[,c("Check_in", "Check_out", "Available_stands")])

# Determine number of clusters
"Fist step of the algorithm is to plot the weighted sum of squares find an 'elbow' 
  on the plot. The elbow should give the ideal number of clusters"
# First sum of squares
wss <- (nrow(df[,c("Check_in", "Check_out")])-1)*sum(apply(df[,c("Check_in", "Check_out")],2,var))
# Subsequent values can be found using this loop, note '15' is arbitrary 
for (i in 2:15) wss[i] <- sum(kmeans(df[,c("Check_in", "Check_out")], 
                                     centers=i)$withinss)
cl_exam <- plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

ggsave(
  "cluster_analysis.png", 
  cl_exam,
  path = "./plots"
  )

"It seems that 4 is the ideal number of clusters, lets use that"
fit <- kmeans(df[,c("Check_in", "Check_out")], 4)

# Plot the different clusters and see the divide
four_clusters <- plot(df[,c("Check_in", "Check_out")], col =fit$cluster)

ggsave(
  "four_clusters.png", 
  four_clusters,
  path = "./plots"
)

# Centroid Plot against 1st 2 discriminant functions
plotcluster(df[,c("Check_in", "Check_out")], fit$cluster)

# Aggregate data
aggregate(df[,c("Check_in", "Check_out")], by = list(fit$cluster), FUN = mean)

# Create a new data frame with the clustered data
clust_df <- data.frame(df, fit$cluster)
clust_df <-  as.tibble(clust_df) %>%
  rename(Cluster = fit.cluster)

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

ggsave(
  "mean_cl_in.png", 
  clust_in,
  path = "./plots"
)
ggsave(
  "cmean_cl_out.png", 
  clust_out,
  path = "./plots"
)


" From these plots I gather cluster 1: lots of ins and few outs
                            cluster 2: few ins and few outs
                            cluster 3: consistent outs and few ins
                            cluster 4: few ins and lots of outs
"
