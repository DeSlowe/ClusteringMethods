
# tidyverse patchwork plyr plotly GGally dendextend factoextra fmsb formattable

# Loading packages
library(tidyverse)
library(patchwork)
library(dplyr)
library(plyr)
library(plotly)
library(GGally)
library(dendextend)
library(factoextra)
library(fmsb)
library(formattable)




# Setting the working directory
setwd("C:\\Users\\Windows 10\\OneDrive\\Desktop\\LUISS LESSON\\02 script\\data")

# Load data
data = read.csv("mall_customers.csv")

# Check for any missing value
anyNA(data)

# rename columns
new_colnames = c('Id', 'Gender', 'Age', 'Income', 'Score')
names(data) = new_colnames

head(data)
########################################################################################
#### CLUSTERING METHODS   ##############################################################
########################################################################################

# Standardize quantitative variables
data$Age_std = scale(data$Age)[,1]
data$Income_std = scale(data$Income)[,1]
data$Score_std = scale(data$Score)[,1]

# Check if means are equal to 0
summarise(data
  , avg_Age_std = mean(Age_std)
  , avg_Income_std = mean(Income_std)
  , avg_Score_std = mean(Score_std)
)


# Check if standar deviations are equal to 1
summarise(data
          , sd_Age_std = sd(Age_std)
          , sd_Income_std = sd(Income_std)
          , sd_Score_std = sd(Score_std)
)

# Calculate distances matrix
dist = data %>% 
  select(Income_std, Score_std) %>% 
  dist(method = 'euclidean')

?dist

# Alternative 2
dist = dist(data[, c("Income_std", "Score_std")], method = 'euclidean')


#######################################
# Calculate Dendrograms ###############
#######################################

# Hierarchical clustering
par(mfrow=c(2,2))

# Single Linkage
hc1 = hclust(dist, method = 'single') %>% 
  as.dendrogram() %>% 
  set('labels', '') # Avoid to plot labels
plot(hc1, main = "Hierarchical Clustering: Single Linkage")


# Complete Linkage
hc2 = hclust(dist, method = 'complete') %>% 
  as.dendrogram() %>% 
  set('labels', '') # Avoid to plot labels
plot(hc2, main = "Hierarchical Clustering: Complete Linkage")

# Average Linkage
hc3 = hclust(dist, method = 'average') %>% 
  as.dendrogram() %>% 
  set('labels', '') # Avoid to plot labels 
plot(hc3, main = "Hierarchical Clustering: Average Linkage")

# Ward's Method
hc4 = hclust(dist, method = 'ward.D') %>% 
  as.dendrogram() %>% 
  set('labels', '') # Avoid to plot labels 
plot(hc4, main = "Hierarchical Clustering: Ward's Method")


############################################################################
# Choose the number of cluster based on 2 different metrics:
# (https://www.datanovia.com/en/lessons/determining-the-optimal-number-of-clusters-3-must-know-methods/)

# 1. Elbow Method: Minimizing within cluster sums of squares
# One should choose a number of clusters so that adding another cluster 
# doesn't improve much better the total WSS
data %>% 
  select(Income_std, Score_std) %>%
  fviz_nbclust(FUN = hcut, method = 'wss')


# 2. Average Silhouette Method: measuring the quality of clustering
# It determines how well each object lies within its cluster. 
# A high average silhouette width indicates a good clustering
data %>% 
  select(Income_std, Score_std) %>% 
  scale() %>% 
  fviz_nbclust(FUN = hcut, method = 'silhouette')

data %>% 
  select(Income_std, Score_std) %>% 
  scale()
?fviz_nbclust


# Hierarchical clustering
par(mfrow=c(1,1))

# Single Linkage
hc1 = hclust(dist, method = 'single') %>% 
  as.dendrogram() %>% 
  set('labels', '') %>% # Avoid to plot labels
  set('branches_k_color', k = 5)
plot(hc1, main = "Hierarchical Clustering: Single Linkage")

# Complete Linkage
hc2 = hclust(dist, method = 'complete') %>% 
  as.dendrogram() %>% 
  set('labels', '') %>% # Avoid to plot labels
  set('branches_k_color', k = 5)
plot(hc2, main = "Hierarchical Clustering: Complete Linkage")
abline(h = 2.75, col = 'red')

# Average Linkage
hc3 = hclust(dist, method = 'average') %>% 
  as.dendrogram() %>% 
  set('labels', '') %>% # Avoid to plot labels 
  set('branches_k_color', k = 5)
plot(hc3, main = "Hierarchical Clustering: Average Linkage")
abline(h = 1.7, col = 'red')

# Ward's Method
hc4 = hclust(dist, method = 'ward.D') %>% 
  as.dendrogram() %>% 
  set('labels', '') %>% # Avoid to plot labels 
  set('branches_k_color', k = 5)
plot(hc4, main = "Hierarchical Clustering: Ward's Method")
abline(h = 25, col = 'red')


# How to find the best Linkage Method to Use (https://www.statology.org/hierarchical-clustering-in-r/)

#define linkage methods
library(factoextra)
library(cluster)
m = c( "single", "complete", "average", "ward")
names(m) = c("single", "complete", "average", "ward")


# function to compute agglomerative coefficient: 
# which is metric that measures the strength of the clusters. 
# The closer this value is to 1, the stronger the clusters. 
ac = function(x) {
  agnes(data[, c("Income_std", "Score_std")], method = x)$ac
}

#calculate agglomerative coefficient for each clustering linkage method
sapply(m, ac)






# Plot Clusters by Income and Score

# Cluster: Single linkage
clust1 = cutree(hc1, k = 5) %>% 
  data.frame() %>% 
  cbind(data) %>% 
  ggplot() + 
  geom_point(aes(x = Income,
                 y = Score,
                 color = as.factor(.)),
             show.legend = F) +
  labs(title = "Single Linkage")

# Cluster: Complete Linkage
clust2 = cutree(hc2, k = 5) %>% 
  data.frame() %>% 
  cbind(data) %>% 
  ggplot() + 
  geom_point(aes(x = Income,
                 y = Score,
                 color = as.factor(.)),
             show.legend = F) +
  labs(title = "Complete Linkage")

# CLuster: Average Linkage
clust3 = cutree(hc3, k = 5) %>% 
  data.frame() %>% 
  cbind(data) %>% 
  ggplot() + 
  geom_point(aes(x = Income,
                 y = Score,
                 color = as.factor(.)),
             show.legend = F) +
  labs(title = "Average Linkage")

# Cluster: Ward's Method
clust4 = cutree(hc4, k = 5) %>% 
  data.frame() %>% 
  cbind(data) %>% 
  ggplot() + 
  geom_point(aes(x = Income,
                 y = Score,
                 color = as.factor(.)),
             show.legend = F) +
  labs(title = "Ward's Method")

# Multiple plot
clust1 + clust2 + clust3 + clust4







# Merging clusters numbers to the starting dataset
new_data = data %>% 
  cbind(cl5_single = cutree(hc1, k = 5)
        , cl5_complete = cutree(hc2, k = 5)
        , cl5_average = cutree(hc3, k = 5)
        , cl5_ward = cutree(hc4, k = 5)
  )




# Describing each cluster

# Choose the cluster scenario to analyze
clst = "cl5_complete"


# Calculate aggregate statistics by clusters
table_descr = new_data %>%
  mutate(Cluster = new_data[, c(clst)]) %>% 
  group_by(Cluster) %>% 
  summarise(n =n(),
            n_Pct = round(n() / nrow(new_data) * 100, 2),
            Avg_Age =round(mean(Age)),
            Avg_Income = round(mean(Income)),
            Avg_Score = round(mean(Score)),
            n_male = sum(Gender=='Male'),
            n_female = sum(Gender=='Female')) %>% 
  mutate(Male_Pct = round(n_male/(n_male + n_female)*100, 1), 
         Female_Pct = round(n_female/(n_male + n_female)*100, 1)) %>% 
  select(Cluster, n, n_Pct, Avg_Age, Avg_Income, Avg_Score, Male_Pct, Female_Pct)


# Export in excel
library(writexl)
write_xlsx(table_descr, 'table_descr.xlsx', col_names=TRUE)


# Calculate penetration indexes

# Calculate statistics for whole sample
table_descr_tot = new_data %>%
  summarise(
    n_Pct = round(n() / nrow(new_data) * 100, 2),
    Avg_Age =round(mean(Age)),
    Avg_Income = round(mean(Income)),
    Avg_Score = round(mean(Score)),
    n_male = sum(Gender=='Male'),
    n_female = sum(Gender=='Female')) %>% 
  mutate(n=n(), Male_Pct = round(n_male/(n_male + n_female)*100, 1), 
         Female_Pct = round(n_female/(n_male + n_female)*100, 1)) %>% 
  select(n, n_Pct, Avg_Age, Avg_Income, Avg_Score, Male_Pct, Female_Pct) 

# Export in excel
write_xlsx(table_descr_tot, 'table_descr_tot.xlsx', col_names=TRUE)


table_descr_idx = table_descr %>% 
  full_join (table_descr_tot, by = character ()) %>%
  mutate(n_idx = round(n.x / (n.y / nrow(table_descr)) * 100, 1)
         , Age_idx = round(Avg_Age.x / Avg_Age.y * 100, 1)
         , Income_idx = round(Avg_Income.x / Avg_Income.y * 100, 1)
         , Score_idx = round(Avg_Score.x / Avg_Score.y * 100, 1)
         , Male_idx = round(Male_Pct.x / Male_Pct.y * 100, 1)
         , Female_idx = round(Female_Pct.x / Female_Pct.y * 100, 1)
  ) %>%
  select(Cluster, n_idx, Age_idx, Income_idx, Score_idx, Male_idx, Female_idx)

# Export in excel
write_xlsx(table_descr_idx, 'table_descr_idx.xlsx', col_names=TRUE)

# Showing the results with formatted table
table_descr_idx%>%
  formattable(list(area(T, 2:7) ~ color_tile("white", "red")), align = 'c')




# Setting up 3-D Graph
clust3D = plot_ly(new_data, 
                  x = ~new_data$Age,
                  y = ~new_data$Income,
                  z = ~new_data$Score,
                  color = ~new_data[, c(clst)]) %>% 
  add_markers() %>% 
  layout(scene = list(xaxis = list(title = 'Age'),
                      yaxis = list(title = 'Income'),
                      zaxis = list(title = 'Spending Score')))

# 3-D plot
clust3D


