library(cluster)  # For k-medoids
library(factoextra)  # For visualization
library(dbscan)  # For dbscan and optics
library(dplyr)
library(tidyverse)
library(ggplot2)

##Data preprocessing
data <- read.csv("C:/Users/saivi/OneDrive/Documents/DSA 5103 INTELLIGENCE SYSTES AND DATA ANALYTICS ASSIGNMENT/automobile/imports-85.data", header = FALSE,
                 col.names = c("symboling","normalized-losses","make","fuel-type","aspiration","num-of-doors","body-style","drive-wheels","engine-location","wheel-base","length","width","height",
                               "curb-weight","engine-type","num-of-cylinders","engine-size","fuel-system","bore","stroke","compression-ratio","horsepower","peak-rpm","city-mpg","highway-mpg","price"))
View(data)
sum(is.na(data))
colSums(is.na(data))
num_question_marks <- sum(data == "?", na.rm = TRUE)
cat("Total '?' in the dataset:", num_question_marks, "\n")
question_marks_per_column <- colSums(data == "?", na.rm = TRUE)
print(question_marks_per_column)

data[data == "?"] <- NA
sum(is.na(data))

data <- data %>% select(-normalized.losses)
data <- data[!duplicated(data), ]
cat("Number of rows after removing duplicates:", nrow(data), "\n")

for (col in names(data)) {
  if (is.factor(data[[col]]) || is.character(data[[col]])) {
    data[[col]] <- as.numeric(as.factor(data[[col]]))
  }
}
colSums(is.na(data))

mode_impute <- function(x) {
  mode <- as.numeric(names(sort(table(x), decreasing = TRUE)[1]))
  x[is.na(x)] <- mode
  return(x)
}
data$num.of.doors <- mode_impute(data$num.of.doors)
data$bore[is.na(data$bore)] <- median(data$bore, na.rm = TRUE)
data$stroke[is.na(data$stroke)] <- median(data$stroke, na.rm = TRUE)
data$horsepower[is.na(data$horsepower)] <- median(data$horsepower, na.rm = TRUE)
data$peak.rpm[is.na(data$peak.rpm)] <- median(data$peak.rpm, na.rm = TRUE)
data$price[is.na(data$price)] <- median(data$price, na.rm = TRUE)

data <- as_tibble(data)

summary(data)
str(data)

# 1. Scatterplot
ggplot(data, aes(x = engine.size, y = length)) + 
  geom_point() + 
  theme_minimal() +
  labs(
    title = "Relationship Between Engine Size and Length",
    x = "Engine Size",
    y = "Length"
  )
ggplot(data, aes(x = engine.size, y = width)) + 
  geom_point() + 
  theme_minimal() +
  labs(
    title = "Relationship Between Engine Size and Width",
    x = "Engine Size",
    y = "Width"
  )
ggplot(data, aes(x = engine.size, y = height)) + 
  geom_point() + 
  theme_minimal() +
  labs(
    title = "Relationship Between Engine Size and Height",
    x = "Engine Size",
    y = "Height"
  )

# 2. Boxplot
ggplot(data, aes(x = body.style, y = engine.size)) +
  geom_boxplot() + 
  theme_minimal() +
  labs(
    title = "Engine Size Across Different Body Styles",
    x = "Body Style",
    y = "Engine Size"
  )
ggplot(data, aes(x = body.style, y = compression.ratio)) +
  geom_boxplot() + 
  theme_minimal() +
  labs(
    title = "Compression Ratio Across Different Body Styles",
    x = "Body Style",
    y = "Compression Ratio"
  )

# 3. Correlation heatmap
library(corrplot)
numeric_data <- data[sapply(data, is.numeric)]
corrplot(cor(numeric_data), method = "color", 
         title = "Correlation Heatmap of Numeric Features",
         tl.col = "black", tl.srt = 45)

##K-MEANS Clustering
fviz_nbclust(numeric_data, kmeans, method = "wss") + 
  ggtitle("Elbow Method for k-means")
set.seed(123)
kmeans_result <- kmeans(numeric_data, centers = 2)
fviz_cluster(kmeans_result, data = numeric_data, geom = "point")+ 
  labs(
    title = "k-Means Clustering Visualization",
    x = "Principal Component 1",
    y = "Principal Component 2"
  )

##K-Medoids Clustering
fviz_nbclust(numeric_data, pam, method = "silhouette") +
  ggtitle("Silhouette Method for k-medoids")
pam_result <- pam(numeric_data, k = 2)
fviz_cluster(pam_result, data = numeric_data, geom = "point") + 
  labs(
    title = "k-Medoids Clustering Visualization",
    x = "Principal Component 1",
    y = "Principal Component 2"
  )

##Hierarchical Clustering
dist_matrix <- dist(numeric_data)
hclust_result <- hclust(dist_matrix)
plot(hclust_result, labels = FALSE, 
     main = "Dendrogram for Hierarchical Clustering",
     xlab = "Observations",
     ylab = "Height")
hclust_clusters <- cutree(hclust_result, k = 2)
fviz_cluster(list(data = numeric_data, cluster = hclust_clusters)) +
  labs(
    title = "Hierarchical Clustering Visualization",
    x = "Principal Component 1",
    y = "Principal Component 2"
  )

##DBSCAN Clustering
kNNdistplot(numeric_data, k = 5)
abline(h = 0.5, col = "red")
dbscan_result <- dbscan(numeric_data, eps = 0.5, minPts = 5)
title("k-NN Distance Plot for DBSCAN")
fviz_cluster(dbscan_result, data = numeric_data, geom = "point") + 
  labs(
    title = "DBSCAN Clustering Visualization",
    x = "Principal Component 1",
    y = "Principal Component 2"
  )

#Final clustering
data$cluster <- factor(kmeans_result$cluster)
ggplot(data, aes(x = engine.size, y = price, color = cluster)) +
  geom_point() +
  theme_minimal() +
  labs(
    title = "k-Means Clustering: Engine Size vs Price",
    x = "Engine Size",
    y = "Price",
    color = "Cluster"
  )
ggplot(data, aes(x = engine.size, y = city.mpg, color = cluster)) +
  geom_point() +
  theme_minimal() +
  labs(
    title = "k-Means Clustering: Engine Size vs MPG",
    x = "Engine Size",
    y = "MPG",
    color = "Cluster"
  )
