# Load required libraries
library(factoextra)
library(dbscan)

# Set working directory and load data (adjust path as necessary)
setwd("D:/Precessing Course")
data_colors <- read.csv("Colors.csv")

# Assuming data_colors is your dataset (replace with your actual dataset)
# Example with data_colors dataset
# Scale the data if necessary
data_colors_scaled <- scale(data_colors)

# Set DBSCAN parameters
eps <- 0.5
minPts <- 5

# Perform DBSCAN clustering
dbscan_result <- dbscan(data_colors_scaled, eps = eps, minPts = minPts)

# Print DBSCAN clustering result
print(dbscan_result)

# Calculate silhouette score
silhouette_score <- silhouette(dbscan_result$cluster, dist(data_colors_scaled))
mean_silhouette_score <- mean(silhouette_score[, "sil_width"])
print(paste("Mean Silhouette Score:", mean_silhouette_score))

# Plot DBSCAN clusters
plot(data_colors_scaled, col = dbscan_result$cluster + 1L, main = "DBSCAN Clustering of Colors Dataset")
legend("topright", legend = c("Cluster 1", "Cluster 2", "Cluster 3", "Noise"), col = 2:5, pch = 19)

# Example of predicting clusters for new data
new_data <- matrix(c(0.1, 0.2, 0.3, 0.4), ncol = 4)  # Example new data point
new_data_scaled <- scale(new_data, center = colMeans(data_colors_scaled), scale = apply(data_colors_scaled, 2, sd))
new_cluster <- predict(dbscan_result, newdata = new_data_scaled)

# Use data_colors for visualization with factoextra fviz_cluster
fviz_cluster(dbscan_result, data = data_colors,
             palette = "jco",
             geom = "point",
             ellipse.type = "convex",
             ggtheme = theme_minimal()) +
  labs(title = "DBSCAN Clustering of Colors Dataset", x = "PC1", y = "PC2")
