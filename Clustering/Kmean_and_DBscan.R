library(datasets)
library(ggplot2)
library(stats)  # �??? s??? d???ng kmeans v� prcomp
library(FactoMineR)  # �??? s??? d???ng PCA
library(scatterplot3d)  # �??? v??? bi???u �??? scatter plot 3D
library(cluster)
# T???i d??? li???u iris
data(iris)

# Ki???m tra v� lo???i b??? c�c gi� tr??? b??? thi???u
iris <- na.omit(iris)
colSums(is.na(iris))  # �???m b???o kh�ng c� gi� tr??? b??? thi???u

# Th???c hi???n PCA
pca_result <- prcomp(iris[, 1:4], scale. = TRUE)

# T�nh to�n v� v??? bi???u �??? Elbow �??? ch???n k ph� h???p
wcss <- numeric(10)  # Kh???i t???o vector �??? l�u tr??? WCSS
for (i in 1:10) {
  kmeans_model <- kmeans(iris[, 1:4], centers = i, nstart = 10)
  wcss[i] <- kmeans_model$tot.withinss
}

# Bi???u �??? Elbow �??? x�c �???nh k t???i �u trong ph�n c???m K-means
elbow_plot <- ggplot(data.frame(K = 1:10, WCSS = wcss), aes(x = K, y = WCSS)) +
  geom_line(color = "blue") +
  geom_point(size = 2, color = "red") +
  labs(title = "Ph��ng ph�p Elbow �??? ch???n s??? l�???ng c???m (k) ph� h???p",
       x = "S??? l�???ng c???m (k)",
       y = "T???ng b??nh ph��ng l???i trong c???m (WCSS)") +
  theme_minimal()

# Hi???n th??? bi???u �??? Elbow
print(elbow_plot)

# X�c �???nh s??? l�???ng c???m k t???i �u t??? bi???u �??? Elbow (trong v� d??? n�y, cho k = 3)
k <- 5

# �p d???ng ph�n c???m K-means tr�n c�c th�nh ph???n PCA
kmeans_model <- kmeans(pca_result$x[, 1:3], centers = k, nstart = 10)
iris$cluster <- as.factor(kmeans_model$cluster)



# T�nh to�n Silhouette score
silhouette_score <- silhouette(kmeans_model$cluster, dist(iris))
mean_silhouette_score <- mean(silhouette_score[, "sil_width"])
print(paste("Mean Silhouette Score:", mean_silhouette_score))

# V??? bi???u �??? �??? tr???c quan h�a c�c c???m
plot(iris[, c("Sepal.Length", "Sepal.Width")], col = kmeans_model$cluster)
points(kmeans_model$centers[, c("Sepal.Length", "Sepal.Width")], col = 1:k, pch = 8, cex = 2)



# V??? bi???u �??? scatter plot 3D c???a d??? li???u �?? ph�n c???m s??? d???ng c�c th�nh ph???n PCA
scatterplot3d(
  x = pca_result$x[, 1],
  y = pca_result$x[, 2],
  z = pca_result$x[, 3],
  color = iris$cluster,
  pch = 16,
  type = "h",
  main = "Ph�n c???m K-means tr�n t???p d??? li???u Iris",
  xlab = "PC1",
  ylab = "PC2",
  zlab = "PC3",
  col.axis = "blue",
  col.grid = "red",
  legend = TRUE
)

# Th�m ch� th�ch m�u s???c
legend("topright", legend = unique(iris$cluster), col = unique(iris$cluster), pch = 16, cex = 0.8, title = "C???m")

















# DBScan
library(factoextra) 
library(dbscan)
data(iris)
iris_data <- iris[, -5]
# Chu???n h�a d??? li???u
iris_scaled <- scale(iris_data)
# �p d???ng DBSCAN
# �???t seed �??? k???t qu??? t�i t???o ��???c
set.seed(123)
# Ch???n tham s??? eps v� minPts
eps <- 0.5
minPts <- 5
dbscan_result <- dbscan(iris_scaled, eps = eps, minPts = minPts)
# In k???t qu??? ph�n c???m
print(dbscan_result)
dbscan_result <- dbscan(iris_data, eps = 0.6, minPts = 5)
print(dbscan_result)

silhouette_score <- silhouette(dbscan_result$cluster, dist(iris_data))
mean_silhouette_score <- mean(silhouette_score[, "sil_width"])
print(mean_silhouette_score)



plot(iris_scaled, col = dbscan_result$cluster + 1L, main = "DBSCAN Clustering of Iris Dataset")
legend("topright", legend = c("Cluster 1", "Cluster 2", "Cluster 3", "Noise"), col = 2:5, pch = 19)
# S??? d???ng m� h??nh �??? ph�n c???m d??? li???u m???i
new_data <- matrix(c(5.1, 3.5, 1.4, 0.2), ncol = 4)
new_data_scaled <- scale(new_data, center = colMeans(iris_data), scale = apply(iris_data, 2, sd))
new_cluster <- predict(dbscan_result, newdata = new_data_scaled)




iris$Cluster <- as.factor(dbscan_result$cluster)
fviz_cluster(dbscan_result, data = iris_data,
             palette = "jco",
             geom = "point",
             ellipse.type = "convex",
             ggtheme = theme_minimal()) +
  labs(title = "DBSCAN Clustering of Iris dataset", x = "PCA 1", y = "PCA 2")
