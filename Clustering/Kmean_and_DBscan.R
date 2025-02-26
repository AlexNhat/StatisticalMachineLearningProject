library(datasets)
library(ggplot2)
library(stats)  # Ð??? s??? d???ng kmeans và prcomp
library(FactoMineR)  # Ð??? s??? d???ng PCA
library(scatterplot3d)  # Ð??? v??? bi???u ð??? scatter plot 3D
library(cluster)
# T???i d??? li???u iris
data(iris)

# Ki???m tra và lo???i b??? các giá tr??? b??? thi???u
iris <- na.omit(iris)
colSums(is.na(iris))  # Ð???m b???o không có giá tr??? b??? thi???u

# Th???c hi???n PCA
pca_result <- prcomp(iris[, 1:4], scale. = TRUE)

# Tính toán và v??? bi???u ð??? Elbow ð??? ch???n k phù h???p
wcss <- numeric(10)  # Kh???i t???o vector ð??? lýu tr??? WCSS
for (i in 1:10) {
  kmeans_model <- kmeans(iris[, 1:4], centers = i, nstart = 10)
  wcss[i] <- kmeans_model$tot.withinss
}

# Bi???u ð??? Elbow ð??? xác ð???nh k t???i ýu trong phân c???m K-means
elbow_plot <- ggplot(data.frame(K = 1:10, WCSS = wcss), aes(x = K, y = WCSS)) +
  geom_line(color = "blue") +
  geom_point(size = 2, color = "red") +
  labs(title = "Phýõng pháp Elbow ð??? ch???n s??? lý???ng c???m (k) phù h???p",
       x = "S??? lý???ng c???m (k)",
       y = "T???ng b??nh phýõng l???i trong c???m (WCSS)") +
  theme_minimal()

# Hi???n th??? bi???u ð??? Elbow
print(elbow_plot)

# Xác ð???nh s??? lý???ng c???m k t???i ýu t??? bi???u ð??? Elbow (trong ví d??? này, cho k = 3)
k <- 5

# Áp d???ng phân c???m K-means trên các thành ph???n PCA
kmeans_model <- kmeans(pca_result$x[, 1:3], centers = k, nstart = 10)
iris$cluster <- as.factor(kmeans_model$cluster)



# Tính toán Silhouette score
silhouette_score <- silhouette(kmeans_model$cluster, dist(iris))
mean_silhouette_score <- mean(silhouette_score[, "sil_width"])
print(paste("Mean Silhouette Score:", mean_silhouette_score))

# V??? bi???u ð??? ð??? tr???c quan hóa các c???m
plot(iris[, c("Sepal.Length", "Sepal.Width")], col = kmeans_model$cluster)
points(kmeans_model$centers[, c("Sepal.Length", "Sepal.Width")], col = 1:k, pch = 8, cex = 2)



# V??? bi???u ð??? scatter plot 3D c???a d??? li???u ð?? phân c???m s??? d???ng các thành ph???n PCA
scatterplot3d(
  x = pca_result$x[, 1],
  y = pca_result$x[, 2],
  z = pca_result$x[, 3],
  color = iris$cluster,
  pch = 16,
  type = "h",
  main = "Phân c???m K-means trên t???p d??? li???u Iris",
  xlab = "PC1",
  ylab = "PC2",
  zlab = "PC3",
  col.axis = "blue",
  col.grid = "red",
  legend = TRUE
)

# Thêm chú thích màu s???c
legend("topright", legend = unique(iris$cluster), col = unique(iris$cluster), pch = 16, cex = 0.8, title = "C???m")

















# DBScan
library(factoextra) 
library(dbscan)
data(iris)
iris_data <- iris[, -5]
# Chu???n hóa d??? li???u
iris_scaled <- scale(iris_data)
# Áp d???ng DBSCAN
# Ð???t seed ð??? k???t qu??? tái t???o ðý???c
set.seed(123)
# Ch???n tham s??? eps và minPts
eps <- 0.5
minPts <- 5
dbscan_result <- dbscan(iris_scaled, eps = eps, minPts = minPts)
# In k???t qu??? phân c???m
print(dbscan_result)
dbscan_result <- dbscan(iris_data, eps = 0.6, minPts = 5)
print(dbscan_result)

silhouette_score <- silhouette(dbscan_result$cluster, dist(iris_data))
mean_silhouette_score <- mean(silhouette_score[, "sil_width"])
print(mean_silhouette_score)



plot(iris_scaled, col = dbscan_result$cluster + 1L, main = "DBSCAN Clustering of Iris Dataset")
legend("topright", legend = c("Cluster 1", "Cluster 2", "Cluster 3", "Noise"), col = 2:5, pch = 19)
# S??? d???ng mô h??nh ð??? phân c???m d??? li???u m???i
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
