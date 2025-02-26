#Thay doi K




library(datasets)
library(pROC)
library(ggplot2)
library(reshape2)
library(caret)
# Load d??? li???u iris
data(iris)
colSums(is.na(iris))
#pHan tich du lieu

ggplot(iris, aes( x = Sepal.Length, y = Sepal.Width)) +
  geom_point(size = 3) +
  labs(title = "Sepal Length vs Width", x = "Sepal Length", y = "Sepal Width") +
  theme_minimal() +
  facet_wrap(~ Species)


ggplot(iris, aes( x = Petal.Length, y = Petal.Width)) +
  geom_point(size = 3) +
  labs(title = "Petal Length vs Width", x = "Petal Length", y = "Petal Width") +
  theme_minimal() +
  facet_wrap(~ Species)

pairs(iris[1:4], col = iris$Species, pch = 19, main = "PairPlot of Iris Dataset")


# Chia d??? li???u thành t???p hu???n luy???n (80%) và t???p ki???m tra (20%)

set.seed(3)
trainIndex <- createDataPartition(iris$Species, p = 0.7, list = FALSE)

# Create training and testing datasets
trainData <- iris[trainIndex, ]
testData <- iris[-trainIndex, ]


trainFeatures <- trainData[,1:4]
trainLabels <- trainData$Species
testFeatures <- testData[,1:4]
testLabels <- testData$Species


# Define target column
target_col <- "Species"

# Apply KNN model
library(class)
k <- 4
knnPrediction <- knn(train = trainFeatures, test = testFeatures, cl = trainLabels, k = k)
confMatrix <- confusionMatrix(knnPrediction, testLabels)
print(confMatrix)




confMatrixTable <- as.table(confMatrix)
confMatrixMelted <- melt(confMatrix$table)
ggplot(data = confMatrixMelted, aes(x = Reference, y = Prediction)) +
  geom_tile(aes(fill = value), color = "cyan") +
  scale_fill_gradient(low = "cyan", high = "steelblue") +
  geom_text(aes(label = value), vjust = 1) +
  labs(title = "Confusion Matrix", x = "Actual", y = "Predicted") +
  theme_minimal()

# Accuracy
accuracy <- mean(knnPrediction == testData[[target_col]])
print(paste("Accuracy:", accuracy))












# Su dung cho svm

# Load necessary libraries
library(datasets)
library(pROC)
library(ggplot2)
library(reshape2)
library(caret)
library(e1071)  # For SVM

# Load iris dataset
data(iris)
colSums(is.na(iris))  # Check for missing values

# Visualize relationships between variables
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_point(aes(color = Species), size = 3) +
  labs(title = "Sepal Length vs Width", x = "Sepal Length", y = "Sepal Width") +
  theme_minimal()

ggplot(iris, aes(x = Petal.Length, y = Petal.Width)) +
  geom_point(aes(color = Species), size = 3) +
  labs(title = "Petal Length vs Width", x = "Petal Length", y = "Petal Width") +
  theme_minimal()

pairs(iris[1:4], col = iris$Species, pch = 19, main = "PairPlot of Iris Dataset")

# Split data into training and testing sets
set.seed(3)
trainIndex <- createDataPartition(iris$Species, p = 0.8, list = FALSE)

# Create training and testing datasets
trainData <- iris[trainIndex, ]
testData <- iris[-trainIndex, ]

# Separate features and target column
trainFeatures <- trainData[, 1:4]
trainLabels <- trainData$Species
testFeatures <- testData[, 1:4]
testLabels <- testData$Species

# Apply SVM model
svm_model <- svm(Species ~ ., data = trainData, kernel = "linear")

# Make predictions
svmPrediction <- predict(svm_model, newdata = testFeatures)

# Evaluate SVM model
confMatrix <- confusionMatrix(svmPrediction, testLabels)
print(confMatrix)

# Visualize confusion matrix
confMatrixTable <- as.table(confMatrix)
confMatrixMelted <- melt(confMatrix$table)
ggplot(data = confMatrixMelted, aes(x = Reference, y = Prediction)) +
  geom_tile(aes(fill = value), color = "cyan") +
  scale_fill_gradient(low = "cyan", high = "steelblue") +
  geom_text(aes(label = value), vjust = 1) +
  labs(title = "Confusion Matrix", x = "Actual", y = "Predicted") +
  theme_minimal()

# Calculate accuracy
accuracy <- mean(svmPrediction == testData$Species)
print(paste("Accuracy:", accuracy))




