setwd("D:/Precessing Course/")

data <- read.csv("Titanic-Dataset.csv")
head(data)


summary(data)
sum(is.na(data))
colSums(is.na(data))

data_subset <-  data[, c("Survived", "Sex", "Age", "Pclass")]

data_subset <- na.omit(data_subset)
colSums((is.na(data_subset)))


# Ve du lieu ra
barplot(table(data_subset$Sex))
barplot(table(data_subset$Pclass))

hist(data_subset$Age)


barplot(table(data_subset$Survived))


library(caret)
library(pROC)



train_indices <- createDataPartition(data_subset$Survived, p = 0.8, list = FALSE)

# Chia d??? li???u thành t???p hu???n luy???n và ki???m tra
train_data <- data_subset[train_indices, ]
test_data <- data_subset[-train_indices, ]

print(paste("S??? d??ng trong t???p hu???n luy???n:", nrow(train_data)))  # K???t qu???: 80
print(paste("S??? d??ng trong t???p ki???m tra:", nrow(test_data)))


# Model 1 co Sex, Pclass, Age
model_1 <- glm(Survived ~ Sex + Pclass+ Age , data = train_data, family = binomial)

# Xem k???t qu??? c???a mô h??nh
summary(model_1)


predicted_probabilities <- predict(model_1, newdata = test_data, type = "response")

# Chuy???n ð???i xác su???t thành d??? ðoán nh??? phân v???i ngý???ng 0.5
predicted_classes <- ifelse(predicted_probabilities > 0.5, 1, 0)

# T???o ma tr???n nh???m l???n
confusion_matrix <- table(Predicted = predicted_classes, Actual = test_data$Survived
                          )
print(confusion_matrix)

# Tính ð??? chính xác
accuracy <- mean(predicted_classes == test_data$Survived)
print(paste("Ð??? chính xác:", accuracy))

# V??? ðý???ng cong ROC
roc_curve <- roc(test_data$Survived, predicted_probabilities)
plot(roc_curve, main = "ROC Curve")

# Tính AUC
auc_value <- auc(roc_curve)
print(paste("AUC:", auc_value))







library(pROC)
library(ggplot2)
library(reshape2)

evaluate_model <- function(model, test_data, target_col) {
  # D??? ðoán xác su???t
  predicted_probabilities <- predict(model, newdata = test_data, type = "response")
  
  # Chuy???n ð???i xác su???t thành d??? ðoán nh??? phân v???i ngý???ng 0.5
  predicted_classes <- ifelse(predicted_probabilities > 0.5, 1, 0)
  
  # T???o ma tr???n nh???m l???n
  confusion_matrix <- table(Predicted = predicted_classes, Actual = test_data[[target_col]])
  print(confusion_matrix)
  
  # V??? ma tr???n nh???m l???n
  conf_matrix <- as.matrix(confusion_matrix)
  conf_df <- as.data.frame(melt(conf_matrix))
  
  ggplot(data = conf_df, aes(x = Var2, y = Var1, fill = value)) +
    geom_tile(color = "white") +
    geom_text(aes(label = sprintf("%d", value)), vjust = 1) +
    scale_fill_gradient(low = "white", high = "blue") +
    theme_minimal() +
    labs(title = "Confusion Matrix",
         x = "Actual",
         y = "Predicted") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Tính ð??? chính xác
  accuracy <- mean(predicted_classes == test_data[[target_col]])
  print(paste("Ð??? chính xác:", accuracy))
  
  # V??? ðý???ng cong ROC
  roc_curve <- roc(test_data[[target_col]], predicted_probabilities)
  plot(roc_curve, main = "ROC Curve")
  
  # Tính AUC
  auc_value <- auc(roc_curve)
  print(paste("AUC:", auc_value))
}



# S??? d???ng hàm
# evaluate_model(model_1, test_data, "Survived")

















# Load các gói c???n thi???t
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
trainIndex <- createDataPartition(iris$Species, p = 0.8, list = FALSE)

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
k <- 5
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

classification_report = data.frame(
  Class = levels(testLabels),
  Precision = confMatrix$byClass[, "Pos Pred Value"],
  Recall = confMatrix$byClass[, "Sensitivity"],
  F1 = confMatrix$byClass[, "F1"]
)
print(classification_report)


resultData <- testData
resultData$Predicted <- knnPrediction
resultData$Correct <- ifelse(resultData$Species == resultData$Predicted, "Correct", "InCorrect")




ggplot(resultData, aes(x = Sepal.Length, y = Sepal.Width, color = Correct)) +
  geom_point(size = 3) +
  labs(title = "Sepal Length vs Width", x = "Sepal Length", y = "Sepal Width") +
  theme_minimal() +
  facet_wrap(~ Species)


ggplot(resultData, aes(x = Petal.Length, y = Petal.Width, color = Correct)) +
  geom_point(size = 3) +
  labs(title = "Petal Length vs Width", x = "Petal Length", y = "Pental Width") +
  theme_minimal() +
  facet_wrap(~ Species)



new_data <- data.frame(
  Sepal.Length = c(5.1, 6.5, 7.3),
  Sepal.Width = c(3.5, 3.0, 2.9),
  Petal.Length = c(1.4, 5.5, 6.3),
  Petal.Width = c(0.2, 2.0, 1.8)
)
predicted_label <- knn(train = trainFeatures, test = new_data, cl = trainLabels, k = k)
print(predicted_label)









