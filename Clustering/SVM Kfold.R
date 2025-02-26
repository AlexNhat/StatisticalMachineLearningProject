# T???i các thý vi???n c???n thi???t
library(datasets)
library(pROC)
library(ggplot2)
library(reshape2)
library(caret)
library(e1071)  # Cho SVM
library(stats)
# T???i t???p d??? li???u iris
data(iris)
# Ki???m tra và lo???i b??? các giá tr??? b??? thi???u
iris <- na.omit(iris)
colSums(is.na(iris))  # Xác minh r???ng không c??n giá tr??? b??? thi???u

# Tr???c quan hóa m???i quan h??? gi???a các bi???n
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_point(aes(color = Species), size = 3) +
  labs(title = "Sepal Length vs Width", x = "Sepal Length", y = "Sepal Width") +
  theme_minimal()

ggplot(iris, aes(x = Petal.Length, y = Petal.Width)) +
  geom_point(aes(color = Species), size = 3) +
  labs(title = "Petal Length vs Width", x = "Petal Length", y = "Petal Width") +
  theme_minimal()

pairs(iris[1:4], col = iris$Species, pch = 19, main = "PairPlot of Iris Dataset")


set.seed(3)

# Chia d??? li???u thành t???p hu???n luy???n (70%) và t???p ki???m tra (30%)
trainIndex <- createDataPartition(iris$Species, p = 0.7, list = FALSE)
trainData <- iris[trainIndex, ]
testData <- iris[-trainIndex, ]

# T???o k-fold trên t???p d??? li???u hu???n luy???n
folds <- createFolds(trainData$Species, k = 3, list = TRUE, returnTrain = TRUE)

# Kh???i t???o vector ð??? lýu tr??? ð??? chính xác và ð??? chính xác trên t???p ki???m tra
accuracies <- numeric(length(folds))
accuracies_for_test <- numeric(length(folds))

# Th???c hi???n k-fold cross-validation trên t???p hu???n luy???n
for (i in 1:length(folds)) {
  # T???o t???p d??? li???u hu???n luy???n và t???p validation cho t???ng fold hi???n t???i
  foldTrainIndex <- folds[[i]]
  foldTrainData <- trainData[foldTrainIndex, ]
  foldValidationData <- trainData[-foldTrainIndex, ]
  
  # Áp d???ng mô h??nh SVM
  svm_model <- svm(Species ~ ., data = foldTrainData, kernel = "linear")
  
  # D??? ðoán trên t???p validation
  foldPrediction <- predict(svm_model, newdata = foldValidationData[, 1:4])
  
  # Tính toán ð??? chính xác trên t???p validation
  accuracy <- mean(foldPrediction == foldValidationData$Species)
  accuracies[i] <- accuracy
  
  # D??? ðoán trên t???p ki???m tra
  testPrediction <- predict(svm_model, newdata = testData[, 1:4])
  
  # Tính toán ð??? chính xác trên t???p ki???m tra
  test_accuracy <- mean(testPrediction == testData$Species)
  accuracies_for_test[i] <- test_accuracy
}

# In ra ð??? chính xác c???a t???ng fold trên t???p validation
print(paste("Ð??? chính xác c???a t???ng fold trên t???p validation:", accuracies))

# Tính toán và in ra ð??? chính xác trung b??nh t??? k-fold cross-validation trên t???p validation
mean_accuracy <- mean(accuracies)
print(paste("Ð??? chính xác trung b??nh t??? k-fold CV trên t???p validation:", mean_accuracy))

# In ra ð??? chính xác c???a t???ng fold trên t???p ki???m tra
print(paste("Ð??? chính xác c???a t???ng fold trên t???p ki???m tra:", accuracies_for_test))

# Tính toán và in ra ð??? chính xác trung b??nh t??? k-fold cross-validation trên t???p ki???m tra
mean_accuracy_test <- mean(accuracies_for_test)
print(paste("Ð??? chính xác trung b??nh t??? k-fold CV trên t???p ki???m tra:", mean_accuracy_test))






# so sanh voi model duoc cho tat ca data tu tap train
# Hu???n luy???n mô h??nh cu???i cùng trên toàn b??? t???p hu???n luy???n
final_svm_model <- svm(Species ~ ., data = trainData, kernel = "linear")

# D??? ðoán trên t???p ki???m tra
finalPrediction <- predict(final_svm_model, newdata = testData[, 1:4])

# Tính toán và in ra ð??? chính xác, ð??? chính xác d??? ðoán (precision), ð??? nh???y (recall) và F1 score trên t???p ki???m tra
confMatrix <- confusionMatrix(finalPrediction, testData$Species)
print(confMatrix)

accuracy <- confMatrix$overall['Accuracy']
precision <- confMatrix$byClass['Pos Pred Value']
recall <- confMatrix$byClass['Sensitivity']
f1 <- 2 * (precision * recall) / (precision + recall)

print(paste("Ð??? chính xác trên t???p ki???m tra:", accuracy))
print(paste("Ð??? chính xác d??? ðoán trên t???p ki???m tra:", precision))
print(paste("Ð??? nh???y trên t???p ki???m tra:", recall))
print(paste("F1 Score trên t???p ki???m tra:", f1))

# Tr???c quan hóa ð??? chính xác t??? k-fold CV trên t???p validation
accuracies_df <- data.frame(Fold = 1:length(accuracies), Accuracy = accuracies)
ggplot(accuracies_df, aes(x = factor(Fold), y = Accuracy)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Ð??? chính xác c???a t???ng fold trên t???p validation", x = "Fold", y = "Ð??? chính xác") +
  theme_minimal()

# Tr???c quan hóa ð??? chính xác t??? k-fold CV trên t???p ki???m tra
accuracies_test_df <- data.frame(Fold = 1:length(accuracies_for_test), Accuracy = accuracies_for_test)
ggplot(accuracies_test_df, aes(x = factor(Fold), y = Accuracy)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Ð??? chính xác c???a t???ng fold trên t???p ki???m tra", x = "Fold", y = "Ð??? chính xác") +
  theme_minimal()
