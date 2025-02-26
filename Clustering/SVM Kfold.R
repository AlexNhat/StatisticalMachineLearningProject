# T???i c�c th� vi???n c???n thi???t
library(datasets)
library(pROC)
library(ggplot2)
library(reshape2)
library(caret)
library(e1071)  # Cho SVM
library(stats)
# T???i t???p d??? li???u iris
data(iris)
# Ki???m tra v� lo???i b??? c�c gi� tr??? b??? thi???u
iris <- na.omit(iris)
colSums(is.na(iris))  # X�c minh r???ng kh�ng c??n gi� tr??? b??? thi???u

# Tr???c quan h�a m???i quan h??? gi???a c�c bi???n
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

# Chia d??? li???u th�nh t???p hu???n luy???n (70%) v� t???p ki???m tra (30%)
trainIndex <- createDataPartition(iris$Species, p = 0.7, list = FALSE)
trainData <- iris[trainIndex, ]
testData <- iris[-trainIndex, ]

# T???o k-fold tr�n t???p d??? li???u hu???n luy???n
folds <- createFolds(trainData$Species, k = 3, list = TRUE, returnTrain = TRUE)

# Kh???i t???o vector �??? l�u tr??? �??? ch�nh x�c v� �??? ch�nh x�c tr�n t???p ki???m tra
accuracies <- numeric(length(folds))
accuracies_for_test <- numeric(length(folds))

# Th???c hi???n k-fold cross-validation tr�n t???p hu???n luy???n
for (i in 1:length(folds)) {
  # T???o t???p d??? li???u hu???n luy???n v� t???p validation cho t???ng fold hi???n t???i
  foldTrainIndex <- folds[[i]]
  foldTrainData <- trainData[foldTrainIndex, ]
  foldValidationData <- trainData[-foldTrainIndex, ]
  
  # �p d???ng m� h??nh SVM
  svm_model <- svm(Species ~ ., data = foldTrainData, kernel = "linear")
  
  # D??? �o�n tr�n t???p validation
  foldPrediction <- predict(svm_model, newdata = foldValidationData[, 1:4])
  
  # T�nh to�n �??? ch�nh x�c tr�n t???p validation
  accuracy <- mean(foldPrediction == foldValidationData$Species)
  accuracies[i] <- accuracy
  
  # D??? �o�n tr�n t???p ki???m tra
  testPrediction <- predict(svm_model, newdata = testData[, 1:4])
  
  # T�nh to�n �??? ch�nh x�c tr�n t???p ki???m tra
  test_accuracy <- mean(testPrediction == testData$Species)
  accuracies_for_test[i] <- test_accuracy
}

# In ra �??? ch�nh x�c c???a t???ng fold tr�n t???p validation
print(paste("�??? ch�nh x�c c???a t???ng fold tr�n t???p validation:", accuracies))

# T�nh to�n v� in ra �??? ch�nh x�c trung b??nh t??? k-fold cross-validation tr�n t???p validation
mean_accuracy <- mean(accuracies)
print(paste("�??? ch�nh x�c trung b??nh t??? k-fold CV tr�n t???p validation:", mean_accuracy))

# In ra �??? ch�nh x�c c???a t???ng fold tr�n t???p ki???m tra
print(paste("�??? ch�nh x�c c???a t???ng fold tr�n t???p ki???m tra:", accuracies_for_test))

# T�nh to�n v� in ra �??? ch�nh x�c trung b??nh t??? k-fold cross-validation tr�n t???p ki???m tra
mean_accuracy_test <- mean(accuracies_for_test)
print(paste("�??? ch�nh x�c trung b??nh t??? k-fold CV tr�n t???p ki???m tra:", mean_accuracy_test))






# so sanh voi model duoc cho tat ca data tu tap train
# Hu???n luy???n m� h??nh cu???i c�ng tr�n to�n b??? t???p hu???n luy???n
final_svm_model <- svm(Species ~ ., data = trainData, kernel = "linear")

# D??? �o�n tr�n t???p ki???m tra
finalPrediction <- predict(final_svm_model, newdata = testData[, 1:4])

# T�nh to�n v� in ra �??? ch�nh x�c, �??? ch�nh x�c d??? �o�n (precision), �??? nh???y (recall) v� F1 score tr�n t???p ki???m tra
confMatrix <- confusionMatrix(finalPrediction, testData$Species)
print(confMatrix)

accuracy <- confMatrix$overall['Accuracy']
precision <- confMatrix$byClass['Pos Pred Value']
recall <- confMatrix$byClass['Sensitivity']
f1 <- 2 * (precision * recall) / (precision + recall)

print(paste("�??? ch�nh x�c tr�n t???p ki???m tra:", accuracy))
print(paste("�??? ch�nh x�c d??? �o�n tr�n t???p ki???m tra:", precision))
print(paste("�??? nh???y tr�n t???p ki???m tra:", recall))
print(paste("F1 Score tr�n t???p ki???m tra:", f1))

# Tr???c quan h�a �??? ch�nh x�c t??? k-fold CV tr�n t???p validation
accuracies_df <- data.frame(Fold = 1:length(accuracies), Accuracy = accuracies)
ggplot(accuracies_df, aes(x = factor(Fold), y = Accuracy)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "�??? ch�nh x�c c???a t???ng fold tr�n t???p validation", x = "Fold", y = "�??? ch�nh x�c") +
  theme_minimal()

# Tr???c quan h�a �??? ch�nh x�c t??? k-fold CV tr�n t???p ki???m tra
accuracies_test_df <- data.frame(Fold = 1:length(accuracies_for_test), Accuracy = accuracies_for_test)
ggplot(accuracies_test_df, aes(x = factor(Fold), y = Accuracy)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "�??? ch�nh x�c c???a t???ng fold tr�n t???p ki???m tra", x = "Fold", y = "�??? ch�nh x�c") +
  theme_minimal()
