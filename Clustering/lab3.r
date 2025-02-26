# T???i c�c g�i c???n thi???t
library(tidyverse)
library(ggplot2)
library(car)

setwd("D:/Precessing Course/data3/")



# �???c d??? li???u t??? file CSV
data <- read.csv("Advertising.csv")

# Xem qua d??? li???u
head(data)
summary(data)

# Bi???u �??? ph�n t�n gi???a TV v� Sales
ggplot(data, aes(x = TV, y = Sales)) +
  geom_point() +
  labs(title = "???nh h�???ng c???a qu???ng c�o tr�n TV �???n doanh s??? b�n h�ng",
       x = "Qu???ng c�o tr�n TV",
       y = "Doanh s??? b�n h�ng")

# Bi???u �??? ph�n t�n gi???a Radio v� Sales
ggplot(data, aes(x = Radio, y = Sales)) +
  geom_point() +
  labs(title = "???nh h�???ng c???a qu???ng c�o tr�n Radio �???n doanh s??? b�n h�ng",
       x = "Qu???ng c�o tr�n Radio",
       y = "Doanh s??? b�n h�ng")

# Bi???u �??? ph�n t�n gi???a Newspaper v� Sales
ggplot(data, aes(x = Newspaper, y = Sales)) +
  geom_point() +
  labs(title = "???nh h�???ng c???a qu???ng c�o tr�n b�o ch� �???n doanh s??? b�n h�ng",
       x = "Qu???ng c�o tr�n b�o ch�",
       y = "Doanh s??? b�n h�ng")



# M� h??nh h???i quy tuy???n t�nh
model <- lm(Sales ~ TV + Radio + Newspaper, data = data)

# T�m t???t m� h??nh
summary(model)

# In ra h??? s??? c???a m� h??nh
coef(model)

# T�m t???t k???t qu???
summary(model)



# Phan tich cho bien TV

model_tv <- lm(Sales ~ TV, data=data)
summary(model_tv)

# Phan tich cho biens tv va Radio

model_tv_radio <-  lm(Sales ~ TV + Radio , data = data)
summary(model_tv_radio)


# Phan tich cho bien tv va newspaper

model_tv_news <-  lm(Sales ~ TV + Newspaper , data = data)
summary(model_tv_news)


# chot phuong an cuoi l� tv v� radio la mo hinh tot nhat

vif_values <- vif(model_tv_radio)

# Hi???n th??? h??? s??? VIF
vif_values


# Ki???m tra gi� tr??? VIF
if (all(vif_values < 5)) {
  print("Kh�ng c� d???u hi???u �a c???ng tuy???n nghi�m tr???ng (VIF < 5).")
} else if (all(vif_values < 10)) {
  print("�a c???ng tuy???n ??? m???c ch???p nh???n ��???c (5 <= VIF < 10).")
} else {
  print("�a c???ng tuy???n nghi�m tr???ng (VIF >= 10).")
}

# In ra gi� tr??? VIF
print(vif_values)



# T�nh ma tr???n correlation
cor_matrix <- cor(data)

# Bi???u �??? heatmap cho ma tr???n correlation
library(ggplot2)

# Chuy???n ma tr???n correlation th�nh d???ng tidy �??? v??? heatmap
cor_data <- as.data.frame(cor_matrix) %>%
  rownames_to_column(var = "variable1") %>%
  gather(key = "variable2", value = "correlation", -variable1)

# V??? heatmap
ggplot(cor_data, aes(variable1, variable2, fill = correlation)) +
  geom_tile() +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(title = "Ma tr???n correlation",
       x = "Bi???n",
       y = "Bi???n",
       fill = "Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))












#Cau2--------------------------------------------------------------------------


data2 <- read.csv("bmi.csv")
data2_sub <- data2[, c("Age", "Height", "Weight")]
head(data2)
tail(data2)

summary((data2))
sum(is.na(data2))

cor_matrix <- cor(data2_sub)
print(cor_matrix)

model_2 <- lm(Weight ~ Height + Age , data = data2)

# T�m t???t m� h??nh
summary(model_2)

coef(model_2)

model_2_height <- lm(Weight ~ Height , data = data2)

summary(model_2_height)



# Danh cho height
residals <- resid(model_2_height)

mse <- mean(residals^2)
rmse <- sqrt(mse)
print(paste("Mean squared Error(MSE):", mse))
print(paste("Root Mean squared Error(RMSE):", rmse))

# D�nh cho height v� Age

residals <- resid(model_2)

mse <- mean(residals^2)
rmse <- sqrt(mse)

print(paste("Mean squared Error(MSE):", mse))
print(paste("Root Mean squared Error(RMSE):", rmse))


# chot ham hai bien height l� tot nhat



# Kiem dinh da cong tuyen

vif_values <- vif(model_2)

# Hi???n th??? h??? s??? VIF
vif_values


# Ki???m tra gi� tr??? VIF
if (all(vif_values < 5)) {
  print("Kh�ng c� d???u hi???u �a c???ng tuy???n nghi�m tr???ng (VIF < 5).")
} else if (all(vif_values < 10)) {
  print("�a c???ng tuy???n ??? m???c ch???p nh???n ��???c (5 <= VIF < 10).")
} else {
  print("�a c???ng tuy???n nghi�m tr???ng (VIF >= 10).")
}

# In ra gi� tr??? VIF
print(vif_values)















#Cau3-------------------------------------------------------------------
weather_data <-read.csv("weatherHistory.csv")


# Xem d??? li???u
head(weather_data)
summary(weather_data)

# X�y d???ng m� h??nh h???i quy tuy???n t�nh
model <- lm(Temperature..C. ~ Humidity + Pressure..millibars. + Wind.Speed..km.h., data = weather_data)

# T�m t???t m� h??nh
summary(model)

# Ki???m tra c�c h??? s??? c???a m� h??nh
coef(model)

# ��nh gi� m� h??nh b???ng c�c �??? �o th�ch h???p kh�c (n???u c???n)

# D??? �o�n nhi???t �??? cho m???t s??? d??? li???u m???i (v� d???)
new_data <- data.frame(Humidity = c(0.8, 0.7), 
                       Pressure..millibars. = c(1010, 1020), 
                       Wind.Speed..km.h. = c(10, 15))

predicted_temps <- predict(model, newdata = new_data)
print("Predicted temperatures:")
print(predicted_temps)












#Cau4----------------------------------------------------------------------




# �???c d??? li???u t??? file CSV
data_titanic <- read.csv("Titanic.csv")

# Hi???n th??? t�n c�c c???t
names(data_titanic)

# Ch???n c�c c???t c???n thi???t v� x??? l?? d??? li???u thi???u trong Age
data_subset_titanic <- data_titanic[, c("Survived", "Pclass", "Sex", "Age")]

# Chuy???n �???i d??? li???u gi???i t�nh (Sex) sang d???ng s??? (0 v� 1)
data_subset_titanic$Sex <- ifelse(data_subset_titanic$Sex == "female", 1, 0)

# Ki???m tra c�c gi� tr??? thi???u trong d??? li???u
colSums(is.na(data_subset_titanic))

# Lo???i b??? c�c h�ng c� d??? li???u thi???u
data_subset_titanic <- na.omit(data_subset_titanic)

# S??? d???ng g�i caret �??? chia d??? li???u th�nh t???p hu???n luy???n v� t???p ki???m tra (80/20)
library(caret)
set.seed(123)  # �??? k???t qu??? c� th??? t�i l???p l???i


train_index <- createDataPartition(data_subset_titanic$Survived, p = 0.8, list = FALSE)

train_data <- data_subset_titanic[train_index, ]
test_data <- data_subset_titanic[-train_index, ]

nrow(train_data)
nrow(test_data)

# S??? d???ng m� h??nh h???i quy logistic �??? d??? �o�n kh??? n�ng s???ng s�t
model_logit_titanic <- glm(Survived ~ Pclass + Sex + Age, data = train_data, family = "binomial")


summary(model_logit_titanic)

predictions <- predict(model_logit_titanic, newdata = test_data, type = "response")

# Chuy???n �???i d??? �o�n th�nh d???ng nh??? ph�n (0 v� 1)
predictions_binary <- ifelse(predictions > 0.5, 1, 0)

# ��nh gi� hi???u su???t c???a m� h??nh tr�n t???p ki???m tra
confusion_matrix <- table(predictions_binary, test_data$Survived)
print(confusion_matrix)

# T�nh t??? l??? ph�n lo???i ��ng (accuracy)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Accuracy:", accuracy))




# Model  cho 

model_logit_titanic_delete_sex <- glm(Survived ~ Pclass + Age, data = train_data, family = "binomial")



summary(model_logit_titanic_delete_sex)

predictions <- predict(model_logit_titanic_delete_sex, newdata = test_data, type = "response")

# Chuy???n �???i d??? �o�n th�nh d???ng nh??? ph�n (0 v� 1)
predictions_binary <- ifelse(predictions > 0.5, 1, 0)

# ��nh gi� hi???u su???t c???a m� h??nh tr�n t???p ki???m tra
confusion_matrix <- table(predictions_binary, test_data$Survived)
print(confusion_matrix)

# T�nh t??? l??? ph�n lo???i ��ng (accuracy)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Accuracy:", accuracy))
