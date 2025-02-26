
library(ggplot2)
options(warn = -1)
library(tidyverse)


# Cau 1----------------------------------------------------

diem_so = c(85, 92, 78, 90, 88, 76, 85, 89, 92, 95, 80, 87, 82, 91, 84)

cau1 <- function(diem_so){
  #1a
  gia_tri_trung_binh <- mean(diem_so)
  cat("Giá tr??? trung b??nh:", gia_tri_trung_binh, "\n")
  
 
  do_lech_chuan <- sd(diem_so)
  cat("Ð??? l???ch chu???n:", do_lech_chuan, "\n")
  
  #1b
  hist(diem_so, main="Bi???u ð??? Histogram c???a Ði???m S???", xlab="Ði???m S???", ylab="T???n S???", col="lightblue", border="black")
  
  
  
  
}

cau1(diem_so)

#--------------------------------------------------------------------------------------------------------






#Cau 2 -------------------------------------------------------------

# 2a
# T???o b???ng v???i hai bi???n Height và Gender
Height <- c(175, 180, 172, 165, 160, 168, 160, 163, 170, 176)
Gender <- c("Male", "Male", "Female", "Male", "Female", "Female", "Male", "Female", "Male", "Female")
data <- data.frame(Height, Gender)


#2b
# Hàm th???c hi???n ki???m ð???nh t ð??? so sánh giá tr??? trung b??nh c???a hai nhóm
kiem_dinh_t <- function(data) {
  male_height <- data$Height[data$Gender == "Male"]
  female_height <- data$Height[data$Gender == "Female"]
  t_test_result <- t.test(male_height, female_height)
  return(t_test_result)
}

#2c

# Hàm v??? bi???u ð??? boxplot týõng ???ng
ve_boxplot <- function(data) {
  boxplot(Height ~ Gender, data=data, main="Boxplot of Height by Gender",
          xlab="Gender", ylab="Height", col=c("lightblue", "lightpink"))
}


t_test_result <- kiem_dinh_t(data)
print(t_test_result)

ve_boxplot(data)

#------------------------------------------------------------------------
  









# Cau3----------------------------------------------------------------------

group1 <- c(60, 65, 68, 72, 56, 58, 63, 70, 75, 80)
group2 <- c(45, 50, 52, 58, 55, 48, 50, 57, 62, 68)

t_test_result <- t.test(group1, group2)

# In k???t qu???
print(t_test_result)

#---------------------------------------------------------------------------






#Cau4-----------------------------------------------------------------------


school_A <- c(85, 92, 78, 90, 88)
school_B <- c(76, 85, 89, 92, 95)
school_C <- c(80, 87, 82, 91, 84)


score <- c(school_A, school_B, school_C)
school <- factor(rep(c("A", "B", "C"), each=5))

data <- data.frame(score, school)

anova_result <- aov(score ~ school, data=data)
summary(anova_result)

#----------------------------------------------------------------------------











#Cau 5------------------------------------------------------------------------

height <- c(165, 170, 175, 180, 155, 160, 168, 172, 158, 162)
weight <- c(60, 70, 75, 80, 50, 55, 65, 68, 52, 58)

# V??? bi???u ð??? scatter plot
plot(height, weight, 
     main="Scatter Plot of Height vs. Weight",
     xlab="Height (cm)",
     ylab="Weight (kg)",
     pch=19,          
     col="blue")      
grid()                

#-----------------------------------------------------------------------------










#Cau6------------------------------------------------------------------------

month <- c("Tháng 1", "Tháng 2", "Tháng 3", "Tháng 4", "Tháng 5")
sales <- c(1200, 1500, 1800, 1350, 1600)

# V??? bi???u ð??? bar chart
barplot(sales, 
        names.arg = month, 
        main = "S??? lý???ng s???n ph???m bán ra theo tháng",
        xlab = "Tháng", 
        ylab = "S??? lý???ng s???n ph???m",
        col = "lightblue",     
        ylim = c(0, 2000))     
grid()                        

#--------------------------------------------------------------------------










#CAu7------------------------------------------------------------------------

library(ggplot2)


data <- data.frame(
  Date = as.Date(c("2022-01-01", "2022-01-02", "2022-01-03", "2022-01-04", "2022-01-05")),
  Sales = c(12000, 15000, 18000, 13500, 16000)
)

ggplot(data, aes(x = Date, y = Sales)) +
  geom_line() +
  geom_point() +
  labs(x = "Date", y = "Sales", title = "xu hý???ng sales")

#-----------------------------------------------------------------------------











#Cau8-------------------------------------------------------------------------

library(ggplot2)


school_A <- c(85, 92, 78, 90, 88, 76, 85, 89, 92, 95)
school_B <- c(80, 87, 82, 91, 84, 70, 75, 78, 86, 88)
school <- c(rep("A", length(school_A)), rep("B", length(school_B)))
scores <- c(school_A, school_B)

# T???o d??? li???u
data <- data.frame(School = factor(school), Score = scores)

# v??? boxplot
ggplot(data, aes(x = School, y = Score, fill = School)) +
  geom_boxplot() +
  labs(x = "School", y = "Score", title = "boxplot c???a score ð???i v???i trý???ng A và B")

# Shapiro-Wilk 
shapiro_test_A <- shapiro.test(school_A)
shapiro_test_B <- shapiro.test(school_B)

cat("Shapiro-Wilk Test A:\n")
print(shapiro_test_A)
cat("\n")
cat("Shapiro-Wilk Test B:\n")
print(shapiro_test_B)


#------------------------------------------------------------------------------














# Cau9-------------------------------------------------------------------------


data <- data.frame(
  Month = c("Tháng 1", "Tháng 2", "Tháng 3", "Tháng 4", "Tháng 5"),
  Sales = c(12000, 15000, 18000, 13500, 16000),
  Revenue = c(5000, 6000, 7500, 5500, 7000)
)


ggplot(data, aes(x = Month)) +
  geom_col(aes(y = Sales, fill = "Sales"), position = "dodge") +
  geom_line(aes(y = Revenue * 0.1, group = 1, color = "Revenue"), size = 1.5) +
  geom_point(aes(y = Revenue * 0.1, group = 1, color = "Revenue"), size = 4) +
  scale_y_continuous(sec.axis = sec_axis(~./0.1, name = "Revenue")) +
  labs(x = "Month", y = "Sales", title = "Sales and Revenue Comparison") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_color_manual(values = c("Revenue" = "blue")) +
  scale_fill_manual(values = c("Sales" = "orange")) +
  guides(color = guide_legend(title = "Revenue", override.aes = list(size = 4)))

#-------------------------------------------------------------------------------







# Các câu b??? sung thêm

setwd("D:/Precessing Course/data/data/")


data <- readLines("data.txt")



































