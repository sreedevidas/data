rm(list = ls())
library(readr)
Prostate_Cancer <- read_csv("C:/Users/TOSHIBA/Downloads/Prostate_Cancer.csv")
View(Prostate_Cancer)
####dividing into train and test samples#####
set.seed(111)
train.index <- sample(row.names(Prostate_Cancer), 0.6*dim(Prostate_Cancer)[1])
valid.index <- setdiff(row.names(Prostate_Cancer), train.index)
train.data <- Prostate_Cancer[train.index,]
valid.data <- Prostate_Cancer[valid.index,]
train.norm.df <- train.data
valid.norm.df <- valid.data
###### normalizing values#############
library(caret)
norm.values <- preProcess(train.data[,3:10], method = c("center", "scale"))
train.norm.df[,3:10] <- predict(norm.values, train.data[,3:10])
valid.norm.df[,3:10] <- predict(norm.values, valid.data[,3:10])
#### applying knn##################
library(FNN)
knn.pred <- knn(train = train.norm.df[,3:10], test = valid.norm.df[,3:10], cl = train.norm.df[,2], k=3)
length(train.norm.df[,3:10])
length(train.norm.df[,2])

############################knn with a different data set########################################
rm(list = ls())
library(readxl)
Course <- read_excel("Course.xlsx")
View(Course)
new.data <- data.frame(Customer = "Prospect",Education = "IT",NoOfYears = 1)
library(caret)
dmy <- dummyVars(~Education, data=Course, fullRank=T)
dummy <- data.frame(predict(dmy, newdata=Course))
Course <- cbind(Course[,-2],dummy)
train.data <-Course[1:2,]
test.data <- Course[3,]
train.data$CT <- c("N","Y")
dmy2 <- dummyVars(~CT, data = train.data, fullRank = T)
dummy2 <- data.frame(predict(dmy2,newdata = train.data))
train.data <- cbind(train.data[,-5], dummy2)
library(FNN)
knn.pred <- knn(train = train.data[,2:4], test = test.data[,2:4], cl= train.data[,5], k=1)
knn.pred
