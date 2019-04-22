rm(list = ls())
library(readxl)
iris <- read.csv("C:/Users/TOSHIBA/Desktop/iris.csv", header = FALSE)
View(iris)
set.seed(111)
train.index <- sample(row.names(iris),0.6*dim(iris)[1])
valid.index <- setdiff(row.names(iris), train.index)
train.data <- iris[train.index,]
valid.data <- iris[valid.index,]
new.df <- data.frame(V1 = 8.0, V2 = 4.0, V3 = 5.4, V4 = 0.2)
plot(V1~V2, data = train.data, pch = ifelse(train.data$V1,1,3))
text(train.data$V1, train.data$V2, rownames(train.data), pos = 4)
text(8.0,4.0, "X")
legend("topright",c("V1","V2","newvalues"), pch = c(1,3,4))
library(FNN)
install.packages("FNN")
nn <- knn(train = train.data[,1:4], test = new.df,cl = train.data[,5], k = 3)
row.names(train.data)[attr(nn,"nn.index")]
library(caret)
knn.pred <- knn(train = train.data[,1:4], test = valid.data[,1:4], cl = train.data[,5], k=3)
cm<-confusionMatrix(knn.pred, valid.data[,5])
cm$overall[1]
accuracy.df <- data.frame(k = seq(1,14,1), accuracy = rep(0,14))
for (i in 1:14){
  knn.pred2 <- knn(train =  train.data[,1:4], test = valid.data[,1:4], cl = train.data[,5], k=i)
  accuracy.df[i,2] <- confusionMatrix(knn.pred2, valid.data[,5])$overall[1]
  }
plot(accuracy.df)
