## set PATH
path <- "C:/Users/hj/Documents/dataset_kor"

## set cran mirror 
cran <- getOption("repos")
cran["dmlc"] <- "https://apache-mxnet.s3-accelerate.dualstack.amazonaws.com/R/CRAN/"
options(repos = cran)

## install pakages
install.packages("readxl")
install.packages("ggplot2")
install.packages("dplyr")
install.packages('mlbench')
install.packages("mxnet")

## for using pakages
library(readxl)
library(ggplot2)
library(dplyr)
library(mxnet)

rm(cran)

## read data
file <- "/교통사망사고정보/Kor_Train_교통사망사고정보(12.1~17.6).csv"
accident <- read.csv(paste(path, file, sep=""))

rm(path)
rm(file)

## DNN with mxnet
## ref: https://mxnet.incubator.apache.org/tutorials/r/fiveMinutesNeuralNetwork.html
test <- accident[1:5000, ]
train <- accident[5001:nrow(accident), ]

train.x <- data.matrix(train %>% select(c(요일, 발생지시도, 사고유형_대분류, 사고유형_중분류, 법규위반, 도로형태_대분류, 도로형태)))
train.y <- as.numeric(train$주야)

test.x <- data.matrix(test %>% select(c(요일, 발생지시도, 사고유형_대분류, 사고유형_중분류, 법규위반, 도로형태_대분류, 도로형태)))
test.y <- as.numeric(test$주야)

mx.set.seed(2000)
model <- mx.mlp(train.x, train.y, hidden_node=20, out_node=30, out_activation="softmax",
                num.round=1000, array.batch.size=50, learning.rate=0.01, momentum=0.9,
                eval.metric=mx.metric.accuracy, eval.data = list(data = test.x, label = test.y))

preds = predict(model, test.x)
pred.label = max.col(t(preds))-1
table(pred.label, test.y)
