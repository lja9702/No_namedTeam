install.packages("readxl")
install.library("ggplot2")
install.library("dplyr")

cran <- getOption("repos")
cran["dmlc"] <- "https://apache-mxnet.s3-accelerate.dualstack.amazonaws.com/R/CRAN/"
options(repos = cran)
install.packages('mlbench')
install.packages("mxnet")
rm(cran)
library(readxl)
library(ggplot2)
library(dplyr)
library(mxnet)
set.seed(4444)

accident <- read.csv('Kor_Train_교통사망사고정보(12.1~17.6).csv')

sample <- accident %>% filter(사상자수 != 1) # 사상자 수가 1인 경우를 제외한 데이터
sample_one <- accident %>% filter(사상자수 == 1)
sample_one <- sample_one[sample(1:nrow(sample_one),4500),]
sample <- rbind(sample, sample_one)

train.x <- data.matrix(sample %>% dplyr::select(c(4,13,14,16,17,18,19,20,22)))
train.y <- sample$사상자수
model <- mx.mlp(train.x, train.y, hidden_node=20, out_node=16, activation="relu", out_activation="softmax",
                num.round=300, array.batch.size=50, learning.rate=0.01, momentum=0.6,
                eval.metric=mx.metric.accuracy, dropout=0.01)

preds = predict(model, train.x)
pred.label = max.col(t(preds))-1
table(pred.label, train.y)

