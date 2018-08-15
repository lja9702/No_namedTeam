install.packages("readxl")
install.packages("ggplot2")
install.packages("dplyr")
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

accident <- read.csv('Kor_Train_교통사망사고정보(12.1~17.6).csv')

test <- accident[1:5000, ]
train <- accident[5001:nrow(accident),]

sample <- train %>% filter(사상자수 != 1) # 사상자 수가 1인 경우를 제외한 데이터
sample_one <- train %>% filter(사상자수 == 1)
sample_one <- sample_one[sample(1:nrow(sample_one),2500),]
sample <- rbind(sample, sample_one)

train.x <- data.matrix(sample %>% dplyr::select(c(4,13,14,16,17,18,19,20,22)))
train.y <- sample$사상자수

test.x <- data.matrix(test %>% dplyr::select(c(4,13,14,16,17,18,19,20,22)))
test.y <- test$사상자수

set.seed(4444)
mx.set.seed(4444)
model <- mx.mlp(train.x, train.y, hidden_node=15, out_node=20, activation="relu", out_activation="softmax",
                num.round=3000, array.batch.size=50, learning.rate=0.01, momentum=0.9,
                eval.metric=mx.metric.accuracy, eval.data = list(data = test.x, label = test.y))
#결과 :
#Train-accuacy = 0.61797980146376
#Validation-accuacy = 0.712600001692772

preds = predict(model, test.x)
pred.label = max.col(t(preds))-1
table(pred.label, test.y)

result <- cbind(as.data.frame(pred.label), as.data.frame(test.y))
result_len <- nrow(result)
result_correct <- nrow(result %>% filter(pred.label == test.y))
result_correct/result_len # Accuracy

