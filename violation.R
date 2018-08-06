## set cran mirror 
cran <- getOption("repos")
cran["dmlc"] <- "https://apache-mxnet.s3-accelerate.dualstack.amazonaws.com/R/CRAN/"
options(repos = cran)

## install pakages
install.packages("readxl")
install.library("ggplot2")
install.library("dplyr")
install.packages('mlbench')
install.packages("mxnet")

## for using pakages
library(readxl)
library(ggplot2)
library(dplyr)
library(mxnet)

rm(cran)

## read data
accident <- read.csv("dataset_kor/교통사망사고정보/Train_교통사망사고정보(12.1~17.6).csv")
seoul_weather <- read.csv("dataset_kor/SupportData/11.일별구별기상관측/일별기상관측(2012~2017).csv")

## data 형식 변경 
# "발생년월일시"를 yyyymmdd 형식으로 바꾸기
date <- as.character(accident$발생년월일시)
r = nrow(accident)  # row의 총 개수
i <- 1
for (i in r) {
  date[i] <- substr(date[i],1, 8)
}

# 서울지역의 "XX구"를 "XX" 형식으로 바꾸기 (예 : 서초구 -> 서초)
location_do <- as.character(accident$발생지시도)
location_gu <- as.character(accident$발생지시군구)
i = 1
for (i in r) {
  print(location_do[i])
  if (location_do[i] == "서울") {
    len = nchar(location_gu[i])
    location_gu[i] <- substr(location_gu[i], 1, len-1)
    print(location_gu[i])
  }
}

accident <- cbind(date, accident)         # accident에 date를 추가
accident <- cbind(location_gu, accident)  # accident에 location_gu를 추가

rm(date)
rm(location_do)
rm(location_gu)

### str(accident)
### head(accident)

left_join(accident, seoul_weather, by = c("date" = "관측일자" & "location_gu" = "지점명"))


## DNN with mxnet
## ref: https://mxnet.incubator.apache.org/tutorials/r/fiveMinutesNeuralNetwork.html
test <- accident[1:5000, ]
train <- accident[5001:nrow(accident),]
sample <- train %>% filter(법규위반 != "안전운전 의무 불이행") # 기타 단일로를 제외한 데이터
sample_law <- train %>% filter(법규위반 == "안전운전 의무 불이행")
sample_law <- sample_law[sample(1:nrow(sample_law),1800),]
sample <- rbind(sample, sample_law)

train.x <- data.matrix(sample %>% select(c(4,13,14,18,19,20,22)))
train.y <- as.numeric(sample$법규위반)
test.x <- data.matrix(test %>% select(c(4,13,14,18,19,20,22)))
test.y <- as.numeric(test$법규위반)

mx.set.seed(2000)
model <- mx.mlp(train.x, train.y, hidden_node=15, out_node=20, out_activation="softmax",
                num.round=500, array.batch.size=50, learning.rate=0.01, momentum=0.9,
                eval.metric=mx.metric.accuracy, eval.data = list(data = test.x, label = test.y))

preds = predict(model, test.x)
pred.label = max.col(t(preds))-1
table(pred.label, test.y)
