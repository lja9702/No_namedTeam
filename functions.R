# 주야
day_night <- function(path, learning_rate, out_node, hidden_node, round, seed)
{
  file <- "Kor_Train_교통사망사고정보(12.1~17.6).csv"
  accident <- read.csv(paste(path, file, sep=""))
  
  test <- accident[1:5000, ]
  train <- accident[5001:nrow(accident), ]
  
  train.x <- data.matrix(train %>% select(c(요일, 발생지시도, 사고유형_대분류, 사고유형_중분류, 법규위반, 도로형태_대분류, 도로형태)))
  train.y <- as.numeric(train$주야)
  
  test.x <- data.matrix(test %>% select(c(요일, 발생지시도, 사고유형_대분류, 사고유형_중분류, 법규위반, 도로형태_대분류, 도로형태)))
  test.y <- as.numeric(test$주야)
  
  mx.set.seed(seed)
  model <- mx.mlp(train.x, train.y, hidden_node=hidden_node, out_node=out_node, out_activation="softmax",
                  num.round=round, array.batch.size=50, learning.rate=learning_rate, momentum=0.9,
                  eval.metric=mx.metric.accuracy, eval.data = list(data = test.x, label = test.y))
  
  preds = predict(model, test.x)
  pred.label = max.col(t(preds))-1
  table(pred.label, test.y)
  
  return(model)
}

# 요이
week <- function(path, learning_rate, out_node, hidden_node, round, seed)
{
  file <- "Kor_Train_교통사망사고정보(12.1~17.6).csv"
  accident <- read.csv(paste(path, file, sep=""))
  
  test <- accident[1:5000, ]
  train <- accident[5001:nrow(accident), ]
  
  train.x <- data.matrix(train %>% select(c(주야, 사고유형_대분류, 사고유형_중분류, 법규위반, 도로형태, 당사자종별_1당_대분류, 당사자종별_2당_대분류)))
  train.y <- as.numeric(train$요일)
  
  test.x <- data.matrix(test %>% select(c(주야, 사고유형_대분류, 사고유형_중분류, 법규위반, 도로형태, 당사자종별_1당_대분류, 당사자종별_2당_대분류)))
  test.y <- as.numeric(test$요일)
  
  mx.set.seed(seed)
  model <- mx.mlp(train.x, train.y, hidden_node=hidden_node, out_node=out_node, out_activation="softmax",
                  num.round=round, array.batch.size=50, learning.rate=learning_rate, momentum=0.9,
                  eval.metric=mx.metric.accuracy, eval.data = list(data = test.x, label = test.y))
  
  preds = predict(model, test.x)
  pred.label = max.col(t(preds))-1
  table(pred.label, test.y)일
  
  return (model)
}

# 법규위반
violation <- function(path, learning_rate, out_node, hidden_node, round, seed)
{
  file <- "Kor_Train_교통사망사고정보(12.1~17.6).csv"
  accident <- read.csv(paste(path, file, sep=""))
  
  test <- accident[1:5000, ]
  train <- accident[5001:nrow(accident), ]
  
  train.x <- data.matrix(sample %>% select(c(주야, 발생지시도, 사고유형_대분류, 사고유형_중분류, 도로형태_대분류, 도로형태, 당사자종별_1당_대분류, 당사자종별_2당_대분류)))
  train.y <- as.numeric(sample$법규위반)
  
  test.x <- data.matrix(test %>% select(c(주야, 발생지시도, 사고유형_대분류, 사고유형_중분류, 도로형태_대분류, 도로형태, 당사자종별_1당_대분류, 당사자종별_2당_대분류)))
  test.y <- as.numeric(test$법규위반)
  
  mx.set.seed(seed)
  model <- mx.mlp(train.x, train.y, hidden_node=hidden_node, out_node=out_node, out_activation="softmax",
                  num.round=round, array.batch.size=50, learning.rate=learning_rate, momentum=0.9,
                  eval.metric=mx.metric.accuracy, eval.data = list(data = test.x, label = test.y))
  
  return (model)
}

# 사상자수
injury_cnt <- function(path, learning_rate, out_node, hidden_node, round, seed)
{
  file <- "Kor_Train_교통사망사고정보(12.1~17.6).csv"
  accident <- read.csv(paste(path, file, sep=""))
  
  test <- accident[1:5000, ]
  train <- accident[5001:nrow(accident),]
  
  sample <- train %>% filter(사상자수 != 1) # 사상자 수가 1인 경우를 제외한 데이터
  sample_one <- train %>% filter(사상자수 == 1)
  sample_one <- sample_one[sample(1:nrow(sample_one),2500),]
  sample <- rbind(sample, sample_one)
  
  train.x <- data.matrix(sample %>% dplyr::select(c(4,6,14,16,17,18,19,20,22)))
  train.y <- sample$사상자수
  
  test.x <- data.matrix(test %>% dplyr::select(c(4,6,14,16,17,18,19,20,22)))
  test.y <- test$사상자수
  
  set.seed(4444)
  mx.set.seed(4444)
  model <- mx.mlp(train.x, train.y, hidden_node=hidden_node, out_node=out_node, out_activation="softmax",
                  num.round=round, array.batch.size=50, learning.rate=learning_rate, momentum=0.9,
                  eval.metric=mx.metric.accuracy, eval.data = list(data = test.x, label = test.y))
  return (model)
}