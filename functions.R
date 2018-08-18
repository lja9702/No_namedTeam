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
  
  set.seed(seed)
  mx.set.seed(seed)
  model <- mx.mlp(train.x, train.y, hidden_node=hidden_node, out_node=out_node, out_activation="softmax",
                  num.round=round, array.batch.size=50, learning.rate=learning_rate, momentum=0.9,
                  eval.metric=mx.metric.accuracy, eval.data = list(data = test.x, label = test.y))
  return (model)
}

# 사고유형
# accident_type(path="", learning_rate=0.07, out_node=22, hidden_node=10, round=130, seed=0)
accident_type <- function(path, learning_rate, out_node, hidden_node, round, seed) {
  
  file <- "Kor_Train_교통사망사고정보(12.1~17.6).csv"
  
  accident <- read.csv(paste(path, file, sep="/"))

  #1당 2당 소분류 필요없어서 제외
  accident <- accident[,-21]
  accident <- accident[,-22]

  #진아_ 사고유형별 사망, 사상, 중상, 경상, 부상신고자 수 딥러닝
  accident.temp <- cbind(accident$사망자수, accident$중상자수, accident$경상자수, accident$부상신고자수, accident$당사자종별_1당, accident$당사자종별_2당, accident$법규위반)
  colnames(accident.temp) <- c("사망자수", "중상자수", "경상자수", "부상신고자수", "당사자종별_1당", "당사자종별_2당", "법규위반")
  accident.temp <- as.data.frame(accident.temp)
  
  accident.temp$사망자수 <- as.numeric(accident.temp$사망자수)
  accident.temp$중상자수 <- as.numeric(accident.temp$중상자수)
  accident.temp$경상자수 <- as.numeric(accident.temp$경상자수)
  accident.temp$부상신고자수 <- as.numeric(accident.temp$부상신고자수)
  
  sagou.temp <- as.data.frame(accident$사고유형)
  colnames(sagou.temp) <- c("사고유형")
  accident.scale <- cbind(accident.temp, sagou.temp)
  accident.scale[, 8] <- as.numeric(accident.scale[, 8])
  
  acsample <- sample(1:nrow(accident.scale), size = round(0.2 * nrow(accident.scale)))
  test.x <- data.matrix(accident.scale[acsample, 1: 7])
  test.y <- accident.scale[acsample, 8]
  train.x <- data.matrix(accident.scale[-acsample, 1:7])
  train.y <- accident.scale[-acsample, 8]
  # 41%정확도
  mx.set.seed(seed)
  model <- mx.mlp(train.x, train.y, hidden_node=hidden_node, out_node=out_node, out_activation="softmax", num.round=round, array.batch.size=200, learning.rate=learning_rate, momentum=0.75, eval.metric=mx.metric.accuracy)
  
  return(model)
}



# 발생시도
# sido(path, 0.01, 17, 100, 400, 4444)
sido <- function(path, learning_rate, out_node, hidden_node, round, seed) {
  
  file <- "Kor_Train_교통사망사고정보(12.1~17.6).csv"

  acc <- read.csv(paste(path, file, sep="/"))
  acc$발생지시군구 <- as.factor(acc$발생지시군구)
  sample <- acc[1:20000, ]
  test <- acc[20001:nrow(acc), ]
  
  train.x <- scale(data.matrix(sample %>% dplyr::select(-발생지시도,c(-1:-5))))
  train.y <- as.numeric(sample$발생지시도)
  test.x <- scale(data.matrix(test %>% dplyr::select(-발생지시도,c(-1:-5))))
  test.y <- as.numeric(test$발생지시도)
  
  mx.set.seed(seed)
  model <- mx.mlp(train.x, train.y, hidden_node=hidden_node, out_node=out_node, activation="relu", out_activation="softmax",
                          num.round=round, array.batch.size=100, learning.rate=learning_rate, momentum=0.9,
                          eval.metric=mx.metric.accuracy, eval.data=list(data = test.x, label = test.y))
  return (model)
}


# 발생시군구
# sigungu(path, 0.01, 209, 1000, 400, 4444)
sigungu <- function(path, learning_rate, out_node, hidden_node, round, seed) {

  file <- "Kor_Train_교통사망사고정보(12.1~17.6).csv"
  
  acc <- read.csv(paste(path, file, sep="/"))
  acc$발생지시군구 <- as.factor(acc$발생지시군구)
  sample <- acc[1:20000, ]
  test <- acc[20001:nrow(acc), ]
  
  train.x <- data.matrix(sample %>% dplyr::select(-발생지시군구,c(-1:-5)))
  train.y <- as.numeric(sample$발생지시군구)
  test.x <- data.matrix(test %>% dplyr::select(-발생지시군구,c(-1:-5)))
  test.y <- as.numeric(test$발생지시군구)
  
  mx.set.seed(seed)
  model <- mx.mlp(train.x, train.y, hidden_node=hidden_node, out_node=out_node, activation="relu", out_activation="softmax",
                             num.round=round, array.batch.size=100, learning.rate=learning_rate, momentum=0.9,
                             eval.metric=mx.metric.accuracy, eval.data=list(data = test.x, label = test.y))

  return (model)  
}

# 도로형태_대분류
# main_road_type(path, 0.01, 9, 1000, 400, 4444)
main_road_type <- function(path, learning_rate, out_node, hidden_node, round, seed) {
  
  file <- "Kor_Train_교통사망사고정보(12.1~17.6).csv"
  
  acc <- read.csv(paste(path, file, sep="/"))
  acc$발생지시군구 <- as.factor(acc$발생지시군구)
  sample <- acc[1:20000, ]
  test <- acc[20001:nrow(acc), ]
  
  train.x <- data.matrix(sample %>% dplyr::select(-도로형태_대분류,c(-1:-5)))
  train.y <- as.numeric(sample$도로형태_대분류)
  test.x <- data.matrix(test %>% dplyr::select(-도로형태_대분류,c(-1:-5)))
  test.y <- as.numeric(test$도로형태_대분류)
  
  mx.set.seed(seed)
  model <- mx.mlp(train.x, train.y, hidden_node=hidden_node, out_node=out_node, activation="relu", out_activation="softmax",
                          num.round=round, array.batch.size=100, learning.rate=learning_rate, momentum=0.9,
                          eval.metric=mx.metric.accuracy, eval.data=list(data = test.x, label = test.y))
  
  return (model)
}

# 도로형태
# detail_road_type(path, 0.01, 16, 1000, 400, 4444)
detail_road_type <- function(path, learning_rate, out_node, hidden_node, round, seed) {
  
  file <- "Kor_Train_교통사망사고정보(12.1~17.6).csv"
  
  acc <- read.csv(paste(path, file, sep="/"))
  acc$발생지시군구 <- as.factor(acc$발생지시군구)
  sample <- acc[1:20000, ]
  test <- acc[20001:nrow(acc), ]
  
  train.x <- data.matrix(sample %>% dplyr::select(-도로형태,c(-1:-5)))
  train.y <- as.numeric(sample$도로형태)
  test.x <- data.matrix(test %>% dplyr::select(-도로형태,c(-1:-5)))
  test.y <- as.numeric(test$도로형태)
  
  mx.set.seed(seed)
  model <- mx.mlp(train.x, train.y, hidden_node=hidden_node, out_node=out_node, activation="relu", out_activation="softmax",
                  num.round=round, array.batch.size=100, learning.rate=learning_rate, momentum=0.9,
                  eval.metric=mx.metric.accuracy, eval.data=list(data = test.x, label = test.y))
  
  return (model)

  #preds = predict(balsido_model, test.x)
  #pred.label = max.col(t(preds))-1
  #table(pred.label, test.y)
  
  #result <- cbind(as.data.frame(pred.label), as.data.frame(test.y))
  #result_len <- nrow(result)
  #result_correct <- nrow(result %>% filter(pred.label == test.y))
  #result_correct/result_len # Accuracy
}
