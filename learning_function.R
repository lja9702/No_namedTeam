
source("./setup_lib.R", encoding="utf-8")

source("./make_x.R", encoding="utf-8")

##########################################################################################함수등록
# 주야
# day_night(path, 0.01, 30, 20, round, 2000)
day_night <- function(path, learning_rate, out_node, hidden_node, round, seed)
{
  file <- "Kor_Train_교통사망사고정보(12.1~17.6).csv"
  accident <- read.csv(paste(path, file, sep=""))
  
  test <- accident[1:5000, ]
  train <- accident[5001:nrow(accident), ]
  
  train.x <- day_night_x(train)
  train.y <- as.numeric(train$주야)
  
  test.x <- day_night_x(test)
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

# 요일
# week(path, 0.01, 30, 20, round, 2000)
week <- function(path, learning_rate, out_node, hidden_node, round, seed)
{
  file <- "Kor_Train_교통사망사고정보(12.1~17.6).csv"
  accident <- read.csv(paste(path, file, sep=""))
  
  test <- accident[1:5000, ]
  train <- accident[5001:nrow(accident), ]
  
  train.x <- week_x(train)
  train.y <- as.numeric(train$요일)
  
  test.x <- week_x(test)
  test.y <- as.numeric(test$요일)
  
  mx.set.seed(seed)
  model <- mx.mlp(train.x, train.y, hidden_node=hidden_node, out_node=out_node, out_activation="softmax",
                  num.round=round, array.batch.size=50, learning.rate=learning_rate, momentum=0.9,
                  eval.metric=mx.metric.accuracy, eval.data = list(data = test.x, label = test.y))
  
  preds = predict(model, test.x)
  pred.label = max.col(t(preds))-1
  table(pred.label, test.y)
  
  return (model)
}

# 법규위반
# violation(path, 0.01, 30, 20, round, 2000)
violation <- function(path, learning_rate, out_node, hidden_node, round, seed)
{
  file <- "Kor_Train_교통사망사고정보(12.1~17.6).csv"
  accident <- read.csv(paste(path, file, sep=""))
  
  test <- accident[1:5000, ]
  train <- accident[5001:nrow(accident), ]
  
  train.x <- violation_x(train)
  train.y <- as.numeric(train$법규위반)
  
  test.x <- violation_x(test)
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

  sample <- accident %>% filter(사상자수 != 1) # 사상자 수가 1인 경우를 제외한 데이터
  sample_one <- accident %>% filter(사상자수 == 1)
  sample_one <- sample_one[sample(1:nrow(sample_one),2500),]
  sample <- rbind(sample, sample_one)
  
  train.x <- injury_count_x(sample)
  train.y <- sample$사상자수
  
  set.seed(seed)
  mx.set.seed(seed)
  model <- mx.mlp(train.x, train.y, hidden_node=hidden_node, out_node=out_node, out_activation="softmax",
                  num.round=round, array.batch.size=50, learning.rate=learning_rate, momentum=0.9,
                  eval.metric=mx.metric.accuracy)
  return (model)
}

# 사망자수
injury_dead_cnt <- function(path, learning_rate, out_node, hidden_node, round, seed)
{
  file <- "Kor_Train_교통사망사고정보(12.1~17.6).csv"
  accident <- read.csv(paste(path, file, sep=""))

  train.x <- injury_dead_cnt_x(accident)
  train.y <- accident$사망자수
  
  set.seed(seed)
  mx.set.seed(seed)
  model <- mx.mlp(train.x, train.y, hidden_node=hidden_node, out_node=out_node, out_activation="softmax",
                  num.round=round, array.batch.size=50, learning.rate=learning_rate, momentum=0.9,
                  eval.metric=mx.metric.accuracy)
  return (model)
}

# 중상자수
injury_mid_cnt <- function(path, learning_rate, out_node, hidden_node, round, seed)
{
  file <- "Kor_Train_교통사망사고정보(12.1~17.6).csv"
  accident <- read.csv(paste(path, file, sep=""))
  
  train.x <- injury_mid_cnt_x(accident)
  train.y <- accident$중상자수
  
  set.seed(seed)
  mx.set.seed(seed)
  model <- mx.mlp(train.x, train.y, hidden_node=hidden_node, out_node=out_node, out_activation="softmax",
                  num.round=round, array.batch.size=50, learning.rate=learning_rate, momentum=0.9,
                  eval.metric=mx.metric.accuracy)
  return (model)
}

# 경상자수
injury_weak_cnt <- function(path, learning_rate, out_node, hidden_node, round, seed)
{
  file <- "Kor_Train_교통사망사고정보(12.1~17.6).csv"
  accident <- read.csv(paste(path, file, sep=""))

  
  train.x <- injury_weak_cnt_x(accident)
  train.y <- accident$경상자수
  
  set.seed(seed)
  mx.set.seed(seed)
  model <- mx.mlp(train.x, train.y, hidden_node=hidden_node, out_node=out_node, out_activation="softmax",
                  num.round=round, array.batch.size=50, learning.rate=learning_rate, momentum=0.9,
                  eval.metric=mx.metric.accuracy)
  return (model)
}

# 부상신고자수
injury_call_cnt <- function(path, learning_rate, out_node, hidden_node, round, seed)
{
  file <- "Kor_Train_교통사망사고정보(12.1~17.6).csv"
  accident <- read.csv(paste(path, file, sep=""))

  
  train.x <- injury_call_cnt_x(accident)
  train.y <- accident$부상신고자수
  
  set.seed(seed)
  mx.set.seed(seed)
  model <- mx.mlp(train.x, train.y, hidden_node=hidden_node, out_node=out_node, out_activation="softmax",
                  num.round=round, array.batch.size=50, learning.rate=learning_rate, momentum=0.9,
                  eval.metric=mx.metric.accuracy)
  return (model)
}

# 사고유형_중분류
# accident_type(path="", learning_rate=0.07, out_node=22, hidden_node=10, round=2000, seed=0)
accident_type <- function(path, learning_rate, out_node, hidden_node, round, seed) {
  
  file <- "Kor_Train_교통사망사고정보(12.1~17.6).csv"
  
  accident <- read.csv(paste(path, file, sep=""))
  
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
  
  sagou.temp <- as.data.frame(accident$사고유형_중분류)
  colnames(sagou.temp) <- c("사고유형_중분류")
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
  
  acc <- read.csv(paste(path, file, sep=""))
  acc$발생지시군구 <- as.factor(acc$발생지시군구)
  sample <- acc[1:20000, ]
  test <- acc[20001:nrow(acc), ]
  
  train.x <- sido_x(sample)
  train.y <- as.numeric(sample$발생지시도)
  test.x <- sido_x(test)
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
  
  acc <- read.csv(paste(path, file, sep=""))
  acc$발생지시군구 <- as.factor(acc$발생지시군구)
  sample <- acc[1:20000, ]
  test <- acc[20001:nrow(acc), ]
  
  train.x <- sigungu_x(sample)
  train.y <- as.numeric(sample$발생지시군구)
  test.x <- sigungu_x(test)
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
  
  acc <- read.csv(paste(path, file, sep=""))
  acc$발생지시군구 <- as.factor(acc$발생지시군구)
  sample <- acc[1:20000, ]
  test <- acc[20001:nrow(acc), ]
  
  train.x <- main_road_type_x(sample)
  train.y <- as.numeric(sample$도로형태_대분류)
  test.x <- main_road_type_x(test)
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
  
  acc <- read.csv(paste(path, file, sep=""))
  acc$발생지시군구 <- as.factor(acc$발생지시군구)
  sample <- acc[1:20000, ]
  test <- acc[20001:nrow(acc), ]
  
  train.x <- detail_road_type_x(sample)
  train.y <- as.numeric(sample$도로형태)
  test.x <- detail_road_type_x(test)
  test.y <- as.numeric(test$도로형태)
  
  mx.set.seed(seed)
  model <- mx.mlp(train.x, train.y, hidden_node=hidden_node, out_node=out_node, activation="relu", out_activation="softmax",
                  num.round=round, array.batch.size=100, learning.rate=learning_rate, momentum=0.9,
                  eval.metric=mx.metric.accuracy, eval.data=list(data = test.x, label = test.y))
  
  return (model)
}

# 당사자종별_1당_대분류
attacker <- function(path, learning_rate, out_node, hidden_node, round, seed) {
  
  file <- "Kor_Train_교통사망사고정보(12.1~17.6).csv"
  
  acc <- read.csv(paste(path, file, sep=""))
  acc$발생지시군구 <- as.factor(acc$발생지시군구)
  sample <- acc[1:20000, ]
  test <- acc[20001:nrow(acc), ]
  
  train.x <- attacker_x(sample)
  train.y <- as.numeric(sample$도로형태)
  test.x <- attacker_x(test)
  test.y <- as.numeric(test$도로형태)
  
  mx.set.seed(seed)
  model <- mx.mlp(train.x, train.y, hidden_node=hidden_node, out_node=out_node, activation="relu", out_activation="softmax",
                  num.round=round, array.batch.size=100, learning.rate=learning_rate, momentum=0.9,
                  eval.metric=mx.metric.accuracy, eval.data=list(data = test.x, label = test.y))
  
  return (model)
}

# 당사자종별_2당_대분류
victim <- function(path, learning_rate, out_node, hidden_node, round, seed) {
  
  file <- "Kor_Train_교통사망사고정보(12.1~17.6).csv"
  
  acc <- read.csv(paste(path, file, sep=""))
  acc$발생지시군구 <- as.factor(acc$발생지시군구)
  sample <- acc[1:20000, ]
  test <- acc[20001:nrow(acc), ]
  
  train.x <- victim_x(sample)
  train.y <- as.numeric(sample$도로형태)
  test.x <- victim_x(test)
  test.y <- as.numeric(test$도로형태)
  
  mx.set.seed(seed)
  model <- mx.mlp(train.x, train.y, hidden_node=hidden_node, out_node=out_node, activation="relu", out_activation="softmax",
                  num.round=round, array.batch.size=100, learning.rate=learning_rate, momentum=0.9,
                  eval.metric=mx.metric.accuracy, eval.data=list(data = test.x, label = test.y))
  
  return (model)
}

# 서울 구별 평균 통행속도
speed_subset_data <- function(path) {
  accident <- read.csv(paste(path, '교통사망사고정보/Kor_Train_교통사망사고정보(12.1~17.6).csv', sep=""))
  road <- readxl::read_xlsx(paste(path, '보조데이터/03.서울시 도로 링크별 교통 사고발생 수/서울시 도로링크별 교통사고(2015~2017).xlsx', sep=""))
  speed_path <- paste(path, "보조데이터/01.서울시 차량 통행 속도", sep="")

  # 진기 코드/
  file_csv_01 <- list.files(paste(speed_path, "/", sep=""), pattern="*.csv")
  file_CSV_01 <- list.files(paste(speed_path, "/", sep=""), pattern="*.CSV")
  file_csv_cnt_01 <- length(file_csv_01)
  file_CSV_cnt_01 <- length(file_CSV_01)
  i=1
  for(j in 1:file_csv_cnt_01){
    tmp = paste("tmp",i,sep="")
    assign(tmp, read.csv(paste(speed_path,"/",file_csv_01[j],sep="")))
    i=i+1
  }
  for(j in 1:file_CSV_cnt_01){
    tmp = paste("tmp",i,sep="")
    assign(tmp, read.csv(paste(speed_path,"/",file_CSV_01[j],sep="")))
    i=i+1
  }
  
  tmp25 <- tmp25[,-33]
  tmp26 <- tmp26[,-33]
  tmp27 <- tmp27[,-33]
  tmp29 <- tmp29[,-33]
  tmp35 <- tmp35[,-33]
  speed <- rbind(tmp1,tmp2,tmp3,tmp4,tmp5,tmp6,tmp7,tmp8,tmp9,tmp10,
                 tmp11,tmp12,tmp13,tmp14,tmp15,tmp16,tmp17,tmp18,tmp19,tmp20,
                 tmp21,tmp22,tmp23,tmp24,tmp25,tmp26,tmp27,tmp28,tmp29,tmp30,
                 tmp31,tmp32,tmp33,tmp34,tmp35,tmp36,tmp37,tmp38,tmp39,tmp40,
                 tmp41)
  rm(tmp1,tmp2,tmp3,tmp4,tmp5,tmp6,tmp7,tmp8,tmp9,tmp10,
     tmp11,tmp12,tmp13,tmp14,tmp15,tmp16,tmp17,tmp18,tmp19,tmp20,
     tmp21,tmp22,tmp23,tmp24,tmp25,tmp26,tmp27,tmp28,tmp29,tmp30,
     tmp31,tmp32,tmp33,tmp34,tmp35,tmp36,tmp37,tmp38,tmp39,tmp40,
     tmp41)
  
  rm(file_csv_01)
  rm(file_CSV_01)
  rm(file_csv_cnt_01)
  rm(file_CSV_cnt_01)
  rm(tmp)
  rm(i)
  rm(j)
  # /진기코드
  
  
  # 전처리
  speed <- rename(speed, "링크ID"="링크아이디")
  speed$링크ID <- as.character(speed$링크ID)
  speed$연도 <- substr(speed$일자, 1, 4)
  speed$X01시 <- as.numeric(speed$X01시)
  
  # 결측치처리 (전체의 평균으로 구함)
  for (i in c(1:24))
  {
    colname <- sprintf("X%02d시", i)
    tmp <- speed %>% filter(!is.na(speed[,colname]))
    speed[,colname] <- ifelse(is.na(speed[,colname]), mean(tmp[,colname]), speed[,colname])
  }
  rm(tmp)
  
  # 통행속도
  ss<-speed %>% group_by(링크ID) %>% summarise(X01시=mean(X01시),X02시=mean(X02시),X03시=mean(X03시),X04시=mean(X04시),X05시=mean(X05시),X06시=mean(X06시),
                                             X07시=mean(X07시),X08시=mean(X08시),X09시=mean(X09시),X10시=mean(X10시),X11시=mean(X11시),X12시=mean(X12시),
                                             X13시=mean(X13시),X14시=mean(X14시),X15시=mean(X15시),X16시=mean(X16시),X17시=mean(X17시),X18시=mean(X18시),
                                             X19시=mean(X19시),X20시=mean(X20시),X21시=mean(X21시),X22시=mean(X22시),X23시=mean(X23시),X24시=mean(X24시))
  #View(ss)
  
  r <- road
  r$사고건수 = as.numeric(r$사고건수)
  r$사망자수 = as.numeric(r$사망자수)
  r$중상자수 = as.numeric(r$중상자수)
  r$경상자수 = as.numeric(r$경상자수)
  r$부상신고자수 = as.numeric(r$부상신고자수)
  
  ss_j <- left_join(ss, r, by=c("링크ID"))
  ss_j <- ss_j %>% filter(!is.na(시군구))
  ss_j$평균통행속도 <- (ss_j$X01시+ss_j$X02시+ss_j$X03시+ss_j$X04시+ss_j$X05시+ss_j$X06시+ss_j$X07시+ss_j$X08시+ss_j$X09시+ss_j$X10시+ss_j$X11시+ss_j$X12시+ss_j$X13시+ss_j$X14시+ss_j$X15시+ss_j$X16시+
                    ss_j$X17시+ss_j$X18시+ss_j$X19시+ss_j$X20시+ss_j$X21시+ss_j$X22시+ss_j$X23시+ss_j$X24시)/24
  #View(ss_j)
  
  #View(head(traffic, 20))
  #View(head(speed, 20))
  #View(head(road, 20))
  
  # 시군구 별 통행속도 및 사고건수
  avg_by_sigungu <- ss_j %>% group_by(시군구) %>% summarise(평균통행속도=mean(평균통행속도),사고건수=mean(사고건수), 사망자수=mean(사망자수), 중상자수=mean(중상자수), 경상자수=mean(경상자수), 부상신고자수=mean(부상신고자수))
  
  #View(avg_by_sigungu)
  
  avg_by_sigungu$사상자수 <- avg_by_sigungu$사망자수+avg_by_sigungu$중상자수+avg_by_sigungu$경상자수+avg_by_sigungu$부상신고자수
  avg_by_sigungu$통행속도대비사상자수 <- avg_by_sigungu$사상자수/avg_by_sigungu$평균통행속도
  
  # 원본 데이터에 붙여보겠음
  avg_speed <- mean(avg_by_sigungu$평균통행속도)
  avg_cnt <- mean(avg_by_sigungu$사고건수)
  avg_hurt <- mean(avg_by_sigungu$사상자수)
  avg_by_sigungu <- rename(avg_by_sigungu, "발생지시군구"="시군구")
  avg_by_sigungu <- rename(avg_by_sigungu, "시군구사상자수"="사상자수")
  avg_by_sigungu <- avg_by_sigungu %>% select(발생지시군구, 평균통행속도, 사고건수, 시군구사상자수)
  
  return (avg_by_sigungu)
  
  #accident <- left_join(accident, avg_by_sigungu, by=c("발생지시군구"))
  # NA는 그냥 평균으로 하자
  #accident$평균통행속도 <- ifelse(is.na(accident$평균통행속도), avg_speed, accident$평균통행속도)
  #accident$사고건수 <- ifelse(is.na(accident$사고건수), avg_cnt, accident$사고건수)
  #accident$시군구사상자수 <- ifelse(is.na(accident$시군구사상자수), avg_hurt, accident$시군구사상자수)
}

learning_all_models <- function(path) {
  
  # 보조데이터 생성
  #speed_subset <- speed_subset_data(path)

  # Make model
  acc_path <- paste(path, "교통사망사고정보/", sep="")
  day_night_model <- day_night(acc_path,learning_rate = 0.01,out_node = 30,hidden_node = 20,round = 1,seed = 2000)
  week_model <- week(acc_path,learning_rate = 0.01,out_node = 30,hidden_node = 20,round = 1,seed = 2000)
  violation_model <- violation(acc_path,learning_rate = 0.01,out_node = 30,hidden_node = 20,round = 1,seed = 2000)
  injury_cnt_model <- injury_cnt(acc_path,learning_rate = 0.001,out_node = 50,hidden_node = 100,round = 1,seed = 4444)
  injury_dead_cnt_model <- injury_dead_cnt(acc_path,learning_rate = 0.001,out_node = 50,hidden_node = 100,round = 1,seed = 4444)
  injury_mid_cnt_model <- injury_mid_cnt(acc_path,learning_rate = 0.001,out_node = 50,hidden_node = 100,round = 1,seed = 4444)
  injury_weak_cnt_model <- injury_weak_cnt(acc_path,learning_rate = 0.001,out_node = 50,hidden_node = 100,round = 1,seed = 4444)
  injury_call_cnt_model <- injury_call_cnt(acc_path,learning_rate = 0.001,out_node = 50,hidden_node = 100,round = 1,seed = 4444)
  accident_type_model <- accident_type(acc_path,learning_rate = 0.07,out_node = 22,hidden_node = 100,round = 1,seed = 4444)
  sido_model <- sido(acc_path,learning_rate = 0.01,out_node = 17,hidden_node = 100,round = 1,seed = 4444)
  sigungu_model <- sigungu(acc_path,learning_rate = 0.01,out_node = 209,hidden_node = 1000,round = 1,seed = 4444)
  main_road_type_model <- main_road_type(acc_path,learning_rate = 0.01,out_node = 9,hidden_node = 1000,round = 1,seed = 4444)
  detail_road_type_model <- detail_road_type(acc_path,learning_rate = 0.01,out_node = 16,hidden_node = 1000,round = 1,seed = 4444)
  attacker_model <- attacker(acc_path,learning_rate = 0.01,out_node = 30,hidden_node = 20,round = 1,seed = 2000)
  victim_model <- victim(acc_path,learning_rate = 0.01,out_node = 30,hidden_node = 20,round = 1,seed = 2000)
  
  # 저장
  mkdir("models")
  saveRDS(day_night_model, "models/day_night_model.rds")
  saveRDS(week_model, "models/week_model.rds")
  saveRDS(violation_model, "models/violation_model.rds")
  saveRDS(injury_cnt_model, "models/injury_cnt_model.rds")
  saveRDS(injury_dead_cnt_model, "models/injury_dead_cnt_model.rds")
  saveRDS(injury_mid_cnt_model, "models/injury_mid_cnt_model.rds")
  saveRDS(injury_weak_cnt_model, "models/injury_weak_cnt_model.rds")
  saveRDS(injury_call_cnt_model, "models/injury_call_cnt_model.rds")
  saveRDS(accident_type_model, "models/accident_type_model.rds")
  saveRDS(sido_model, "models/sido_model.rds")
  saveRDS(sigungu_model, "models/sigungu_model.rds")
  saveRDS(main_road_type_model, "models/main_road_type_model.rds")
  saveRDS(detail_road_type_model, "models/detail_road_type_model.rds")
  saveRDS(attacker_model, "models/attacker_model.rds")
  saveRDS(victim_model, "models/victim_model.rds")
}
