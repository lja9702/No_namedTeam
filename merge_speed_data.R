cran <- getOption("repos")
cran["dmlc"] <- "https://apache-mxnet.s3-accelerate.dualstack.amazonaws.com/R/CRAN/"
options(repos = cran)
install.packages('mlbench')
install.packages("mxnet")
install.packages('readxl')
install.packages('randomForest')
install.packages('MASS')
install.packages('dplyr')
require(readxl)
require(randomForest)
require(MASS)
require(dplyr)
require(mlbench)
require(mxnet)
rm(cran)

set.seed(4444)

setwd('C:/Users/Administrator/Downloads/dataset_kor/교통사망사고정보')
accident <- read.csv('Train_교통사망사고정보(12.1~17.6).csv')
#, fileEncoding = 'CP949', encoding = 'UTF-8')

setwd('C:/Users/Administrator/Downloads/dataset_kor/보조데이터/01.서울시 차량 통행 속도/')
# 진기 코드/
file_csv_01 <- list.files(".",pattern="*.csv")
file_CSV_01 <- list.files(".",pattern="*.CSV")
file_csv_cnt_01 <- length(file_csv_01)
file_CSV_cnt_01 <- length(file_CSV_01)
i=1
for(j in 1:file_csv_cnt_01){
  tmp = paste("tmp",i,sep="")
  assign(tmp, read.csv(paste(".","/",file_csv_01[j],sep="")))
  i=i+1
}
for(j in 1:file_CSV_cnt_01){
  tmp = paste("tmp",i,sep="")
  assign(tmp, read.csv(paste(".","/",file_CSV_01[j],sep="")))
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


setwd('C:/Users/Administrator/Downloads/dataset_kor/보조데이터/02.서울시 교통량/')
traffic <- read.csv('서울시_교통량(15.1~17.6).csv')

setwd('C:/Users/Administrator/Downloads/dataset_kor/보조데이터/03.서울시 도로 링크별 교통 사고발생 수/')
road <- readxl::read_xlsx('서울시 도로링크별 교통사고(2015~2017).xlsx')

setwd('C:/Users/Administrator/Downloads/dataset_kor/보조데이터/04.무단횡단사고다발지/')
across <- read.csv('무단횡단사고다발지(2012~2016).csv')

setwd('C:/Users/Administrator/Downloads/dataset_kor/보조데이터/05.보행노인사고다발지/')
old <- read.csv('보행노인사고다발지(2012~2016).csv')

setwd('C:/Users/Administrator/Downloads/dataset_kor/보조데이터/06.보행어린이사고다발지/')
child <- read.csv('보행어린이사고다발지(2012~2016).csv')

setwd('C:/Users/Administrator/Downloads/dataset_kor/보조데이터/07.스쿨존내사고다발지/')
school <- read.csv('스쿨존내어린이사고다발지(2012~2016).csv')

setwd('C:/Users/Administrator/Downloads/dataset_kor/보조데이터/08.자전거사고다발지/')
bicycle <- read.csv('자전거사고다발지(2012~2016).csv')



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
View(ss)

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
View(ss_j)

View(head(traffic, 20))
View(head(speed, 20))
View(head(road, 20))

# 시군구 별 통행속도 및 사고건수
sigungu <- ss_j %>% group_by(시군구) %>% summarise(평균통행속도=mean(평균통행속도),사고건수=mean(사고건수), 사망자수=mean(사망자수), 중상자수=mean(중상자수), 경상자수=mean(경상자수), 부상신고자수=mean(부상신고자수))

View(sigungu)

sigungu$사상자수 <- sigungu$사망자수+sigungu$중상자수+sigungu$경상자수+sigungu$부상신고자수
sigungu$통행속도대비사상자수 <- sigungu$사상자수/sigungu$평균통행속도



# 원본 데이터에 붙여보겠음
avg_speed <- mean(sigungu$평균통행속도)
avg_cnt <- mean(sigungu$사고건수)
avg_hurt <- mean(sigungu$사상자수)

sigungu <- rename(sigungu, "발생지시군구"="시군구")
sigungu <- rename(sigungu, "시군구사상자수"="사상자수")
sigungu <- sigungu %>% select(발생지시군구, 평균통행속도, 사고건수, 시군구사상자수)

accident <- left_join(accident, sigungu, by=c("발생지시군구"))
# NA는 그냥 평균으로 하자
accident$평균통행속도 <- ifelse(is.na(accident$평균통행속도), avg_speed, accident$평균통행속도)
accident$사고건수 <- ifelse(is.na(accident$사고건수), avg_cnt, accident$사고건수)
accident$시군구사상자수 <- ifelse(is.na(accident$시군구사상자수), avg_hurt, accident$시군구사상자수)



## 발생지시도 모델
acc <- accident
acc$발생지시군구 <- as.factor(acc$발생지시군구)
sample <- acc[1:20000, ]
test <- acc[20001:nrow(acc), ]

train.발생지시도.x <- scale(data.matrix(sample %>% dplyr::select(-발생지시도,c(-1:-5))))
train.발생지시도.y <- as.numeric(sample$발생지시도)
test.발생지시도.x <- scale(data.matrix(test %>% dplyr::select(-발생지시도,c(-1:-5))))
test.발생지시도.y <- as.numeric(test$발생지시도)

mx.set.seed(4444)
balsido_model <- mx.mlp(train.발생지시도.x, train.발생지시도.y, hidden_node=100, out_node=17, activation="relu", out_activation="softmax",
                num.round=400, array.batch.size=100, learning.rate=0.01, momentum=0.9,
                eval.metric=mx.metric.accuracy, eval.data=list(data = test.발생지시도.x, label = test.발생지시도.y))

preds = predict(balsido_model, test.발생지시도.x)
pred.label = max.col(t(preds))-1
table(pred.label, test.발생지시도.y)

result <- cbind(as.data.frame(pred.label), as.data.frame(test.발생지시도.y))
result_len <- nrow(result)
result_correct <- nrow(result %>% filter(pred.label == test.발생지시도.y))
result_correct/result_len # Accuracy


## 발생지시군구 모델
acc <- accident
acc$발생지시군구 <- as.factor(acc$발생지시군구)
sample <- acc[1:20000, ]
test <- acc[20001:nrow(acc), ]

train.발생지시군구.x <- data.matrix(sample %>% dplyr::select(-발생지시군구,c(-1:-5)))
train.발생지시군구.y <- as.numeric(sample$발생지시군구)
test.발생지시군구.x <- data.matrix(test %>% dplyr::select(-발생지시군구,c(-1:-5)))
test.발생지시군구.y <- as.numeric(test$발생지시군구)

mx.set.seed(4444)
balsigungu_model <- mx.mlp(train.발생지시군구.x, train.발생지시군구.y, hidden_node=1000, out_node=209, activation="relu", out_activation="softmax",
                    num.round=400, array.batch.size=100, learning.rate=0.01, momentum=0.9,
                    eval.metric=mx.metric.accuracy, eval.data=list(data = test.발생지시군구.x, label = test.발생지시군구.y))

preds = predict(balsigungu_model, test.발생지시군구.x)
pred.label = max.col(t(preds))-1
table(pred.label, test.발생지시군구.y)

result <- cbind(as.data.frame(pred.label), as.data.frame(test.발생지시군구.y))
result_len <- nrow(result)
result_correct <- nrow(result %>% filter(pred.label == test.발생지시군구.y))
result_correct/result_len # Accuracy


## 도로형태_대분류 모델
acc <- accident
acc$발생지시군구 <- as.factor(acc$발생지시군구)
sample <- acc[1:20000, ]
test <- acc[20001:nrow(acc), ]

train.도로형태_대분류.x <- data.matrix(sample %>% dplyr::select(-도로형태_대분류,c(-1:-5)))
train.도로형태_대분류.y <- as.numeric(sample$도로형태_대분류)
test.도로형태_대분류.x <- data.matrix(test %>% dplyr::select(-도로형태_대분류,c(-1:-5)))
test.도로형태_대분류.y <- as.numeric(test$도로형태_대분류)

mx.set.seed(4444)
dorodae_model <- mx.mlp(train.도로형태_대분류.x, train.도로형태_대분류.y, hidden_node=1000, out_node=9, activation="relu", out_activation="softmax",
                           num.round=400, array.batch.size=100, learning.rate=0.01, momentum=0.9,
                           eval.metric=mx.metric.accuracy, eval.data=list(data = test.도로형태_대분류.x, label = test.도로형태_대분류.y))

preds = predict(balsigungu_model, test.도로형태_대분류.x)
pred.label = max.col(t(preds))-1
table(pred.label, test.도로형태_대분류.y)

result <- cbind(as.data.frame(pred.label), as.data.frame(test.도로형태_대분류.y))
result_len <- nrow(result)
result_correct <- nrow(result %>% filter(pred.label == test.도로형태_대분류.y))
result_correct/result_len # Accuracy

