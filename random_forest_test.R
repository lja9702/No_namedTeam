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

set.seed(22)

setwd('C:/Users/Administrator/Downloads/dataset_kor/교통사망사고정보')
accident <- read.csv('Train_교통사망사고정보(12.1~17.6).csv')
                     #, fileEncoding = 'CP949', encoding = 'UTF-8')

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


# DNN with mxnet
# ref: https://mxnet.incubator.apache.org/tutorials/r/fiveMinutesNeuralNetwork.html
# 필요한 컬럼만 취한다.
acc <- accident %>% dplyr::select(
    -도로형태_대분류)
#    -1,-2,-3,-4,-5,
#    -발생지시도, -발생지시군구,-당사자종별_1당_대분류,-당사자종별_1당,
#    -당사자종별_2당_대분류,-당사자종별_2당)

# 비율로 랜덤하게 나눔
n <- nrow(acc)
data <- data.frame(x=runif(n), y=rnorm(n))
ind <- sample(c(TRUE, FALSE), n, replace=TRUE, prob=c(0.70, 0.30))
train <- acc[ind, ]
test <- acc[!ind, ]
#n <- nrow(test)
#data <- data.frame(x=runif(n), y=rnorm(n))
#ind <- sample(c(TRUE, FALSE), n, replace=TRUE, prob=c(0.50, 0.50))
cross <- test
#cross <- test[ind, ]
#test <- test[!ind, ]
# train, cross, test: 70:15:15
# train은 학습용, cross는 비교용, test는 결과 채점용

# 도로형태가 기타 단일로로 너무 편향되어있기에 기타 단일로의 데이터는 4000개 정도만 추출하여 학습시켜본다
sample <- train %>% filter(도로형태 != "기타단일로") # 기타 단일로를 제외한 데이터
sample_road <- train %>% filter(도로형태 == "기타단일로")
sample_road <- sample_road[sample(1:nrow(sample_road),2000),]
sample <- rbind(sample, sample_road)

# 도로형태만 빼고 scale
train.x <- data.matrix(sample %>% dplyr::select(-도로형태))
train.x <- scale(train.x)
train.y <- as.numeric(sample$도로형태)
cross.x <- data.matrix(cross %>% dplyr::select(-도로형태))
cross.x <- scale(cross.x)
cross.y <- as.numeric(cross$도로형태)
test.x <- data.matrix(test %>% dplyr::select(-도로형태))
test.x <- scale(test.x)
test.y <- as.numeric(test$도로형태)

# 학습 model
mx.set.seed(22)
model_road <- mx.mlp(train.x, train.y, hidden_node=15, out_node=16, activation="relu", out_activation="softmax",
                num.round=300, array.batch.size=20, learning.rate=0.001, momentum=0.9, ctx=mx.cpu(),
                eval.metric=mx.metric.accuracy, dropout=0.01, eval.data=list(data=cross.x, label=cross.y))
                #epoch.end.callback=mx.callback.save.checkpoint("model_road")) 회차저장

# 저장
saveRDS(model_road, "model_road.rds")
# 불러오기
model_road <- readRDS("model_road.rds")

# 결과 테이블
preds = predict(model_road, test.x)
pred.label = max.col(t(preds))-1
table(pred.label, test.y)

# 결과 퍼센트
result <- cbind(as.data.frame(pred.label), as.data.frame(test.y))
result_len <- nrow(result)
result_correct <- nrow(result %>% filter(pred.label == test.y))
result_correct/result_len # Accuracy


# 여기부턴 random forest 코드
# except 발생지시군구 (too many levels...[209])
cp_accident <- accident %>% dplyr::select(-발생지시군구)

# train with 300 rows
train <- sample(1:nrow(cp_accident), 300)

# result(발생지시군구)
accident.rf <- randomForest(발생지시군구 ~ . ,data=cp_accident, subset=train)
accident.rf

# see graph
plot(accident.rf)

#
oob.err=double(13)
test.err=double(13)

#mtry is no of Variables randomly chosen at each split
for(mtry in 1:13) 
{
  rf=randomForest(발생지시군구 ~ . , data = cp_accident , subset = train,mtry=mtry,ntree=400) 
  oob.err[mtry] = rf$mse[400] #Error of all Trees fitted
  
  pred<-predict(rf,cp_accident[-train,]) #Predictions on Test Set for each Tree
  test.err[mtry]= with(cp_accident[-train,], mean( (발생지시군구 - pred)^2)) #Mean Squared Test Error
  
  cat(mtry," ") #printing the output to the console
  
}

# show error
test.err
oob.err

# show error graph
matplot(1:mtry , cbind(oob.err,test.err), pch=19 , col=c("red","blue"),type="b",ylab="Mean Squared Error",xlab="Number of Predictors Considered at each Split")
legend("topright",legend=c("Out of Bag Error","Test Error"),pch=19, col=c("red","blue"))
