#########################################################################################라이브러리설치
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

add_last_sep_path <- function(path) {
  if(!endsWith(path, "/")) {
    return(paste(path, "/", sep=""))
  } else {
    return(path)
  }
}

#테스트 데이터의 ""값을 NA로 변환. test_path는 test_dataSet의 경로
presetting_testdata <- function(test_path){
  x <- read.csv(test_path)
  for(i in 1:ncol(x)){
    temp <- as.character(x[, i])
    temp[!nzchar(temp)] <- NA
    x[, i] <- as.factor(temp)
  }
  x
}

predict_y <- function(model, testdata) {
  preds = predict(model, testdata)
  pred.label = max.col(t(preds))-1
  #table(pred.label, test.y)
  return(pred.label)
  
  #result <- cbind(as.data.frame(pred.label), as.data.frame(test.y))
  #result_len <- nrow(result)
  #result_correct <- nrow(result %>% filter(pred.label == test.y))
  #result_correct/result_len # Accuracy
}

########################################################################################## 메인함수
# 실행시
# Rscript --vanilla main.R <model directory path> <test_kor csv file> <result_kor csv file>
args = commandArgs(trailingOnly = TRUE)
model_dir_path = add_last_sep_path(args[1])
testdata_file = add_last_sep_path(args[2])
resultdata_file = add_last_sep_path(args[3])

# 불러오기
day_night_model <- readRDS(paste(model_dir_path, "day_night_model.rds", sep=""))
week_model <- readRDS(paste(model_dir_path, "week_model.rds", sep=""))
violation_model <- readRDS(paste(model_dir_path, "violation_model.rds", sep=""))
injury_cnt_model <- readRDS(paste(model_dir_path, "injury_cnt_model.rds", sep=""))
accident_type_model <- readRDS(paste(model_dir_path, "accident_type_model.rds", sep=""))
sido_model <- readRDS(paste(model_dir_path, "sido_model.rds", sep=""))
sigungu_model <- readRDS(paste(model_dir_path, "sigungu_model.rds", sep=""))
main_road_type_model <- readRDS(paste(model_dir_path, "main_road_type_model.rds", sep=""))
detail_road_type_model <- readRDS(paste(model_dir_path, "detail_road_type_model.rds", sep=""))

test_data <- presetting_testdata(testdata_file)
result_data <- read.csv(resultdata_file)

# TODO: input test_kor to models
# predict_y(day_night_model, ~~~)

# TODO: put result data

# TODO: save to result_kor