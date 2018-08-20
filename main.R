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

# TODO: input test_kor to models

# TODO: read result_kor and put result data

# TODO: save to result_kor