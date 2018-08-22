
####################### 소스코드가 있는 경로 입력 ###############################
setwd("C:/Users/Administrator/Documents/GitHub/No_namedTeam/")
#################################################################################

source("./setup_lib.R", encoding="utf-8")
source("./util.R", encoding="utf-8")

####################### 여기에 test_kor 경로 입력 ############################
TEST_KOR_PATH = "C:/Users/Administrator/Downloads/dataset_kor/test_kor.csv"
#################################################################################

####################### 여기에 result_kor 경로 입력 ############################
RESULT_KOR_PATH = "C:/Users/Administrator/Downloads/dataset_kor/result_kor.csv"
#################################################################################


########################################################################################## 메인함수
model_dir_path = "./"

# 불러오기
day_night_model <- readRDS(paste(model_dir_path, "day_night_model.rds", sep=""))
week_model <- readRDS(paste(model_dir_path, "week_model.rds", sep=""))
violation_model <- readRDS(paste(model_dir_path, "violation_model.rds", sep=""))
injury_cnt_model <- readRDS(paste(model_dir_path, "injury_cnt_model.rds", sep=""))
injury_dead_cnt_model <- readRDS(paste(model_dir_path, "injury_dead_cnt_model.rds", sep=""))
injury_mid_cnt_model <- readRDS(paste(model_dir_path, "injury_mid_cnt_model.rds", sep=""))
injury_weak_cnt_model <- readRDS(paste(model_dir_path, "injury_weak_cnt_model.rds", sep=""))
injury_call_cnt_model <- readRDS(paste(model_dir_path, "injury_call_cnt_model.rds", sep=""))
accident_type_model <- readRDS(paste(model_dir_path, "accident_type_model.rds", sep=""))
sido_model <- readRDS(paste(model_dir_path, "sido_model.rds", sep=""))
sigungu_model <- readRDS(paste(model_dir_path, "sigungu_model.rds", sep=""))
main_road_type_model <- readRDS(paste(model_dir_path, "main_road_type_model.rds", sep=""))
detail_road_type_model <- readRDS(paste(model_dir_path, "detail_road_type_model.rds", sep=""))

test_data <- presetting_testdata(TEST_KOR_PATH)
result_data <- read.csv(RESULT_KOR_PATH)

# TODO: input test_kor to models
# predict_y(day_night_model, ~~~)

# TODO: put result data

# TODO: save to result_kor