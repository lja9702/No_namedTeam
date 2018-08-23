
####################### 소스코드가 있는 경로 입력 ###############################
setwd("C:/Users/ehfkd/OneDrive/Documents/No_named/")
#################################################################################

source("./setup_lib.R", encoding="utf-8")
source("./util.R", encoding="utf-8")
source("./about_test_data_function.R", encoding="utf-8")
source("./preprocessing.R", encoding="utf-8")
source("./make_x.R", encoding="utf-8")

####################### 여기에 train 데이터 경로 입력 ############################
TRAIN_PATH = "C:/Users/ehfkd/OneDrive/Documents/dataset_kor/교통사망사고정보/Kor_Train_교통사망사고정보(12.1~17.6).csv"
#################################################################################

####################### 여기에 test_kor 경로 입력 ############################
TEST_KOR_PATH = "C:/Users/ehfkd/OneDrive/Documents/test_kor.csv"
#################################################################################

####################### 여기에 result_kor 경로 입력 ############################
RESULT_KOR_PATH = "C:/Users/ehfkd/OneDrive/Documents/result_kor.csv"
#################################################################################


########################################################################################## 메인함수
model_dir_path = "./models/"

# 불러오기
day_night_model <- readRDS(paste(model_dir_path, "day_night_model.rds", sep=""))                  #주야
week_model <- readRDS(paste(model_dir_path, "week_model.rds", sep=""))                            #요일
violation_model <- readRDS(paste(model_dir_path, "violation_model.rds", sep=""))                  #법규위반
injury_cnt_model <- readRDS(paste(model_dir_path, "injury_cnt_model.rds", sep=""))                #사상자수
injury_dead_cnt_model <- readRDS(paste(model_dir_path, "injury_dead_cnt_model.rds", sep=""))      #사망자수
injury_mid_cnt_model <- readRDS(paste(model_dir_path, "injury_mid_cnt_model.rds", sep=""))        #중상자수
injury_weak_cnt_model <- readRDS(paste(model_dir_path, "injury_weak_cnt_model.rds", sep=""))      #경상자수
injury_call_cnt_model <- readRDS(paste(model_dir_path, "injury_call_cnt_model.rds", sep=""))      #부상신고자수
accident_type_model <- readRDS(paste(model_dir_path, "accident_type_model.rds", sep=""))          #사고유형
sido_model <- readRDS(paste(model_dir_path, "sido_model.rds", sep=""))                            #발생지시도
sigungu_model <- readRDS(paste(model_dir_path, "sigungu_model.rds", sep=""))                      #발생지시군구
main_road_type_model <- readRDS(paste(model_dir_path, "main_road_type_model.rds", sep=""))        #도로형태_대분류
detail_road_type_model <- readRDS(paste(model_dir_path, "detail_road_type_model.rds", sep=""))    #도로형태
attacker_model <- readRDS(paste(model_dir_path, "attacker_model.rds", sep=""))                    #당사자종별_1당_대분류
victim_model <- readRDS(paste(model_dir_path, "victim_model.rds", sep=""))                        #당사자종별_2당_대분류

train_data <- read.csv(TRAIN_PATH)
test_data <- presetting_testdata(TEST_KOR_PATH)
result_data <- read.csv(RESULT_KOR_PATH)

test_data$사망자수 <- as.numeric(test_data$사망자수)
test_data$사상자수 <- as.numeric(test_data$사상자수)
test_data$중상자수 <- as.numeric(test_data$중상자수)
test_data$경상자수 <- as.numeric(test_data$경상자수)
test_data$부상신고자수 <- as.numeric(test_data$부상신고자수)

for(i in 1:nrow(test_data)){
  t <- pre_except_sasang(test_data[i, ], train_data)
  test_data[i, "사망자수"] <- t$사망자수
  test_data[i, "사상자수"] <- t$사상자수
  test_data[i, "중상자수"] <- t$중상자수
  test_data[i, "경상자수"] <- t$경상자수
  test_data[i, "부상신고자수"] <- t$부상신고자수
}

test_data <- pre_input_accidentType_big(test_data, train_data)
test_data <- pre_input_doro_big(test_data, train_data)
test_data <- preprocessing_based_accidentType(test_data)
test_data <- preprocessing_based_dangsaja(test_data)

test_clone <- test_data

td <- train_data
td$주야 <- as.ordered(td$주야)
max(td$주야)
td$주야 <- ifelse(is.na(td$주야), max(td$주야), td$주야)

td$요이 <- as.ordered(일)
max(td$주야)
td$주야 <- ifelse(is.na(td$주야), max(td$주야), td$주야)

td$주야 <- as.ordered(td$주야)
max(td$주야)
td$주야 <- ifelse(is.na(td$주야), max(td$주야), td$주야)

td$주야 <- as.ordered(td$주야)
max(td$주야)
td$주야 <- ifelse(is.na(td$주야), max(td$주야), td$주야)

td$주야 <- as.ordered(td$주야)
max(td$주야)
td$주야 <- ifelse(is.na(td$주야), max(td$주야), td$주야)

td$주야 <- as.ordered(td$주야)
max(td$주야)
td$주야 <- ifelse(is.na(td$주야), max(td$주야), td$주야)

td$주야 <- as.ordered(td$주야)
max(td$주야)
td$주야 <- ifelse(is.na(td$주야), max(td$주야), td$주야)

# 사망자수, 사상자수, 중상자수, 경상자수, 부상신고자수
test_data$사망자수 <- ifelse(is.na(test_data$사망자수), mean(td$사망자수), test_data$사망자수)
test_data$중상자수 <- ifelse(is.na(test_data$중상자수), mean(td$중상자수), test_data$중상자수)
test_data$경상자수 <- ifelse(is.na(test_data$경상자수), mean(td$경상자수), test_data$경상자수)
test_data$부상신고자수 <- ifelse(is.na(test_data$부상신고자수), mean(td$부상신고자수), test_data$부상신고자수)

  
# TODO: input test_kor to models
# predict_y(day_night_model, ~~~)
c1 <- predict_y(day_night_model, day_night_x(test_data))
c2 <- predict_y(week_model, week_x(test_data))
c3 <- predict_y(injury_dead_cnt_model, injury_dead_cnt_x(test_data))
c4 <- predict_y(injury_cnt_model, injury_cnt_x(test_data))
c5 <- predict_y(injury_mid_cnt_model, injury_mid_cnt_x(test_data))
c6 <- predict_y(injury_weak_cnt_model, injury_weak_cnt_x(test_data))
c7 <- predict_y(injury_call_cnt_model, injury_call_cnt_x(test_data))
c8 <- predict_y(sido_model, sido_x(test_data))
c9 <- predict_y(sigungu_model, sigungu_x(test_data))
c10 <- predict_y(accident_type_model, accident_type_x(test_data)) #dummy
c11 <- predict_y(accident_type_model, accident_type_x(test_data))
c12 <- predict_y(violation_model, violation_x(test_data))
c13 <- predict_y(main_road_type_model, main_road_type_x(test_data))
c14 <- predict_y(detail_road_type_model, detail_road_type_x(test_data))
c15 <- predict_y(attacker_model, attacker_x(test_data))
c16 <- predict_y(victim_model, victim_x(test_data))

predict_df <- cbind(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16)
predict_df <- as.data.frame(predict_df)

for(row in 1:nrow(test_clone))
{
  for(col in 1:ncol(test_clone))
  {
    if(is.na(test_clone[row,col]))
    {
      test_clone[row,col] <- predict_df[row, col]
    }
  }
}

test_clone$사고유형_대분류 <- NA
test_clone$도로형태_대분류 <- NA
pre_input_accidentType_big(test_clone, train_data)
pre_input_doro_big(test_clone, train_data)

# TODO: put result data

read_res_and_input(result_data, test_clone, "./")

# TODO: save to result_kor