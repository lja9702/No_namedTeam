
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

td <- train_data
td$주야 <- as.ordered(td$주야)
max(td$주야)
test_data$주야 <- ifelse(is.na(test_data$주야), max(td$주야), test_data$주야)

td$요일 <- as.ordered(td$요일)
max(td$요일)
test_data$주야 <- ifelse(is.na(test_data$요일), max(td$요일), test_data$요일)

td$발생지시도 <- as.ordered(td$발생지시도)
max(td$발생지시도)
test_data$발생지시도 <- ifelse(is.na(test_data$발생지시도), max(td$발생지시도), test_data$발생지시도)

td$발생지시군구 <- as.ordered(td$발생지시군구)
max(td$발생지시군구)
test_data$발생지시군구 <- ifelse(is.na(test_data$발생지시군구), max(td$발생지시군구), test_data$발생지시군구)

td$사고유형_대분류 <- as.ordered(td$사고유형_대분류)
max(td$사고유형_대분류)
test_data$사고유형_대분류 <- ifelse(is.na(test_data$사고유형_대분류), max(td$사고유형_대분류), test_data$사고유형_대분류)

td$사고유형_중분류 <- as.ordered(td$사고유형_중분류)
max(td$사고유형_중분류)
test_data$사고유형_중분류 <- ifelse(is.na(test_data$사고유형_중분류), max(td$사고유형_중분류), test_data$사고유형_중분류)

td$법규위반 <- as.ordered(td$법규위반)
max(td$법규위반)
test_data$법규위반 <- ifelse(is.na(test_data$법규위반), max(td$법규위반), test_data$법규위반)

td$도로형태_대분류 <- as.ordered(td$도로형태_대분류)
max(td$도로형태_대분류)
test_data$도로형태_대분류 <- ifelse(is.na(test_data$도로형태_대분류), max(td$도로형태_대분류), test_data$도로형태_대분류)

td$도로형태 <- as.ordered(td$도로형태)
max(td$도로형태)
test_data$도로형태 <- ifelse(is.na(test_data$도로형태), max(td$도로형태), test_data$도로형태)

td$당사자종별_1당_대분류 <- as.ordered(td$당사자종별_1당_대분류)
max(td$당사자종별_1당_대분류)
test_data$당사자종별_1당_대분류 <- ifelse(is.na(test_data$당사자종별_1당_대분류), max(td$당사자종별_1당_대분류), test_data$당사자종별_1당_대분류)

td$당사자종별_2당_대분류 <- as.ordered(td$당사자종별_2당_대분류)
max(td$당사자종별_2당_대분류)
test_data$당사자종별_2당_대분류 <- ifelse(is.na(test_data$당사자종별_2당_대분류), max(td$당사자종별_2당_대분류), test_data$당사자종별_2당_대분류)
  
# TODO: input test_kor to models
# predict_y(day_night_model, ~~~)
c1 <- predict_y(day_night_model, day_night_x(test_data))
c2 <- predict_y(week_model, test_data)
c3 <- predict_y(injury_dead_cnt_model, test_data)
c4 <- predict_y(injury_cnt_model, test_data)
c5 <- predict_y(injury_mid_cnt_model, test_data)
c6 <- predict_y(injury_weak_cnt_model, test_data)
c7 <- predict_y(injury_call_cnt_model, test_data)
c8 <- predict_y(sido_model, test_data)
c9 <- predict_y(sigungu_model, test_data)
c10 <- predict_y(accident_type_model, test_data) #dummy
c11 <- predict_y(accident_type_model, test_data)
c12 <- predict_y(violation_model, test_data)
c13 <- predict_y(main_road_type_model, test_data)
c14 <- predict_y(detail_road_type_model, test_data)
c15 <- predict_y(attacker_model, test_data)
c16 <- predict_y(victim_model, test_data)

predict_df <- cbind(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16)
predict_df <- as.data.frame(predict_df)

for(row in 1:nrow(test_data))
{
  for(col in 1:ncol(test_data))
  {
    if(is.na(test_data[row,col]))
    {
      if(col == 1)
      {
        test_data[row,col] <- predict_y(day_night_model, test_data)
      }
      else if(col == 2)
      {
        test_data[row,col] <- predict_y(week_model, test_data)
      }
      else if(col == 3)
      {
        test_data[row,col] <- predict_y(injury_dead_cnt_model, test_data)
      }
      else if(col == 4)
      {
        test_data[row,col] <- predict_y(injury_cnt_model, test_data)
      }
      else if(col == 5)
      {
        test_data[row,col] <- predict_y(injury_mid_cnt_model, test_data)
      }
      else if(col == 6)
      {
        test_data[row,col] <- predict_y(injury_weak_cnt_model, test_data)
      }
      else if(col == 7)
      {
        test_data[row,col] <- predict_y(injury_call_cnt_model, test_data)
      }
      else if(col == 8)
      {
        test_data[row,col] <- predict_y(sido_model, test_data)
      }
      else if(col == 9)
      {
        test_data[row,col] <- predict_y(sigungu_model, test_data)
      }
      else if(col == 10)
      {
        # 중분류 값을 참조해서 구하기
      }
      else if(col == 11)
      {
        test_data[row,col] <- predict_y(accident_type_model, test_data)
      }
      else if(col == 12)
      {
        test_data[row,col] <- predict_y(violation_model, test_data)
      }
      else if(col == 13)
      {
        test_data[row,col] <- predict_y(main_road_type_model, test_data)
      }
      else if(col == 14)
      {
        test_data[row,col] <- predict_y(detail_road_type_model, test_data)
      }
      else if(col == 15)
      {
        test_data[row,col] <- predict_y(attacker_model, test_data)
      }
      else if(col == 16)
      {
        test_data[row,col] <- predict_y(victim_model, test_data)
      }
    }
  }
}

pre_input_accidentType_big(test_data, train_data)

# TODO: put result data

read_res_and_input(result_data, test_data, "./")

# TODO: save to result_kor