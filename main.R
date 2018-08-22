
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

test_data <- presetting_testdata(TEST_KOR_PATH)
result_data <- read.csv(RESULT_KOR_PATH)

# TODO: input test_kor to models
# predict_y(day_night_model, ~~~)

for(row in 1:nrow(test_data))
{
  for(col in 1:ncol(test_data))
  {
    if(is.na(test_data[row][col]))
    {
      if(col == 1)
      {
        test_data[row][col] <- predict_y(day_night_model, test_data)
      }
      else if(col == 2)
      {
        test_data[row][col] <- predict_y(week_model, test_data)
      }
      else if(col == 3)
      {
        test_data[row][col] <- predict_y(injury_dead_cnt_model, test_data)
      }
      else if(col == 4)
      {
        test_data[row][col] <- predict_y(injury_cnt_model, test_data)
      }
      else if(col == 5)
      {
        test_data[row][col] <- predict_y(injury_mid_cnt_model, test_data)
      }
      else if(col == 6)
      {
        test_data[row][col] <- predict_y(injury_weak_cnt_model, test_data)
      }
      else if(col == 7)
      {
        test_data[row][col] <- predict_y(injury_call_cnt_model, test_data)
      }
      else if(col == 8)
      {
        test_data[row][col] <- predict_y(sido_model, test_data)
      }
      else if(col == 9)
      {
        test_data[row][col] <- predict_y(sigungu_model, test_data)
      }
      else if(col == 10)
      {
        # 중분류 값을 참조해서 구하기
      }
      else if(col == 11)
      {
        test_data[row][col] <- predict_y(accident_type_model, test_data)
      }
      else if(col == 12)
      {기
        test_data[row][col] <- predict_y(violation_model, test_data)
      }
      else if(col == 13)
      {
        test_data[row][col] <- predict_y(main_road_type_model, test_data)
      }
      else if(col == 14)
      {
        test_data[row][col] <- predict_y(detail_road_type_model, test_data)
      }
      else if(col == 15)
      {
        test_data[row][col] <- predict_y(attacker_model, test_data)
      }
      else if(col == 16)
      {
        test_data[row][col] <- predict_y(victim_model, test_data)
      }
    }
  }
}


# TODO: put result data

# TODO: save to result_kor