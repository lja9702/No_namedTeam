install.packages("readxl")
library(readxl)
accident <- read.csv("dataset_kor/교통사망사고정보/Train_교통사망사고정보(12.1~17.6).csv")
#당사자종별_1당과 _2당의 경우 2017년 데이터부터는 
#대분류와 소분류가 같게 적혀있어 모든 데이터를 그꼴로 바꾸었음
dang1_so <- accident$당사자종별_1당_대분류
dang2_so <- accident$당사자종별_2당_대분류
accident$당사자종별_1당 <- dang1_so
accident$당사자종별_2당 <- dang2_so
#""와 "0"을 모두 "없음"으로 치환
temp <- gsub("0", "없음", accident$당사자종별_2당_대분류)
temp[!nzchar(temp)] <- "없음"
accident$당사자종별_2당 <- temp
accident$당사자종별_2당_대분류 <- temp