#주데이터 서울시 묶음

library(readxl)
library(ggplot2)
library(dplyr)
library(mxnet)

accident <- read.csv("C:/Users/user/Documents/GitHub/No_namedTeam/Kor_Train_교통사망사고정보(12.1~17.6).csv")
seoulTotalAcc <- read_excel("C:/Users/user/Documents/GitHub/No_namedTeam/서울시 도로링크별 교통사고(2015~2017).xlsx")
SeoulData <- accident %>% filter(발생지시도 == "서울")

seoulTotalAcc$사고건수 <- as.numeric(seoulTotalAcc$사고건수)
seoulTotalAcc$사망자수 <- as.numeric(seoulTotalAcc$사망자수)
seoulTotalAcc$중상자수 <- as.numeric(seoulTotalAcc$중상자수)
seoulTotalAcc$경상자수 <- as.numeric(seoulTotalAcc$경상자수)
seoulTotalAcc$부상신고자수 <- as.numeric(seoulTotalAcc$부상신고자수)
#서울시 도로링크별 교통사고 데이터 추출
seoulGuSumData <- seoulTotalAcc %>% group_by(시군구) %>% summarise(sum사고건수 = sum(사고건수), sum사망자수 = sum(사망자수), sum중상자수 = sum(중상자수), sum경상자수 = sum(경상자수), sum부상신고자수 = sum(부상신고자수))
seoulGuMeanData <- seoulTotalAcc %>% group_by(시군구) %>% summarise(sum사고건수 = sum(사고건수), Mean사망자수 = sum(사망자수) / sum사고건수, Mean중상자수 = sum(중상자수) / sum사고건수, Mean경상자수 = sum(경상자수) / sum사고건수, Mean부상신고자수 = sum(부상신고자수) / sum사고건수)#교통사망사고정보에서 서울시 데이터 사상자수 평균 추출
seoulDataMean <- SeoulData %>% group_by(발생지시군구) %>% summarise(사고건수 = n(), Mean사망자수 = mean(사망자수), Mean중상자수 = mean(중상자수), Mean경상자수 = mean(경상자수), Mean부상신고자수 = mean(부상신고자수))
