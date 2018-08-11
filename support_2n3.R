## Allocating additional memory before loading rJava
rm(list = ls())
options(java.parameters = "-Xmx8000m")

## Set cran mirror 
cran <- getOption("repos")
cran["dmlc"] <- "https://apache-mxnet.s3-accelerate.dualstack.amazonaws.com/R/CRAN/"
options(repos = cran)

## Install pakages
install.packages("readxl")
install.packages("xlsx")
install.packages("dplyr")

## Load pakages
library(readxl)
library(xlsx)
library(dplyr)

rm(cran)

## Read data
s2_TrafficVolume <- read.csv('No_namedTeam/dataset_kor/보조데이터/02.서울시 교통량/서울시_교통량(15.1~17.6).csv')
s3_LinkAccident <- read.xlsx2('No_namedTeam/dataset_kor/보조데이터/03.서울시 도로 링크별 교통 사고발생 수/서울시 도로링크별 교통사고(2015~2017).xlsx', 1)

## Join
left <- left_join(s3_LinkAccident, s2_TrafficVolume, by = c("도로명" = "지점명"))
left$도로명 <- as.factor(left$도로명)

full <- full_join(s3_LinkAccident, s2_TrafficVolume, by = c("도로명" = "지점명"))
full$도로명 <- as.factor(full$도로명)

inner <- inner_join(s3_LinkAccident, s2_TrafficVolume, by = c("도로명" = "지점명"))
inner$도로명 <- as.factor(inner$도로명)
