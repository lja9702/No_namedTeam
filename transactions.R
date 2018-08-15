## set PATH
path <- "C:/Users/hj/Documents/dataset_kor"

## for using pakages
library(arules)

## read data
file <- "/교통사망사고정보/Kor_Train_교통사망사고정보(12.1~17.6).csv"
accident <- read.csv(paste(path, file, sep=""), header=T)

df <- split(accident$요일, accident$당사자종별_1당_대분류)
df_tra <- as(df, "transactions")
df_rule <- apriori(df_tra, parameter = list(support=0.5,confidence=0.5))
df_rule_list <- as(df_rule, "data.frame")
df_rule_list
