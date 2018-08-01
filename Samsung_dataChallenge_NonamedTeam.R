install.packages("readxl")
install.library("ggplot2")
install.library("dplyr")

cran <- getOption("repos")
cran["dmlc"] <- "https://apache-mxnet.s3-accelerate.dualstack.amazonaws.com/R/CRAN/"
options(repos = cran)
install.packages('mlbench')
install.packages("mxnet")
rm(cran)
library(readxl)
library(ggplot2)
library(dplyr)
library(mxnet)

accident <- read.csv("dataset_kor/교통사망사고정보/Train_교통사망사고정보(12.1~17.6).csv")
#1당 2당 소분류 필요없어서 제외
accident <- accident[,-21]
accident <- accident[,-22]
#""와 "0"을 모두 "없음"으로 치환
temp <- gsub("0", "없음", accident$당사자종별_2당_대분류)
temp[!nzchar(temp)] <- "없음"
accident$당사자종별_2당_대분류 <- temp

# 발생지시도와 시군구는 인구수도 같이 봐야할 것 같다.
# 지역별 특성보다는 인구수에 비례한 사고가 나는것 같아서
# - 준서

show_chisq_graph <- function(df, pvalue){
  px <- seq(0, 20, length.out = 101)
  plot(px, dchisq(px, df), type = 'l', col = 'blue')

  xa <- qchisq(0.95, df)
  lines(c(xa, xa), c(1, 0), lty = 2)

  xc <- qchisq(1 - pvalue, df)
  lines(c(xc, xc), c(1, 0), col = 'red')
}

# 시간별 평균 사상자 수 그래프
hour <- as.numeric(substr(accident$발생년월일시, 9, 10))

accident_people <- accident$사상자수
test_ha <- as.data.frame(cbind(hour, accident_people))
test_ha_mean <- test_ha %>% group_by(hour) %>% summarise(mean=mean(accident_people))
plot(test_ha_mean)
lines(test_ha_mean$hour, test_ha_mean$mean)


# 요일별 시간당 평균 사상자 수 그래프
test_haw <- accident %>% group_by(요일, 시간=as.numeric(substr(발생년월일시, 9, 10))) %>% summarise(mean=mean(사상자수))
# 모든 요일
ggplot(data = test_haw, aes(x = 시간, y = mean, col = 요일)) + geom_line(size =2)
# 금토일
ggplot(data = test_haw %>% filter(요일=="일" | 요일 == "토"| 요일 == "금"), aes(x = 시간, y = mean, col = 요일)) + geom_line(size =2)


# 사고 유형별 사망자수 합,평균
View(accident %>% group_by(사고유형) %>% summarise(sum = sum(사망자수), mean = mean(사망자수)))
# 사고 유형별 사상자수 박스플롯 그래프
ggplot(accident, aes(accident$사고유형, accident$사상자수)) + geom_boxplot() + ylim(0, 6)
# 사고유형대분류 대비 사망자수 비율과 나머지 비
View(accident %>% group_by(사고유형_대분류) %>% summarise(samang_mean = sum(사망자수) / sum(사상자수) * 100, other_mean = sum(중상자수 + 경상자수) / sum(사상자수) * 100))
# 사고유형중분류 대비 사망자수 비율과 나머지비, 그리고 각각의 발생횟수
View(accident %>% group_by(사고유형_중분류) %>% summarise(samang_mean = sum(사망자수) / sum(사상자수) * 100, other_mean = sum(중상자수 + 경상자수) / sum(사상자수) * 100, count = n()))
# 사고유형중분류 대비 사상자수 각각의 비율과 발생횟수
View(accident %>% group_by(사고유형_중분류) %>% summarise(samang_mean = sum(사망자수) / sum(사상자수) * 100, joong_mean = sum(중상자수) / sum(사상자수) * 100, kyoung_mean = sum(경상자수) / sum(사상자수) * 100, busangsin_mean = sum(부상신고자수) / sum(사상자수) * 100, count = n()))

#진아_ 사고유형별 사망, 사상, 중상, 경상, 부상신고자 수 딥러닝
accident.temp <- cbind(accident$사망자수, accident$사상자수, accident$중상자수, accident$경상자수, accident$부상신고자수, accident$법규위반, accident$도로형태)
colnames(accident.temp) <- c("사망자수", "사상자수", "중상자수", "경상자수", "부상신고자수", "법규위반", "도로형태", "")
accident.temp <- as.data.frame(accident.temp)

sagou.temp <- as.data.frame(accident$사고유형)
colnames(sagou.temp) <- c("사고유형")
accident.scale <- cbind(scale(accident.temp), sagou.temp)
accident.scale[, 8] <- as.numeric(accident.scale[, 8])

acsample <- sample(1:nrow(accident.scale), size = round(0.2 * nrow(accident.scale)))
test.x <- data.matrix(accident.scale[acsample, 1: 7])
test.y <- accident.scale[acsample, 8]
train.x <- data.matrix(accident.scale[-acsample, 1:7])
train.y <- accident.scale[-acsample, 8]

mx.set.seed(0)
model <- mx.mlp(train.x, train.y, hidden_node=10, out_node=2, out_activation="softmax", num.round=20, array.batch.size=15, learning.rate=0.07, momentum=0.9, eval.metric=mx.metric.accuracy)

