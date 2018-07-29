#install.packages("readxl")
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
#install.library("ggplot2")
library(ggplot2)
#install.library("dplyr")
library(dplyr)

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
