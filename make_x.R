# 요기다 x로 바꿔주는 함수들 입력

day_night_x <- function(data) {
  return(data.matrix(data %>% select(c(요일, 발생지시도, 사고유형_대분류, 사고유형_중분류, 법규위반, 도로형태_대분류, 도로형태))))
}
