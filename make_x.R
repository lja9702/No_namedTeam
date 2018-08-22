require(dplyr)
# 요기다 x로 바꿔주는 함수들 입력

# 주야
day_night_x <- function(data) {
  return(data.matrix(data %>% select(c(요일, 발생지시도, 사고유형_대분류, 사고유형_중분류, 법규위반, 도로형태_대분류, 도로형태))))
}

#사고유형
accident_type_x <- function(data){
  return(data.matrix(data %>% select(c(사망자수, 중상자수, 경상자수, 부상신고자수, 당사자종별_1당, 당사자종별_2당, 법규위반))))
}

#발생지시도
sido_x <- function(data){
  return(data.matrix(data %>% dplyr::select(c(사망자수, 중상자수, 경상자수, 부상신고자수, 발생지시군구, 도로형태_대분류, 도로형태))))
}

# 발생지시군구
sigungu_x <- function(data){
  return(data.matrix(data %>% dplyr::select(c(사망자수, 중상자수, 경상자수, 부상신고자수, 발생지시도, 도로형태_대분류, 도로형태))))
}

#도로형태 대분류
main_road_type_x <- function(data){
  return(data.matrix(data %>% dplyr::select(c(사망자수, 중상자수, 경상자수, 부상신고자수, 발생지시도, 발생지시군구, 사고유형_대분류, 사고유형_중분류, 법규위반, 도로형태, 당사자종별_1당, 당사자종별_2당))))
}

#도로형태
detail_road_type_x <- function(data){
  return(data.matrix(data %>% dplyr::select(c(주야,사망자수,중상자수,경상자수,부상신고자수,사고유형_대분류, 사고유형_중분류, 법규위반, 도로형태, 당사자종별_1당, 당사자종별_2당))))
}

# 사상자수
injury_count_x <- function(data){
  return(data.matrix(data %>% dplyr::select(c(사망자수, 중상자수, 경상자수, 부상신고자수, 발생지시도, 발생지시군구, 사고유형_대분류, 사고유형_중분류, 법규위반, 도로형태_대분류, 당사자종별_1당, 당사자종별_2당))))
}

# 사망자수
injury_dead_cnt_x <- function(data){
  return(data.matrix(data %>% dplyr::select(c(사상자수, 중상자수, 경상자수, 부상신고자수, 발생지시도, 발생지시군구, 사고유형_대분류, 사고유형_중분류, 법규위반, 도로형태_대분류, 당사자종별_1당, 당사자종별_2당))))
}

# 중상자수
injury_mid_cnt_x <- function(data){
  return(data.matrix(data %>% dplyr::select(c(사망자수, 사상자수, 경상자수, 부상신고자수, 발생지시도, 발생지시군구, 사고유형_대분류, 사고유형_중분류, 법규위반, 도로형태_대분류, 당사자종별_1당, 당사자종별_2당))))
}


# 경상자수
injury_weak_cnt_x <- function(data){
  return(data.matrix(data %>% dplyr::select(c(사망자수, 중상자수, 사상자수, 부상신고자수, 발생지시도, 발생지시군구, 사고유형_대분류, 사고유형_중분류, 법규위반, 도로형태_대분류, 당사자종별_1당, 당사자종별_2당))))
}


# 부상신고자수
injury_call_cnt_x <- function(data){
  return(data.matrix(data %>% dplyr::select(c(사망자수, 중상자수, 경상자수, 사상자수, 발생지시도, 발생지시군구, 사고유형_대분류, 사고유형_중분류, 법규위반, 도로형태_대분류, 당사자종별_1당, 당사자종별_2당))))
}