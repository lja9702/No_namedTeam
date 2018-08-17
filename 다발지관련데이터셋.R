mudanGo <- read.csv("C:/Users/user/Documents/GitHub/No_namedTeam/04.무단횡단사고다발지/무단횡단사고다발지(2012~2016).csv")
olderGo <- read.csv("C:/Users/user/Documents/GitHub/No_namedTeam/05.보행노인사고다발지/보행노인사고다발지(2012~2016).csv")
childGo <- read.csv("C:/Users/user/Documents/GitHub/No_namedTeam/06.보행어린이사고다발지/보행어린이사고다발지(2012~2016).csv")
schoolZoneGo <- read.csv("C:/Users/user/Documents/GitHub/No_namedTeam/07.스쿨존내사고다발지/스쿨존내어린이사고다발지(2012~2016).csv")
cycleGo <- read.csv("C:/Users/user/Documents/GitHub/No_namedTeam/08.자전거사고다발지/자전거사고다발지(2012~2016).csv")
library(dplyr)
#다발지 데이터 축약 함수
change_dabalJi_Data <- function(x, y){
  x$발생시도 <- NA
  x$발생군구 <- NA
  x$발생년 <- NA
  split다발지명 <- strsplit(as.character(x$다발지명), " ")
  for(i in 1:nrow(x)){
    temp1 <- split다발지명[[i]][1]
    temp2 <- split다발지명[[i]][2]
    발생년 <- substr(x$다발지그룹식별자, 1, 4)
    if(temp1 == "강원도") temp1 = "강원"
    else if(temp1 == "경기도") temp1 = "경기"
    else if(temp1 == "경상남도") temp1 = "경남"
    else if(temp1 == "경상북도") temp1 = "경북"
    else if(temp1 == "광주광역시") temp1 = "광주"
    else if(temp1 == "대구광역시") temp1 = "대구"
    else if(temp1 == "대전광역시") temp1 = "대전"
    else if(temp1 == "부산광역시") temp1 = "부산"
    else if(temp1 == "서울특별시") temp1 = "서울"
    else if(temp1 == "세종특별자치시") temp1 = "세종"
    else if(temp1 == "울산광역시") temp1 = "울산"
    else if(temp1 == "인천광역시") temp1 = "인천"
    else if(temp1 == "전라남도") temp1 = "전남"
    else if(temp1 == "전라북도") temp1 = "전북"
    else if(temp1 == "제주특별자치도") temp1 = "제주"
    else if(temp1 == "충청남도") temp1 = "충남"
    else if(temp1 == "충청북도") temp1 = "충북"
    
    if(temp2 == "세종특별자치시") temp2 = "세종"
    else if(temp2 == "창원시") temp2 = "창원시(통합)"
    x[i, "발생시도"] <- temp1
    x[i, "발생군구"]<- temp2
    x[i, "발생년"] <- 발생년[i]
    x[i, "사고종류"] <- y
  }
  #필요없는행제거
  x <- x[, -c(1, 2, 3, 4, 5, 6, 15)]
  x <- x[, -c(7, 8)]
  x
}

########자전거다발지 관련 데이터########
cycleGo <- change_dabalJi_Data(cycleGo, "자전거사고")

#필요한 데이터들 묶은 자전거 다발지 set
SeparateCycleGo <- cycleGo %>% group_by(발생시도, 발생군구 , 발생년) %>% summarise(sum자전거발생건 = sum(발생건수), sum자전거사망 = sum(사망자수), mean자전거사망 = sum(사망자수) / sum자전거발생건, sum자전거중상 = sum(중상자수), mean자전거중상 = sum(중상자수) / sum자전거발생건, sum자전거경상 = sum(경상자수), mean자전거경상 = sum(경상자수) / sum자전거발생건, sum자전거부상 = sum(부상신고자수), mean자전거부상 = sum(부상신고자수) / sum자전거발생건)


########스쿨존내 사고다발지 관련 데이터########
schoolZoneGo <- change_dabalJi_Data(schoolZoneGo, "스쿨존내사고")

#필요한 데이터들 묶은 스쿨존다발지 set
SeparateSchoolZoneGo <- schoolZoneGo %>% group_by(발생시도, 발생군구 , 발생년) %>% summarise(sum스쿨존발생건 = sum(발생건수), sum스쿨존사망 = sum(사망자수), mean스쿨존사망 = sum(사망자수) / sum스쿨존발생건, sum스쿨존중상 = sum(중상자수), mean스쿨존중상 = sum(중상자수) / sum스쿨존발생건, sum스쿨존경상 = sum(경상자수), mean스쿨존경상 = sum(경상자수) / sum스쿨존발생건, sum자전거부상 = sum(부상신고자수), mean스쿨존부상 = sum(부상신고자수) / sum스쿨존발생건)


########보행어린이 사고다발지 관련 데이터########
childGo <- change_dabalJi_Data(childGo, "보행어린이사고")

#필요한 데이터들 묶은 보행어린이사고다발지 set
SeparateChildGo <- childGo %>% group_by(발생시도, 발생군구 , 발생년) %>% summarise(sum어린이발생건 = sum(발생건수), sum어린이사망 = sum(사망자수), mean어린이사망 = sum(사망자수) / sum어린이발생건, sum어린이중상 = sum(중상자수), mean어린이중상 = sum(중상자수) / sum어린이발생건, sum어린이경상 = sum(경상자수), mean어린이경상 = sum(경상자수) / sum어린이발생건, sum어린이부상 = sum(부상신고자수), mean어린이부상 = sum(부상신고자수) / sum어린이발생건)

########보행노인 사고다발지 관련 데이터########
olderGo <- change_dabalJi_Data(olderGo, "보행노인사고")

#필요한 데이터들 묶은 보행노인 사고다발지 set
SeparateOlderGo <- olderGo %>% group_by(발생시도, 발생군구 , 발생년) %>% summarise(sum노인발생건 = sum(발생건수), sum노인사망 = sum(사망자수), mean노인사망 = sum(사망자수) / sum노인발생건, sum노인중상 = sum(중상자수), mean노인중상 = sum(중상자수) / sum노인발생건, sum노인경상 = sum(경상자수), mean노인경상 = sum(경상자수) / sum노인발생건, sum노인부상 = sum(부상신고자수), mean노인부상 = sum(부상신고자수) / sum노인발생건)

########무단횡단 사고다발지 관련 데이터########
mudanGo <- change_dabalJi_Data(mudanGo, "무단횡단사고")

#필요한 데이터들 묶은 무단횡단 사고다발지 set
SeparateMudanGorGo <- mudanGo %>% group_by(발생시도, 발생군구 , 발생년) %>% summarise(sum무단발생건 = sum(발생건수), sum무단사망 = sum(사망자수), mean무단사망 = sum(사망자수) / sum무단발생건, sum무단중상 = sum(중상자수), mean무단중상 = sum(중상자수) / sum무단발생건, sum무단경상 = sum(경상자수), mean무단경상 = sum(경상자수) / sum무단발생건, sum무단부상 = sum(부상신고자수), mean무단부상 = sum(부상신고자수) / sum무단발생건)

temp <- left_join(SeparateChildGo, SeparateCycleGo)
temp <- left_join(temp, SeparateMudanGorGo)
temp <- left_join(temp, SeparateOlderGo)
temp <- left_join(temp, SeparateSchoolZoneGo)