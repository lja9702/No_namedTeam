-#테스트 데이터의 ""값을 NA로 변환. test_path는 test_dataSet의 경로
  
######################################################preprocessing에 씀 지워야하는 코드!!!
presetting_testdata <- function(test_path){
    x <- read.csv(test_path)
    for(i in 1:ncol(x)){
    temp <- as.character(x[, i])
    temp[!nzchar(temp)] <- NA
    x[, i] <- as.factor(temp)
  }
  x
}


testData <- presetting_testdata("~/GitHub/No_namedTeam/test_kor.csv")   #경로 변경할 것
NaList <- check_and_Save_NA(testData)

#테스트데이터의 col별 번호와 카테고리
#1: 주야 2: 요일 3: 사망자수 4: 사상자수 5: 중상자수  
#6: 경상자수 7: 부상신고자수 8: 발생지시도 9: 발생지시군구 10: 사고유형_대분류
#11: 사고유형_중분류 12: 법규위반 13: 도로형태_대분류 14: 도로형태 15: 당사자종별_1당_대분류 16: 당사자종별_2당_대분류

get_naData_col_row_res <- function(x, y){  #x는 NAList, y는 모델을 돌린 결과값
  for(i in 1:length(x)){
    temp <- strsplit(x[[i]], ",")
    cap_temp <- sapply(as.integer(temp[[1]]), function(s) toupper(letters[s]))
    for(j in 1:length(cap_temp)){
      rownum = i + 1
      col_cap = cap_temp[j]
      col_num = as.integer(temp[[1]][j])
      res = y[i][col_num]
    }
  }
}

#NA데이터 위치를 저장하는 함수 x는 ,기준으로 구분된 NA좌표리스트
check_and_Save_NA <- function(dataSet){  
  x <- list()
  for(i in 1:nrow(dataSet)){
    temp <- c()
    for(j in 1:ncol(dataSet)){
      if(is.na(dataSet[i, j]) == TRUE){
        temp <- append(temp, as.character(j))
      }
    }
    x <- append(x, paste(temp, collapse=","))
  }
  x
}

resultData <- read.csv("~/GitHub/No_namedTeam/result_kor.csv")

read_res_and_input <- function(x, y){ #x가 result_kor데이터셋, y가 testDataSet
  for(i in 1:nrow(x)){
    rownum = x[i][1]
    colnum = match(x[i][2], toupper(letters[1:26]))
    x[i][3] = y[rownum][colnum]
  }
  write.csv("~/GitHub/No_namedTeam/result_kor.csv")
}

get_naData_col_row_res(NaList, testData)


fill_seouldata <- function(testData_Onerow, seoulMeanData){
  if(is.na(testData_Onerow$사망자수) == TRUE){ 
    testData_Onerow$사망자수 <- (seoulMeanData %>% filter(시군구 == testData_Onerow$발생지시군구))$사망자수
  }
  if(is.na(testData_Onerow$사상자수) == TRUE){
    testData_Onerow$사망자수 <- (seoulMeanData %>% filter(시군구 == testData_Onerow$발생지시군구))$사상자수
  }
  if(is.na(testData_Onerow$중상자수) == TRUE){
    testData_Onerow$사망자수 <- (seoulMeanData %>% filter(시군구 == testData_Onerow$발생지시군구))$중상자수
  }
  if(is.na(testData_Onerow$경상자수) == TRUE){
    testData_Onerow$사망자수 <- (seoulMeanData %>% filter(시군구 == testData_Onerow$발생지시군구))$경상자수
  }
  if(is.na(testData_Onerow$부상신고자수) == TRUE){
    testData_Onerow$사망자수 <- (seoulMeanData %>% filter(시군구 == testData_Onerow$발생지시군구))$부상신고자수
  }
}