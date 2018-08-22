#사상, 사망, 중상, 경상, 부상신고자 전처리 
pre_except_sasang <- function(test_row, accident_file){ #데이터 테스트파일의 한줄이랑 주데이터파일 넘기기
  #사망자수를 러닝시키기위해 나머지(사망자수, 중상자수, 경상자수, 부상신고자수 전처리하는 함수) -> 사용하기 위해서는 사고유형 중분류를 알아야함
  if(is.na(test_row$사고유형_중분류) == FALSE){
    #사고유형 중분류를 알고 사상자수를 알 경우 -> 비율이용
    if(is.na(test_row$사상자수) == FALSE){
      Know_accidentType_and_sasang <- accident_file %>% group_by(사고유형_중분류) %>% 
        summarise(samang_mean = sum(사망자수) / sum(사상자수) * 100, joong_mean = sum(중상자수) / sum(사상자수) * 100, 
                  kyoung_mean = sum(경상자수) / sum(사상자수) * 100, busangsin_mean = sum(부상신고자수) / sum(사상자수) * 100, count = n())
      test_row$사망자수 <- Know_accidentType_and_sasang$samang_mean
      test_row$중상자수 <- Know_accidentType_and_sasang$joong_mean
      test_row$경상자수 <- Know_accidentType_and_sasang$kyoung_mean
      test_row$부상신고자수 <- Know_accidentType_and_sasang$busangsin_mean
    }
    #사고유형_중분류만 알 경우
    else{
      Know_only_accidentType <- accident %>% group_by(사고유형) %>% 
        summarise(mean_사망자수 = mean(사망자수), mean_사상자수 = mean(사상자수), mean_경상자수 = mean(경상자수), mean_중상자수 = mean(중상자수), mean_부상신고자수 = mean(부상신고자수))
      test_row$사망자수 <- Know_only_accidentType$mean_사망자수
      test_row$중상자수 <- Know_only$joong_mean$mean_중상자수
      test_row$경상자수 <- Know_only$mean_경상자수
      test_row$부상신고자수 <- Know_only_accidentType$mean_부상신고자수
      test_row$사상자수 <- Know_only_accidentType$mean_사상자수
    }
  }
  else if(is.na(test_row$) == TRUE){
    
  }
}

#사고유형_중분류를 알고있을 때 대분류 전처리
pre_input_accidentType_big <- function(TEST_KOR_PATH, accident){  #테스트파일 불러오기, 주데이터파일
  file <- presetting_testdata(TEST_KOR_PATH)
  type_small_to_big <- accident %>% group_by(사고유형_중분류) %>% distinct(사고유형_대분류)
  for(i in 1:nrow(file)){
    if(is.na(file[i, "사고유형_중분류"]) == FALSE && is.na(file[i, "사고유형_대분류"]) == TRUE && file[i, "사고유형_중분류"] != "기타"){
      temp <- subset(type_small_to_big, 사고유형_중분류 == file[i, "사고유형_중분류"])
      file[i, "사고유형_대분류"] <- temp[1, "사고유형_대분류"]
    }
  }
}

#당사자종별이나 사고유형 대분류 전처리
preprocessing_dangsaja <- function(TEST_KOR_PATH){
  file <- read.csv(TEST_KOR_PATH)
  for(i in 1:nrow(file)){
    #당사자종별_2당_대분류 "열차" 경우의수 전처리 
    if(file[i, "당사자종별_2당_대분류"] ==  "열차" && is.na(file[i, "사고유형_대분류"]) = TRUE)
      file[i, "사고유형_대분류"] = "건널목"
    
    #사고유형_대분류 "건널목" 경우의수 전처리
    if(file[i, "사고유형_대분류"] ==  "건널목" && is.na(file[i, "당사자종별_2당_대분류"]) = TRUE)
      file[i, "당사자종별_2당_대분류"] = "열차"
    
    #사고유형_대분류 "차대사람"경우의수 전처리
    if(file[i, "사고유형_대분류"] ==  "차대사람"){
      if(file[i, "당사자종별_2당_대분류"] != "보행자"){
        
        if(is.na(file[i, "사고유형_중분류"]) == TRUE)
          file[i, "사고유형_중분류"] = "기타"
        
        if(is.na(file[i, "법규위반"]) == TRUE)
          file[i, "법규위반"] = "안전운전 의무 불이행"
        
        if(is.na(file[i, "당사자종별_1당_대분류"]) == TRUE)
          file[i, "당사자종별_1당_대분류"] = "승용차"
      }
      else if(is.na(file[i, "당사자종별_2당_대분류"]) == TRUE){
        if(file[i, "사고유형_중분류"] != "기타"){
          
        }
        else{
          file[i, "당사자종별_2당_대분류"] = "승용차"
          if(is.na(file[i, "법규위반"]) == TRUE)
            file[i, "법규위반"] = "안전운전 의무 불이행"
          if(is.na(file[i, "당사자종별_1당_대분류"]) == TRUE)
            file[i, "당사자종별_1당_대분류"] = "승용차"
        }
      }
    }
  }
}

#test 데이터셋 비어있는 칸 NA로 채우는코드
presetting_testdata <- function(TEST_KOR_PATH){
  x <- read.csv(TEST_KOR_PATH)
  for(i in 1:ncol(x)){
    temp <- as.character(x[, i])
    temp[!nzchar(temp)] <- NA
    x[, i] <- as.factor(temp)
  }
  x
}
