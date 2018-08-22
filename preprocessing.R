#사상, 사망, 중상, 경상, 부상신고자 전처리 
pre_except_sasang <- function(test_row, accident_file){ #데이터 테스트파일의 한줄이랑 주데이터파일 넘기기
  #사망자수를 러닝시키기위해 나머지(사망자수, 중상자수, 경상자수, 부상신고자수 전처리하는 함수) -> 사용하기 위해서는 사고유형 중분류를 알아야함
  
  if(is.na(test_row$사고유형_중분류) == FALSE){
    
    #사고유형 중분류를 알고 사상자수를 알 경우 -> 비율이용
    if(is.na(test_row$사상자수) == FALSE){
      using_rate <- accident_file %>% group_by(사고유형_중분류) %>% 
        summarise(samang_mean = sum(사망자수) / sum(사상자수), joong_mean = sum(중상자수) / sum(사상자수), 
                  kyoung_mean = sum(경상자수) / sum(사상자수), busangsin_mean = sum(부상신고자수) / sum(사상자수))
      target <- using_rate %>% filter(사고유형_중분류 == factor(test_row$사고유형_중분류, levels = levels(using_rate$사고유형_중분류)) )
      if(is.na(test_row$사망자수))
        test_row$사망자수 <- (target[1, ]$samang_mean * as.numeric(test_row$사상자수))
      if(is.na(test_row$중상자수))
        test_row$중상자수 <- (target[1, ]$joong_mean * as.numeric(test_row$사상자수))
      if(is.na(test_row$경상자수))
        test_row$경상자수 <- (target[1, ]$kyoung_mean * as.numeric(test_row$사상자수))
      if(is.na(test_row$부상신고자수))
        test_row$부상신고자수 <- (target[1, ]$busangsin_mean * as.numeric(test_row$사상자수))
      return(test_row)
    }
    #사고유형_중분류만 알 경우
    else{
      using_mean <- accident_file %>% group_by(사고유형_중분류) %>% 
        summarise(mean_사망자수 = mean(사망자수), mean_사상자수 = mean(사상자수), mean_경상자수 = mean(경상자수), mean_중상자수 = mean(중상자수), mean_부상신고자수 = mean(부상신고자수))

      target <- using_mean %>% filter(사고유형_중분류 == factor(test_row$사고유형_중분류, levels = levels(using_rate$사고유형_중분류)))
      if(is.na(test_row$사망자수))
        test_row$사망자수 <- target[1, ]$mean_사망자수
      if(is.na(test_row$중상자수))
        test_row$중상자수 <- target[1, ]$mean_중상자수
      if(is.na(test_row$경상자수))
        test_row$경상자수 <- target[1, ]$mean_경상자수
      if(is.na(test_row$부상신고자수))
        test_row$부상신고자수 <- target[1, ]$mean_부상신고자수
      test_row$사상자수 <- target$mean_사상자수
      return(test_row)
    }
  } else if(is.na(test_row$도로형태) == FALSE){
    
    #도로형태 알고 사상자수를 알 경우 -> 비율이용
    if(is.na(test_row$사상자수) == FALSE){
      using_rate <- accident_file %>% group_by(도로형태) %>% 
        summarise(samang_mean = sum(사망자수) / sum(사상자수), joong_mean = sum(중상자수) / sum(사상자수), 
                  kyoung_mean = sum(경상자수) / sum(사상자수), busangsin_mean = sum(부상신고자수) / sum(사상자수))
      target <- using_rate %>% filter(도로형태 == factor(test_row$도로형태, levels = levels(using_rate$도로형태)))
      if(is.na(test_row$사망자수))
        test_row$사망자수 <- (target[1, ]$samang_mean * as.numeric(test_row$사상자수))
      if(is.na(test_row$중상자수))
        test_row$중상자수 <- (target[1, ]$joong_mean * as.numeric(test_row$사상자수))
      if(is.na(test_row$경상자수))
        test_row$경상자수 <- (target[1, ]$kyoung_mean * as.numeric(test_row$사상자수))
      if(is.na(test_row$부상신고자수))
        test_row$부상신고자수 <- (target[1, ]$busangsin_mean * as.numeric(test_row$사상자수))
      return(test_row)
    }
    #도로형태만 알 경우
    else{
      using_mean <- accident_file %>% group_by(도로형태) %>% 
        summarise(mean_사망자수 = mean(사망자수), mean_사상자수 = mean(사상자수), mean_경상자수 = mean(경상자수), mean_중상자수 = mean(중상자수), mean_부상신고자수 = mean(부상신고자수))
      target <- using_mean %>% filter(도로형태 == factor(test_row$도로형태, levels = levels(using_rate$도로형태)))
      if(is.na(test_row$사망자수))
        test_row$사망자수 <- target$mean_사망자수
      if(is.na(test_row$중상자수))
        test_row$중상자수 <- target$mean_중상자수
      if(is.na(test_row$경상자수))
        test_row$경상자수 <- target$mean_경상자수
      if(is.na(test_row$부상신고자수))
        test_row$부상신고자수 <- target$mean_부상신고자수
      test_row$사상자수 <- target$mean_사상자수
      return(test_row)
    }
  }
  
  mean <- accident_file %>% 
    summarise(mean_사망자수 = mean(사망자수), mean_사상자수 = mean(사상자수), mean_경상자수 = mean(경상자수), mean_중상자수 = mean(중상자수), mean_부상신고자수 = mean(부상신고자수))
  if(is.na(test_row$사망자수))
    test_row$사망자수 <- mean$mean_사망자수
  if(is.na(test_row$중상자수))
    test_row$중상자수 <- mean$mean_중상자수
  if(is.na(test_row$경상자수))
    test_row$경상자수 <- mean$mean_경상자수
  if(is.na(test_row$부상신고자수))
    test_row$부상신고자수 <- mean$mean_부상신고자수

  return(test_row)
}

#===================================================================================================================
#사고유형_중분류를 알고있을 때 대분류 전처리
pre_input_accidentType_big <- function(file, accident){  #테스트파일 불러오기, 주데이터파일
  type_small_to_big <- accident %>% group_by(사고유형_중분류) %>% distinct(사고유형_대분류)
  
  type_small_to_big$사고유형_중분류 = as.character(type_small_to_big$사고유형_중분류)
  type_small_to_big$사고유형_대분류 = as.character(type_small_to_big$사고유형_대분류)
  file$사고유형_중분류 = as.character(file$사고유형_중분류)
  file$사고유형_대분류 = as.character(file$사고유형_대분류)
  
  for(i in 1:nrow(file)){
    if(is.na(file[i, "사고유형_중분류"]) == FALSE && is.na(file[i, "사고유형_대분류"]) == TRUE && file[i, "사고유형_중분류"] != "기타"){
      temp <- subset(type_small_to_big, 사고유형_중분류 == file[i, "사고유형_중분류"])
      file[i, "사고유형_대분류"] <- temp[1, "사고유형_대분류"]
    }
  }
  file$사고유형_중분류 = as.factor(file$사고유형_중분류)
  file$사고유형_대분류 = as.factor(file$사고유형_대분류)
  
  file
}


#===================================================================================================================
#도로형태를 알고있을 때 대분류 전처리
pre_input_doro_big <- function(file, accident){  #테스트파일 불러오기, 주데이터파일
  type_small_to_big <- accident %>% group_by(도로형태) %>% distinct(도로형태_대분류)
  
  type_small_to_big$도로형태 = as.character(type_small_to_big$도로형태)
  type_small_to_big$도로형태_대분류 = as.character(type_small_to_big$도로형태_대분류)
  file$도로형태 = as.character(file$도로형태)
  file$도로형태_대분류 = as.character(file$도로형태_대분류)

  for(i in 1:nrow(file)){
    if(is.na(file[i, "도로형태"]) == FALSE){
      if(is.na(file[i, "도로형태_대분류"])){
        temp <- subset(type_small_to_big, 도로형태 == file[i, "도로형태"])
        file[i, "도로형태_대분류"] <- temp[1, "도로형태_대분류"]
      }
    } else {
      if(is.na(file[i, "도로형태_대분류"] == FALSE && file[i, "도로형태_대분류"] != "단일로" && file[i, "도로형태_대분류"] != "교차로")) {
        file[i, "도로형태"] <- file[i, "도로형태_대분류"]
      }
    }
  }
  file$도로형태 = as.factor(file$도로형태)
  file$도로형태_대분류 = as.factor(file$도로형태_대분류)
  file
}


#===================================================================================================================
#사고유형_대분류기준 전처리
preprocessing_based_accidentType <- function(file){

  for(i in 1:nrow(file)){
    #사고유형_대분류 "건널목" 경우의수 전처리
    if(!is.na(file[i, "사고유형_대분류"])){
      if(file[i, "사고유형_대분류"] ==  "건널목" && is.na(file[i, "당사자종별_2당_대분류"]) == TRUE)
        file[i, "당사자종별_2당_대분류"] <- "열차"
    
      ###################################################
      #사고유형_대분류가 차량단독이면서 당사자종별_2당대분류에 무슨 값이 있을때
      else if(file[i, "사고유형_대분류"] == "차량단독" && !is.na(file[i, "당사자종별_2당_대분류"]) && file[i, "당사자종별_2당_대분류"] != "없음"){
        if(is.na(file[i, "사고유형_중분류"]))
          file[i, "사고유형_중분류"] <- "주/정차차량 충돌"
    
        if(is.na(file[i, "경상자수"]))
          file[i, "경상자수"] <- 0
        
        if(is.na(file[i, "부상신고자수"]))
          file[i, "부상신고자수"] <- 0
      }
      else if(file[i, "사고유형_대분류"] == "차량단독" && is.na(file[i, "당사자종별_2당_대분류"])) {
        file[i, "당사자종별_2당_대분류"] <- "없음"
      }
    }
    if(is.na(file[i, "법규위반"]))
      file[i, "법규위반"] <- "안전운전 의무 불이행"
  }
  file
}


#===================================================================================================================
#당사자종별기준 전처리
preprocessing_based_dangsaja <- function(file){
  
  for(i in 1:nrow(file)){
    #당사자종별_2당_대분류 "열차" 경우의수 전처리 
    if(!is.na(file[i, "당사자종별_2당_대분류"])){
      if(file[i, "당사자종별_2당_대분류"] ==  "열차" && is.na(file[i, "사고유형_대분류"]) == TRUE)
        file[i, "사고유형_대분류"] = "건널목"
      
      #########################################################################################
      #사고유형_대분류 "차대사람"경우의수 전처리
      if(!is.na(file[i, "사고유형_대분류"]) && file[i, "사고유형_대분류"] ==  "차대사람"){
        
        #당사자종별_2당_대분류가 보행자가 아닐 경우 (핵 특수 예외케이스)
        if(file[i, "당사자종별_2당_대분류"] != "보행자"){
          
          if(is.na(file[i, "사고유형_중분류"]) == TRUE)
            file[i, "사고유형_중분류"] = "기타"
          
          if(is.na(file[i, "법규위반"]) == TRUE)
            file[i, "법규위반"] = "안전운전 의무 불이행"
          
          if(is.na(file[i, "당사자종별_1당_대분류"]) == TRUE)
            file[i, "당사자종별_1당_대분류"] = "승용차"
          
          if(is.na(file[i, "주야"]) == TRUE)
            file[i, "주야"] = "야간"
        }
      ######################################################################
      } else {
        #당사자 종별 2당 대분류가 비어있을 경우
        if(!is.na(file[i, "사고유형_중분류"])&& file[i, "사고유형_중분류"] != "기타"){
          #사고유형_중분류가 기타가 아니라면
          
          if(is.na(file[i, "당사자종별_2당_대분류"]) == TRUE)
            file[i, "당사자종별_2당_대분류"] = "보행자"
          
        }
        else{ #사고유형_중분류가 기타일 경우
          file[i, "당사자종별_2당_대분류"] = "승용차"
          
          if(is.na(file[i, "법규위반"]) == TRUE)
            file[i, "법규위반"] = "안전운전 의무 불이행"
          
          if(is.na(file[i, "당사자종별_1당_대분류"]) == TRUE)
            file[i, "당사자종별_1당_대분류"] = "승용차"
          
          if(is.na(file[i, "주야"]) == TRUE)
            file[i, "주야"] = "야간"
        }
      }
    }
  }
  file
}

