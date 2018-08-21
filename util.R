

add_last_sep_path <- function(path) {
  if(!endsWith(path, "/")) {
    return(paste(path, "/", sep=""))
  } else {
    return(path)
  }
}



#테스트 데이터의 ""값을 NA로 변환. test_path는 test_dataSet의 경로
presetting_testdata <- function(test_path){
  x <- read.csv(test_path)
  for(i in 1:ncol(x)){
    temp <- as.character(x[, i])
    temp[!nzchar(temp)] <- NA
    x[, i] <- as.factor(temp)
  }
  x
}

# 예측
predict_y <- function(model, testdata) {
  preds = predict(model, testdata)
  pred.label = max.col(t(preds))-1
  #table(pred.label, test.y)
  return(pred.label)
  
  #result <- cbind(as.data.frame(pred.label), as.data.frame(test.y))
  #result_len <- nrow(result)
  #result_correct <- nrow(result %>% filter(pred.label == test.y))
  #result_correct/result_len # Accuracy
}

