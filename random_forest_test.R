install.packages('readxl')
install.packages('randomForest')
install.packages('MASS')
install.packages('dplyr')
require(readxl)
require(randomForest)
require(MASS)
require(dplyr)

set.seed(4444)

setwd('~/Downloads/dataset_kor/êµí†µ?‚¬×ºì‚¬ê³ ì •ë³?/')
accident <- read.csv('Train_êµí†µ?‚¬ë§ì‚¬ê³ ì •ë³?(12.1~17.6).csv',
                     fileEncoding = 'CP949', encoding = 'UTF-8')

dim(accident)

# except ë°œìƒì§€?‹œêµ°êµ¬ (too many levels...[209])
cp_accident <- accident %>% dplyr::select(-¹ß»ıÁö½Ã±º±¸)

# train with 300 rows
train <- sample(1:nrow(cp_accident), 300)

# result(?‚¬?ƒ??ˆ˜)
accident.rf <- randomForest(¹ß»ıÁö½Ã±º±¸ ~ . ,data=cp_accident, subset=train)
accident.rf

# see graph
plot(accident.rf)

#
oob.err=double(13)
test.err=double(13)

#mtry is no of Variables randomly chosen at each split
for(mtry in 1:13) 
{
  rf=randomForest(¹ß»ıÁö½Ã±º±¸ ~ . , data = cp_accident , subset = train,mtry=mtry,ntree=400) 
  oob.err[mtry] = rf$mse[400] #Error of all Trees fitted
  
  pred<-predict(rf,cp_accident[-train,]) #Predictions on Test Set for each Tree
  test.err[mtry]= with(cp_accident[-train,], mean( (¹ß»ıÁö½Ã±º±¸ - pred)^2)) #Mean Squared Test Error
  
  cat(mtry," ") #printing the output to the console
  
}

# show error
test.err
oob.err

# show error graph
matplot(1:mtry , cbind(oob.err,test.err), pch=19 , col=c("red","blue"),type="b",ylab="Mean Squared Error",xlab="Number of Predictors Considered at each Split")
legend("topright",legend=c("Out of Bag Error","Test Error"),pch=19, col=c("red","blue"))
