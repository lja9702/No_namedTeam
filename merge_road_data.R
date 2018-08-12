cran <- getOption("repos")
cran["dmlc"] <- "https://apache-mxnet.s3-accelerate.dualstack.amazonaws.com/R/CRAN/"
options(repos = cran)
install.packages('mlbench')
install.packages("mxnet")
install.packages('readxl')
install.packages('randomForest')
install.packages('MASS')
install.packages('dplyr')
require(readxl)
require(randomForest)
require(MASS)
require(dplyr)
require(mlbench)
require(mxnet)
rm(cran)

set.seed(4444)

setwd('C:/Users/Administrator/Downloads/dataset_kor/교통사망사고정보')
accident <- read.csv('Train_교통사망사고정보(12.1~17.6).csv')
#, fileEncoding = 'CP949', encoding = 'UTF-8')

setwd('C:/Users/Administrator/Downloads/dataset_kor/보조데이터/01.서울시 차량 통행 속도/')
# 진기 코드/
file_csv_01 <- list.files(".",pattern="*.csv")
file_CSV_01 <- list.files(".",pattern="*.CSV")
file_csv_cnt_01 <- length(file_csv_01)
file_CSV_cnt_01 <- length(file_CSV_01)
i=1
for(j in 1:file_csv_cnt_01){
  tmp = paste("tmp",i,sep="")
  assign(tmp, read.csv(paste(".","/",file_csv_01[j],sep="")))
  i=i+1
}
for(j in 1:file_CSV_cnt_01){
  tmp = paste("tmp",i,sep="")
  assign(tmp, read.csv(paste(".","/",file_CSV_01[j],sep="")))
  i=i+1
}

tmp25 <- tmp25[,-33]
tmp26 <- tmp26[,-33]
tmp27 <- tmp27[,-33]
tmp29 <- tmp29[,-33]
tmp35 <- tmp35[,-33]
speed <- rbind(tmp1,tmp2,tmp3,tmp4,tmp5,tmp6,tmp7,tmp8,tmp9,tmp10,
                 tmp11,tmp12,tmp13,tmp14,tmp15,tmp16,tmp17,tmp18,tmp19,tmp20,
                 tmp21,tmp22,tmp23,tmp24,tmp25,tmp26,tmp27,tmp28,tmp29,tmp30,
                 tmp31,tmp32,tmp33,tmp34,tmp35,tmp36,tmp37,tmp38,tmp39,tmp40,
                 tmp41)
rm(tmp1,tmp2,tmp3,tmp4,tmp5,tmp6,tmp7,tmp8,tmp9,tmp10,
   tmp11,tmp12,tmp13,tmp14,tmp15,tmp16,tmp17,tmp18,tmp19,tmp20,
   tmp21,tmp22,tmp23,tmp24,tmp25,tmp26,tmp27,tmp28,tmp29,tmp30,
   tmp31,tmp32,tmp33,tmp34,tmp35,tmp36,tmp37,tmp38,tmp39,tmp40,
   tmp41)

rm(file_csv_01)
rm(file_CSV_01)
rm(file_csv_cnt_01)
rm(file_CSV_cnt_01)
rm(tmp)
rm(i)
rm(j)
# /진기코드


setwd('C:/Users/Administrator/Downloads/dataset_kor/보조데이터/02.서울시 교통량/')
traffic <- read.csv('서울시_교통량(15.1~17.6).csv')

setwd('C:/Users/Administrator/Downloads/dataset_kor/보조데이터/03.서울시 도로 링크별 교통 사고발생 수/')
road <- readxl::read_xlsx('서울시 도로링크별 교통사고(2015~2017).xlsx')

setwd('C:/Users/Administrator/Downloads/dataset_kor/보조데이터/04.무단횡단사고다발지/')
across <- read.csv('무단횡단사고다발지(2012~2016).csv')

setwd('C:/Users/Administrator/Downloads/dataset_kor/보조데이터/05.보행노인사고다발지/')
old <- read.csv('보행노인사고다발지(2012~2016).csv')

setwd('C:/Users/Administrator/Downloads/dataset_kor/보조데이터/06.보행어린이사고다발지/')
child <- read.csv('보행어린이사고다발지(2012~2016).csv')

setwd('C:/Users/Administrator/Downloads/dataset_kor/보조데이터/07.스쿨존내사고다발지/')
school <- read.csv('스쿨존내어린이사고다발지(2012~2016).csv')

setwd('C:/Users/Administrator/Downloads/dataset_kor/보조데이터/08.자전거사고다발지/')
bicycle <- read.csv('자전거사고다발지(2012~2016).csv')



# 전처리
speed <- rename(speed, "링크ID"="링크아이디")
speed$링크ID <- as.character(speed$링크ID)
speed$연도 <- substr(speed$일자, 1, 4)
speed$X01시 <- as.numeric(speed$X01시)

# 결측치처리 (전체의 평균으로 구함)
for (i in c(1:24))
{
  colname <- sprintf("X%02d시", i)
  tmp <- speed %>% filter(!is.na(speed[,colname]))
  speed[,colname] <- ifelse(is.na(speed[,colname]), mean(tmp[,colname]), speed[,colname])
}
rm(tmp)

# 연도 기준 통행속도
ss<-speed %>% group_by(연도, 도로명, 시점명, 종점명, 링크ID, 거리, 방향) %>% summarise(X01시=mean(X01시),X02시=mean(X02시),X03시=mean(X03시),X04시=mean(X04시),X05시=mean(X05시),X06시=mean(X06시),
                                                                  X07시=mean(X07시),X08시=mean(X08시),X09시=mean(X09시),X10시=mean(X10시),X11시=mean(X11시),X12시=mean(X12시),
                                                                  X13시=mean(X13시),X14시=mean(X14시),X15시=mean(X15시),X16시=mean(X16시),X17시=mean(X17시),X18시=mean(X18시),
                                                                  X19시=mean(X19시),X20시=mean(X20시),X21시=mean(X21시),X22시=mean(X22시),X23시=mean(X23시),X24시=mean(X24시))
View(ss)

ss_j <- left_join(ss, road, by=c("링크ID"))
View(ss_j)

View(head(traffic, 20))
View(head(speed, 20))
View(head(road, 20))
