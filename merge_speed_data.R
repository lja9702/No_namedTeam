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

speed_subset_data <- function(path) {
  accident <- read.csv(paste(path, 'Train_교통사망사고정보(12.1~17.6).csv', sep="/"))
  road <- readxl::read_xlsx(paste(path, '보조데이터/03.서울시 도로 링크별 교통 사고발생 수/서울시 도로링크별 교통사고(2015~2017).xlsx', sep="/"))
  speed_path <- paste(path, "보조데이터/01.서울시 차량 통행 속도", sep="/")
  # 진기 코드/
  file_csv_01 <- list.files(paste(speed_path, "/", sep=""), pattern="*.csv")
  file_CSV_01 <- list.files(paste(speed_path, "/", sep=""), pattern="*.CSV")
  file_csv_cnt_01 <- length(file_csv_01)
  file_CSV_cnt_01 <- length(file_CSV_01)
  i=1
  for(j in 1:file_csv_cnt_01){
    tmp = paste("tmp",i,sep="")
    assign(tmp, read.csv(paste(speed_path,"/",file_csv_01[j],sep="")))
    i=i+1
  }
  for(j in 1:file_CSV_cnt_01){
    tmp = paste("tmp",i,sep="")
    assign(tmp, read.csv(paste(speed_path,"/",file_CSV_01[j],sep="")))
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
  
  # 통행속도
  ss<-speed %>% group_by(링크ID) %>% summarise(X01시=mean(X01시),X02시=mean(X02시),X03시=mean(X03시),X04시=mean(X04시),X05시=mean(X05시),X06시=mean(X06시),
                                             X07시=mean(X07시),X08시=mean(X08시),X09시=mean(X09시),X10시=mean(X10시),X11시=mean(X11시),X12시=mean(X12시),
                                             X13시=mean(X13시),X14시=mean(X14시),X15시=mean(X15시),X16시=mean(X16시),X17시=mean(X17시),X18시=mean(X18시),
                                             X19시=mean(X19시),X20시=mean(X20시),X21시=mean(X21시),X22시=mean(X22시),X23시=mean(X23시),X24시=mean(X24시))
  #View(ss)
  
  r <- road
  r$사고건수 = as.numeric(r$사고건수)
  r$사망자수 = as.numeric(r$사망자수)
  r$중상자수 = as.numeric(r$중상자수)
  r$경상자수 = as.numeric(r$경상자수)
  r$부상신고자수 = as.numeric(r$부상신고자수)
  
  ss_j <- left_join(ss, r, by=c("링크ID"))
  ss_j <- ss_j %>% filter(!is.na(시군구))
  ss_j$평균통행속도 <- (ss_j$X01시+ss_j$X02시+ss_j$X03시+ss_j$X04시+ss_j$X05시+ss_j$X06시+ss_j$X07시+ss_j$X08시+ss_j$X09시+ss_j$X10시+ss_j$X11시+ss_j$X12시+ss_j$X13시+ss_j$X14시+ss_j$X15시+ss_j$X16시+
                    ss_j$X17시+ss_j$X18시+ss_j$X19시+ss_j$X20시+ss_j$X21시+ss_j$X22시+ss_j$X23시+ss_j$X24시)/24
  #View(ss_j)
  
  #View(head(traffic, 20))
  #View(head(speed, 20))
  #View(head(road, 20))
  
  # 시군구 별 통행속도 및 사고건수
  avg_by_sigungu <- ss_j %>% group_by(시군구) %>% summarise(평균통행속도=mean(평균통행속도),사고건수=mean(사고건수), 사망자수=mean(사망자수), 중상자수=mean(중상자수), 경상자수=mean(경상자수), 부상신고자수=mean(부상신고자수))
  
  #View(avg_by_sigungu)
  
  avg_by_sigungu$사상자수 <- avg_by_sigungu$사망자수+avg_by_sigungu$중상자수+avg_by_sigungu$경상자수+avg_by_sigungu$부상신고자수
  avg_by_sigungu$통행속도대비사상자수 <- avg_by_sigungu$사상자수/avg_by_sigungu$평균통행속도
  
  
  
  # 원본 데이터에 붙여보겠음
  avg_speed <- mean(avg_by_sigungu$평균통행속도)
  avg_cnt <- mean(avg_by_sigungu$사고건수)
  avg_hurt <- mean(avg_by_sigungu$사상자수)
  
  avg_by_sigungu <- rename(avg_by_sigungu, "발생지시군구"="시군구")
  avg_by_sigungu <- rename(avg_by_sigungu, "시군구사상자수"="사상자수")
  avg_by_sigungu <- avg_by_sigungu %>% select(발생지시군구, 평균통행속도, 사고건수, 시군구사상자수)
  
  return (avg_by_sigungu)
  
  #accident <- left_join(accident, avg_by_sigungu, by=c("발생지시군구"))
  # NA는 그냥 평균으로 하자
  #accident$평균통행속도 <- ifelse(is.na(accident$평균통행속도), avg_speed, accident$평균통행속도)
  #accident$사고건수 <- ifelse(is.na(accident$사고건수), avg_cnt, accident$사고건수)
  #accident$시군구사상자수 <- ifelse(is.na(accident$시군구사상자수), avg_hurt, accident$시군구사상자수)
}
