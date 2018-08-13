#경로는 바꾸어 주세요 ♥

#########################################  [01.서울시 차량 통행 속도]합치기   ###########################################################
dir_01 <- c("C:/Users/Park JinGi/Desktop/Samsung Contest/dataset_kor/보조데이터/01.서울시 차량 통행 속도")
file_csv_01 <- list.files(dir_01,pattern="*.csv")
file_CSV_01 <- list.files(dir_01,pattern="*.CSV")
file_csv_cnt_01 <- length(file_csv_01)
file_CSV_cnt_01 <- length(file_CSV_01)
i=1
for(j in 1:file_csv_cnt_01){
  tmp = paste("tmp",i,sep="")
  assign(tmp, read.csv(paste(dir_01,"/",file_csv_01[j],sep="")))
  i=i+1
}
for(j in 1:file_CSV_cnt_01){
  tmp = paste("tmp",i,sep="")
  assign(tmp, read.csv(paste(dir_01,"/",file_CSV_01[j],sep="")))
  i=i+1
}
tmp42=read_xlsx("../보조데이터/01.서울시 차량 통행 속도/2016년 1월 통행속도.xlsx")
tmp25 <- tmp25[,-33]
tmp26 <- tmp26[,-33]
tmp27 <- tmp27[,-33]
tmp29 <- tmp29[,-33]
tmp35 <- tmp35[,-33]
data_01 <- rbind(tmp1,tmp2,tmp3,tmp4,tmp5,tmp6,tmp7,tmp8,tmp9,tmp10,
                 tmp11,tmp12,tmp13,tmp14,tmp15,tmp16,tmp17,tmp18,tmp19,tmp20,
                 tmp21,tmp22,tmp23,tmp24,tmp25,tmp26,tmp27,tmp28,tmp29,tmp30,
                 tmp31,tmp32,tmp33,tmp34,tmp35,tmp36,tmp37,tmp38,tmp39,tmp40,
                 tmp41)
rm(tmp1,tmp2,tmp3,tmp4,tmp5,tmp6,tmp7,tmp8,tmp9,tmp10,
      tmp11,tmp12,tmp13,tmp14,tmp15,tmp16,tmp17,tmp18,tmp19,tmp20,
      tmp21,tmp22,tmp23,tmp24,tmp25,tmp26,tmp27,tmp28,tmp29,tmp30,
      tmp31,tmp32,tmp33,tmp34,tmp35,tmp36,tmp37,tmp38,tmp39,tmp40,
      tmp41)

rm(dir_01)
rm(file_csv_01)
rm(file_CSV_01)
rm(file_csv_cnt_01)
rm(file_CSV_cnt_01)
rm(file_01)
rm(file_cnt_01)
rm(tmp)
rm(i)
rm(j)
