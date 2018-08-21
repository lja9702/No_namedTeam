#########################################################################################라이브러리설치

cran <- getOption("repos")
cran["dmlc"] <- "https://apache-mxnet.s3-accelerate.dualstack.amazonaws.com/R/CRAN/"
options(repos = cran)
options(repos = c(CRAN = "http://cran.rstudio.com"))
if (!require(devtools)){
  install.packages("devtools")
  require(devtools)
}
if (!require(mlbench)){
  install.packages("mlbench")
  require(mlbench)
}
if (!require(mxnet)){
  install.packages("mxnet")
  require(mxnet)
}
if (!require(readxl)){
  install.packages("readxl")
  require(readxl)
}
if (!require(randomForest)){
  install.packages("randomForest")
  require(randomForest)
}
if (!require(MASS)){
  install.packages("MASS")
  require(MASS)
}
if (!require(dplyr)){
  install.packages("dplyr")
  require(dplyr)
}
rm(cran)

