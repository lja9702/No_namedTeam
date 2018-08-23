#########################################################################################라이브러리설치

if (!require(devtools)){
  options(repos = c(CRAN = "http://cran.rstudio.com"))
  install.packages("devtools")
  require(devtools)
}
if (!require(mlbench)){
  options(repos = c(CRAN = "http://cran.rstudio.com"))
  install.packages("mlbench")
  require(mlbench)
}
if (!require(mxnet)){
  cran <- getOption("repos")
  cran["dmlc"] <- "https://apache-mxnet.s3-accelerate.dualstack.amazonaws.com/R/CRAN/"
  options(repos = cran)
  install.packages("mxnet")
  require(mxnet)
}
if (!require(readxl)){
  options(repos = c(CRAN = "http://cran.rstudio.com"))
  install.packages("readxl")
  require(readxl)
}
if (!require(randomForest)){
  options(repos = c(CRAN = "http://cran.rstudio.com"))
  install.packages("randomForest")
  require(randomForest)
}
if (!require(MASS)){
  options(repos = c(CRAN = "http://cran.rstudio.com"))
  install.packages("MASS")
  require(MASS)
}
if (!require(dplyr)){
  options(repos = c(CRAN = "http://cran.rstudio.com"))
  install.packages("dplyr")
  require(dplyr)
}
rm(cran)

