
####################### 소스코드가 있는 경로 입력 ###############################
setwd("C:/Users/Administrator/Documents/GitHub/No_namedTeam/")
#################################################################################

source("./learning_function.R", encoding="utf-8")
source("./util.R", encoding="utf-8")

####################### 여기에 dataset_kor 경로 입력 ############################
DATASET_KOR_PATH = "C:/Users/Administrator/Downloads/dataset_kor"
#################################################################################

DATASET_KOR_PATH <- add_last_sep_path(DATASET_KOR_PATH)

learning_all_models(DATASET_KOR_PATH)

