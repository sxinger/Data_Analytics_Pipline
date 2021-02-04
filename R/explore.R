rm(list=ls())
source("./R/util.R")

#install specific packages
require_libraries(c("tidyverse",
                    "magrittr",
                    "broom",
                    "Hmisc"
                    ))

enroll<-readRDS("./data/preproc/enroll.rda")
walkspeed<-readRDS("./data/preproc/walkspeed.rda")


#====Table 1====
var_lst<-c("Sex","HT","Wheelchair","System_Installed")
var_type<-c("cat","num","cat","cat")

tbl1<-univar_analysis_mixed(id=enroll$Research_ID,
                            grp=enroll$Summary_Type,
                            X=enroll[,var_lst],
                            data_type=var_type,
                            pretty=T)



#====Walking Speed====



