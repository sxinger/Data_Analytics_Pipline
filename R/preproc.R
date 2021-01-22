rm(list=ls())
source("./R/util.R")

#install specific packages
require_libraries(c("tidyverse",
                    "magrittr",
                    "openxlsx"))

#load data
#====Enrollment Data
enroll<-read.xlsx("./data/enroll_roster_210115.xlsx",sheet = 1) %>%
  filter(!is.na(Research.ID)&!is.na(Enrolled)) %>% 
  mutate(Enrolled=as.Date(date_convert(Enrolled),origin="1970-01-01"),
         Withdrawn=as.Date(date_convert(Withdrawn),origin="1970-01-01"),
         System.Installed=tolower(System.Installed)) %>%
  select(Research.ID,Summary.Type,Last.Name,First.Name,Sex,Enrolled,Withdrawn,System.Installed) %>%
  mutate(First.Name=gsub("\\s+.*","",First.Name))

height<-read.xlsx("./data/height_19Jun.xlsx",sheet = 1) %>%
  select(Research.ID,`Height.(inches)`) %>%
  rename("HT_inches"="Height.(inches)") %>%
  filter(!is.na(HT_inches))


#====GaitRITE Data
gaitrite_files<-list.files("./data",pattern="^Gait")

gait<-c()
for(file in gaitrite_files){
  gait %<>%
    bind_rows(read.xlsx(paste0("./data/",file),sheet = 1))
}
#normalize columnames
unnorm_colnm<-colnames(gait)
colnames(gait)<-normalize_name(unnorm_colnm)


#====Grip Data
grip<-read.xlsx("./data/Grip_20Jun.xlsx",sheet=1,startRow = 2)
unnorm_colnm<-colnames(grip)
colnames(grip)<-normalize_name(unnorm_colnm)



#integration


#dedup





