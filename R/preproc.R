rm(list=ls())
source("./R/util.R")

#install specific packages
require_libraries(c("tidyverse",
                    "magrittr",
                    "openxlsx"))

#load data
#====Enrollment Data====
enroll<-read.xlsx("./data/enroll_roster_210129.xlsx",sheet = 1) %>%
  filter(!is.na(Research.ID)&!is.na(Enrolled)) %>% 
  mutate(Enrolled=as.Date(date_convert(Enrolled),origin="1970-01-01"),
         Withdrawn=as.Date(date_convert(Withdrawn),origin="1970-01-01"),
         System.Installed=tolower(System.Installed)) %>%
  select(Research.ID,Summary.Type,Last.Name,First.Name,Sex,Enrolled,Withdrawn,System.Installed) %>%
  mutate(First.Name=gsub("\\s+.*","",First.Name))

#====Height====
height<-read.xlsx("./data/Height_AmulatoryStatus_20Mar.xlsx",sheet = 1) %>%
  rename("HT"="Height.(inches)",
         "Wheelchair"="primarily.uses.wheelchair.(Y/N)",
         "Walking"="Is.walking.(Y/N)",
         "Assistive_Device"="Assistive.Device") %>%
  select(Research.ID,HT,Wheelchair,Walking,Assistive_Device) %>%
  filter(!is.na(HT))

#====Enrollment+Height====
enroll %<>%
  left_join(height,by="Research.ID")

#normalize column names
unnorm_colnm<-colnames(enroll)
colnames(enroll)<-normalize_name(unnorm_colnm)


#====GaitRITE Data====
gaitrite_files<-list.files("./data",pattern="^Gait")

gait<-c()
for(file in gaitrite_files){
  gait %<>%
    bind_rows(read.xlsx(paste0("./data/",file),sheet = 1))
}
#normalize column names
unnorm_colnm<-colnames(gait)
colnames(gait)<-normalize_name(unnorm_colnm)

gait %<>%
  select(-Subject_ID) %>%
  inner_join(enroll %>%
               select(Research_ID,Last_Name,First_Name,Sex,HT,Summary_Type,
                      Wheelchair,Walking,Assistive_Device,Enrolled,Withdrawn),
             by=c("Last_Name","First_Name")) %>%
  arrange(Research_ID,Date_Time_of_Test) %>% unique %>%
  mutate(Mins_of_Day=round((Date_Time_of_Test-floor(Date_Time_of_Test))*24*60),
         Date_Time_of_Test=as.Date(date_convert(Date_Time_of_Test),origin="1970-01-01")) %>%
  gather(var,val,-Research_ID,-Last_Name,-First_Name,-Sex,-HT,-Summary_Type,-Wheelchair,-Walking,-Assistive_Device,
         -Enrolled,-Withdrawn,-Date_Time_of_Test,-Mins_of_Day) %>% unique %>%
  group_by(Research_ID,Last_Name,First_Name,Sex,HT,Summary_Type,Wheelchair,Walking,Assistive_Device,
           Enrolled,Withdrawn,Date_Time_of_Test,var) %>%
  summarise(val=mean(val),.groups="drop") %>% filter(!is.na(val)) %>%
  group_by(Research_ID,Last_Name,First_Name,Sex,HT,Summary_Type,Wheelchair,Walking,Assistive_Device,
           Enrolled,Withdrawn,var) %>%
  arrange(var,Date_Time_of_Test) %>%
  mutate(Day_Since_Last=as.numeric(round(Date_Time_of_Test-dplyr::lag(Date_Time_of_Test,n=1)))) %>%
  ungroup %>% replace_na(list(Day_Since_Last=0)) %>%
  mutate(Day_Since_Enroll=as.numeric(round(Date_Time_of_Test-Enrolled)))


#====Walk Speed====
walkspeed<-read.xlsx("./data/walkingspeed_20Nov.xlsx",sheet=1,startRow = 1) %>%
  rename("Research_ID"="Subject.ID",
         "ws_10ft_trial1"="10ft.walking.speed.(seconds).Trial.1",
         "ws_10ft_trial2"="10ft.walking.speed.(seconds).Trial.2",
         "right_leg_length"="Right.Leg.Length.(cm)",
         "left_leg_length"="Left.Leg.Length.(cm)") %>%
  select(Research_ID,ws_10ft_trial1,ws_10ft_trial2,right_leg_length,left_leg_length,Date) %>%
  filter(!is.na(Date)&grepl("^[0-9]",Date)) %>% 
  mutate(Date=as.Date(date_convert(as.numeric(Date)),origin="1970-01-01"),
         ws_10ft=(ws_10ft_trial1+pmax(ws_10ft_trial2,0,na.rm = T))/(2-is.na(ws_10ft_trial2))) %>%
  group_by(Research_ID) %>% arrange(Date) %>%
  mutate(Day_Since_Last=as.numeric(round(Date-dplyr::lag(Date,n=1)))) %>%
  ungroup %>% replace_na(list(Day_Since_Last=0))

  

#====Grip Data====
grip<-read.xlsx("./data/Grip_20Nov.xlsx",sheet=1,startRow = 2) %>%
  rename("Research_ID"="Subject.ID",
         "grip_right1"="Right.1",
         "grip_right2"="Right.2",
         "grip_left1"="Left.1",
         "grip_left2"="Left.2",
         "Date"="Test.Date") %>%
  select(Research_ID,grip_right1,grip_right2,grip_left1,grip_left2,Date) %>%
  filter(!is.na(Date)&grepl("^[0-9]",Date)) %>% 
  mutate(Date=as.Date(date_convert(as.numeric(Date)),origin="1970-01-01"),
         grip_right=(as.numeric(grip_right1)+pmax(as.numeric(grip_right2),0,na.rm = T))/(2-is.na(as.numeric(grip_right2))),
         grip_left=(as.numeric(grip_left1)+pmax(as.numeric(grip_left2),0,na.rm = T))/(2-is.na(as.numeric(grip_left2)))) %>%
  group_by(Research_ID) %>% arrange(Date) %>%
  mutate(Day_Since_Last=as.numeric(round(Date-dplyr::lag(Date,n=1)))) %>%
  ungroup %>% replace_na(list(Day_Since_Last=0))




#====save data
saveRDS(enroll,file="./data/preproc/enroll.rda")
saveRDS(gait,file="./data/preproc/gaitrite.rda")
saveRDS(walkspeed,file="./data/preproc/walkspeed.rda")
saveRDS(grip,file="./data/preproc/grip.rda")





