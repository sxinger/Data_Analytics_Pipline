rm(list=ls())
source("./R/util.R")

#install specific packages
require_libraries(c("tidyverse",
                    "magrittr",
                    "broom",
                    "Hmisc",
                    "lme4",
                    "lmerTest",
                    "kableExtra",
                    "readxl"
                    ))

enroll<-readRDS("./data/preproc/enroll.rda") 
walkspeed<-readRDS("./data/preproc/walkspeed.rda")
gaitrite<-readRDS("./data/preproc/gaitrite.rda")
grip<-readRDS("./data/preproc/grip.rda")

#====Overview
pat_n<-length(unique(enroll$Research_ID))
case_n<-length(unique(enroll$Research_ID[enroll$Summary_Type=="Linguistic"]))
ctrl_n<-length(unique(enroll$Research_ID[enroll$Summary_Type=="Standard"]))

#====Table 1====
enroll %<>%
  left_join(walkspeed %>% group_by(Research_ID) %>%
              arrange(Date) %>% slice(1:1) %>%
              select(Research_ID,ws_10ft),
            by="Research_ID") %>%
  left_join(grip %>% group_by(Research_ID) %>%
              arrange(Date) %>% slice(1:1) %>%
              select(Research_ID,grip_right,grip_left),
            by="Research_ID") %>%
  left_join(gaitrite %>% group_by(Research_ID,var) %>%
              arrange(Date_Time_of_Test) %>% slice(1:1) %>%
              select(Research_ID,var,val) %>%
              spread(var,val),
            by="Research_ID")

var_tbl1<-tibble(var_nm=as.character(),
                 var_type=as.character())
var_tbl1 %<>% 
  add_row(var_nm="Age_at_Enr",var_type="num") %>%
  add_row(var_nm="Sex",var_type="cat") %>%
  add_row(var_nm="HT",var_type="num") %>%
  add_row(var_nm="Wheelchair",var_type="cat") %>%
  add_row(var_nm="System_Installed",var_type="cat") %>%
  add_row(var_nm="Censored",var_type="num") %>%
  add_row(var_nm="Enroll_Days",var_type="num") %>%
  bind_rows(data.frame(var_nm=c("ws_10ft","grip_right","grip_left",unique(gaitrite$var)),
                       var_type=rep("num",85),
                       stringsAsFactors = F))

tbl1<-univar_analysis_mixed(id=enroll$Research_ID,
                            grp=enroll$Summary_Type,
                            X=enroll[,var_tbl1$var_nm],
                            data_type=var_tbl1$var_type,
                            pretty=T)

tbl1 %>%
  kbl(caption="Table 1 - Demographic and Clinical Characteristic Comparison") %>%
  collapse_rows(columns = 5, valign = "top")


#====Walking Speed - Overview
ws_pat_n<-length(unique(walkspeed$Research_ID))
ws2_pat_n<-length(unique(walkspeed$Research_ID[walkspeed$Repeat_Meas_n>1]))
ws2_case_n<-length(unique(walkspeed$Research_ID[walkspeed$Repeat_Meas_n>1&walkspeed$Summary_Type=="Linguistic"]))
ws2_ctrl_n<-length(unique(walkspeed$Research_ID[walkspeed$Repeat_Meas_n>1&walkspeed$Summary_Type=="Standard"]))


#====Walking Speed - Mixed Model ====
ggplot(walkspeed %>%
         filter(Repeat_Meas_n > 1) %>%
         select(Research_ID,ws_10ft,Mth3_Since_Last,Summary_Type),
       aes(x=Mth3_Since_Last,y=ws_10ft,color=Summary_Type)) +
  geom_point(alpha=0.5) +
  geom_smooth(aes(group=Research_ID),
              method="lm",formula = 'y~x',se=FALSE,linetype=1)

fit_ws<-lmer(ws_10ft ~ Mth3_Since_Last*Summary_Type+(1+Mth3_Since_Last|Research_ID),
             data=walkspeed %>% filter(Repeat_Meas_n > 1))
summary(fit_ws)
confint(fit_ws)
anova(fit_ws)


#====Grip - Overview====
grip_pat_n<-length(unique(grip$Research_ID))
grip2_pat_n<-length(unique(grip$Research_ID[grip$Repeat_Meas_n>1]))
grip2_case_n<-length(unique(grip$Research_ID[grip$Repeat_Meas_n>1&grip$Summary_Type=="Linguistic"]))
grip2_ctrl_n<-length(unique(grip$Research_ID[grip$Repeat_Meas_n>1&grip$Summary_Type=="Standard"]))


#====Grip - Mixed Model====
ggplot(grip %>%
         filter(Repeat_Meas_n > 1) %>%
         select(Research_ID,grip_right,grip_left,Mth3_Since_Last) %>%
         inner_join(enroll %>% select(Research_ID,Summary_Type),
                    by="Research_ID") %>%
         gather(grip_side,grip,-Research_ID,-Summary_Type,-Mth3_Since_Last),
       aes(x=Mth3_Since_Last,y=grip,color=Summary_Type)) +
  geom_point(alpha=0.5) +
  geom_smooth(aes(group=Research_ID),method="lm",
              formula = 'y~x',se=FALSE,linetype=1) +
  facet_wrap(~grip_side,ncol=2,scales="free")

fit_gripl<-lmer(grip_left ~ Mth3_Since_Last*Summary_Type+(1|Research_ID),
                data=grip %>% filter(Repeat_Meas_n > 1))
summary(fit_gripl)
anova(fit_gripl)
confint(fit_gripl)


fit_gripr<-lmer(grip_right ~ Mth3_Since_Last*Summary_Type+(1|Research_ID),
                data=grip %>% filter(Repeat_Meas_n > 1))
summary(fit_gripr)
anova(fit_gripr)
confint(fit_gripr)


#====GaitRite - Overview====
gaitrite %<>%
  mutate(var_idx=dense_rank(var))

gait_p<-max(gaitrite$var_idx)
gait_pat_n<-length(unique(gaitrite$Research_ID))
gait2_pat_n<-length(unique(gaitrite$Research_ID[gaitrite$Repeat_Meas_n>1]))
gait2_case_n<-length(unique(gaitrite$Research_ID[gaitrite$Repeat_Meas_n>1&gaitrite$Summary_Type=="Linguistic"]))
gait2_ctrl_n<-length(unique(gaitrite$Research_ID[gaitrite$Repeat_Meas_n>1&gaitrite$Summary_Type=="Standard"]))

#====GaitRite - Mixed Model====
test_df<-c()
sig_plot<-list()
for(i in seq_len(gait_p)){
  #get a cut of the data set 
  data_i<-gaitrite %>% filter(Repeat_Meas_n > 1 & var_idx == i) %>% unique
  var_i<-data_i %>% select(var) %>% slice(1:1) %>% unlist
  
  #build glmm model with random intercept and slope
  fit_gait<-lmer(val ~ Mth3_Since_Last*Summary_Type+(1+Mth3_Since_Last|Research_ID),
                 data=data_i)
  
  #get model summary
  summ<-summary(fit_gait)
  # ci<-confint(fit_gait)
  
  test_df_add<-cbind(
    var_i
    ,round(summ$coefficients[2:4,],4)
    # ,ci[6:8,]
  )
  
  #collect visual for significant-variant factors
  if(min(summ$coefficients[3:4,5])<=0.1){
    test_df_add<-cbind(test_df_add,sig=1)
    
    sig_plot[[var_i]]<-ggplot(data_i,
                              aes(x=Day_Since_Last,y=val,color=Summary_Type)) +
      geom_point(alpha=0.5) +
      geom_smooth(aes(group=Research_ID),
                  method="lm",formula = 'y~x',se=FALSE,
                  linetype=1) +
      facet_wrap(~var,ncol=4,scales="free")
  }else{
    test_df_add<-cbind(test_df_add,sig=0)
  }
  
  test_df<-rbind(test_df,test_df_add)
}
#re-organize the table for better reasability
rownm<-rownames(test_df)
row.names(test_df)<-NULL
test_df2show<-data.frame(
  GaitRite_meas=test_df[,1],
  coef=rownm,
  test_df[,-1]) %>%
  group_by(GaitRite_meas) %>%
  dplyr::mutate(min_pval=min(`Pr...t..`)) %>%
  ungroup %>%
  arrange(min_pval,GaitRite_meas) %>%
  select(-min_pval)
  
test_df2show %>%
  filter(sig==1) %>%
  kbl(caption="Table 2 - GLMM-Model-based screening results of GaitRite metrics") %>%
  collapse_rows(columns = 1, valign = "top")




