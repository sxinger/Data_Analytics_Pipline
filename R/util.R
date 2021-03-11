##---------------------------helper functions--------------------------------------##
## install (if needed) and require packages
require_libraries<-function(package_list,verb=T){
  for (lib in package_list) {
    chk_install<-!(lib %in% installed.packages()[,"Package"])
    if(chk_install){
      install.packages(lib)
    }
    library(lib, character.only=TRUE,lib.loc=.libPaths())
    if(verb){
      cat("\n", lib, " loaded.", sep="") 
    }
  }
}

date_convert<-function(x){
  sapply(x,function(x){
    if(grepl("[0-9]{5}",x)){
      as.Date(as.numeric(x),origin = "1899-12-30")
    }else if(!is.na(x)){
      as.Date(x,format="%m/%d/%Y")
    }else{
      x
    }
  })
}

normalize_name<-function(x){
  sapply(x,function(x){
    x<-gsub("[^[:alnum:]]","_",x)
    x<-gsub("_+","_",x)
    x<-gsub("_$","",gsub("^_","",x))
  })
}

##https://github.com/sxinger/utils/blob/master/data_analysis_util.R
univar_analysis_mixed<-function(id,grp,X,data_type,pretty=F, digit=1){
  if(ncol(X)!=length(data_type)){
    stop("data types of X need to be specified")
  }
  
  #TODO: when there is only 1 category
  
  # anova
  df_num<-data.frame(cbind(id,grp,X[,(data_type=="num"),drop=F]),stringsAsFactors=F) %>%
    gather(var,val,-grp,-id) %>%
    mutate(grp=as.factor(grp)) %>%
    mutate(val=as.numeric(val))
  
  out_num<-df_num %>%
    group_by(var,grp) %>%
    dplyr::summarise(n=length(unique(id)),
                     val_miss=sum(is.na(val)),
                     val_mean=mean(val,na.rm=T),
                     val_sd=sd(val,na.rm=T),
                     val_med=median(val,na.rm=T),
                     val_q1=quantile(val,0.25,na.rm=T),
                     val_q3=quantile(val,0.75,na.rm=T),
                     val_min=min(val,na.rm=T),
                     val_max=max(val,na.rm=T),
                     .groups="drop") %>% 
    left_join(df_num %>%
                nest(data=c(id, grp, val)) %>%
                mutate(fit=map(data, ~ aov(val~grp,data=.x)),
                       tidied=map(fit,tidy)) %>%
                unnest(tidied) %>% 
                filter(!is.na(p.value)) %>%
                select(var,p.value),
              by="var") %>%
    mutate(label=paste0(n,"; ",
                        # round(val_miss/n,2),"; ", #missing rate
                        round(val_mean,1),"(",round(val_sd,2),"); ",
                        val_med,"(",val_q1,",",val_q3,")"))
  
  
  # chi-sq
  df_cat<-data.frame(cbind(id,grp,X[,(data_type=="cat")]),stringsAsFactors=F) %>%
    gather(var,val,-grp,-id) %>%
    mutate(grp=as.factor(grp),val=as.factor(val))
  
  out_cat<-df_cat %>%
    group_by(grp) %>%
    dplyr::mutate(tot=length(unique(id))) %>%
    ungroup %>%
    group_by(var) %>%
    dplyr::mutate(val_miss=sum(is.na(val))) %>%
    ungroup %>% filter(!is.na(val)) %>%
    group_by(var,grp,tot,val_miss,val) %>%
    dplyr::summarise(n=length(unique(id)),
                     .groups="drop") %>%
    mutate(prop=round(n/tot,4)) %>%
    left_join(df_cat %>%
                group_by(var) %>%
                dplyr::summarise(p.value=chisq.test(val,grp,simulate.p.value=T)$p.value,
                                 .groups="drop"),
              by="var") %>%
    mutate(label=paste0(n,"; ",
                        # round(val_miss/n,2),"; ", #missing rate
                        "(",prop*100,"%)"))
  
  #output
  if(pretty){
    out<-out_num %>% 
      select(n,grp) %>% unique %>%
      gather(var,val,-grp) %>% 
      mutate(val=as.character(val)) %>% 
      spread(grp,val) %>%
      bind_rows(out_num %>%
                  mutate(label2=paste0(round(val_mean,digit)," (",round(val_sd,digit+1),")"," [",round(val_miss/n,digit+1),"]")) %>%
                  dplyr::select(var,grp,p.value,label2) %>% spread(grp,label2)) %>%
      bind_rows(out_cat %>%
                  unite("var",c("var","val"),sep="=") %>%
                  mutate(label2=paste0(n," (",round(prop*100,digit),"%)"," [",round(val_miss/n,digit+1),"]")) %>%
                  dplyr::select(var,grp,p.value,label2) %>% spread(grp,label2)) %>%
      mutate(p.value=round(p.value,4)) %>%
      separate("var",c("var","cat"),sep="=",extra="merge",fill="right") %>%
      mutate(cat=case_when(var=="n" ~ "",
                           is.na(cat) ~ "mean(sd) [miss]",
                           TRUE ~ paste0(cat,",n(%) [miss]")))
    
  }else{
    out<-list(out_num=out_num,
              out_cat=out_cat)
  }
  
  return(out)
}




