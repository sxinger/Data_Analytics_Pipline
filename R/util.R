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
    x<-gsub("_$")
  })
}



