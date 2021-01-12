setwd("C:\\DATA\\projects\\Intego\\automated data cleansing\\check codes")
#packages
library(data.table)
library(plyr)
library(dplyr)
library(stringr)
library(readxl)
#library(tidyr)
library(vwr) # levenhstein distance
#library(zoo) # date -> quarterly data
#library(tidyverse)
#library(caret)
#library(glmnet)

lab=fread("C:\\DATA\\projects\\Intego\\lab_tests.csv")

Dictionary <- read_excel("Dictionary.xlsx",range=cell_cols(2:10))
Dictionary$conversion<-as.numeric(Dictionary$conversion)
Dictionary$Min_extreme<-as.numeric(Dictionary$Min_extreme)
Dictionary$Max_extreme<-as.numeric(Dictionary$Max_extreme)
Dictionary$Min_Common<-as.numeric(Dictionary$Min_Common)
Dictionary$Max_Common<-as.numeric(Dictionary$Max_Common)

# dataframe for cleaning results
Statistics3<-data.frame()
final3<-data.frame(matrix(ncol=2))
x <- c("Date", "id_patient")
colnames(final3) <- x


# Levenhstein Ration function:
lev.ratiomatch<-function(a,b){
  1-(levenshtein.distance(a,b)[[1]]/max(nchar(a),nchar(b)))
}

labcodes<-Dictionary$labcode[!duplicated(Dictionary$labcode)]
TotalCode=length(labcodes)
# t/m 18 gehad
start_time <- Sys.time()
for(i in 1:TotalCode){
  Database<-data.frame()
  feature<-lab[which(lab$LabMddcCod==labcodes[i]),]
  
  #cleaning stats
  OriginalLength=length(!is.na(feature$LabResult))
  completeness1=sum(is.na(feature$LabResult))/length(feature$LabResult)
  NA_results1<-sum(is.na(feature$LabResult))
  NA_units1<-sum(is.na(feature$LabEenh))
  
  # clean results column
  feature$LabResult<-str_replace_all(feature$LabResult,"^\\,([0-9]+$)","0.\\1")
  feature$LabResult<-str_replace_all(feature$LabResult,"$","")
  feature$LabResult<-str_replace_all(feature$LabResult,"([!@#%^&<>();:?ë])","")
  feature$LabResult<-str_replace_all(feature$LabResult,"[A-Z]","")
  feature$LabResult<-str_replace_all(feature$LabResult,"[a-z]","")
  feature$LabResult<-str_replace_all(feature$LabResult,"([0-9].*).([0-9][0-9][0-9])","\\1\\2")
  feature$LabResult<-str_replace_all(feature$LabResult,"([0-9].*).([0-9][0-9][0-9]),(.*)","\\1\\2.\\3")
  feature$LabResult<-str_replace_all(feature$LabResult,"([0-9].*).([0-9][0-9][0-9]).([0-9][0-9][0-9])","\\1\\2\\3")
  feature$LabResult<-gsub(",",".",feature$LabResult)
  
  feature$LabResult <- as.numeric(feature$LabResult)
  completeness2=sum(is.na(feature$LabResult))/OriginalLength*100
  # Obtain sub-dictionary for the conversion of te lab results
  dict<-Dictionary[which(Dictionary$labcode==labcodes[i]),]
  
  #calculate percentage in common range
  MaxCom=as.numeric(dict$Max_Common[1])
  MinCom=as.numeric(dict$Min_Common[1])
  MaxExt=as.numeric(dict$Max_extreme[1])
  MinExt=as.numeric(dict$Min_extreme[1])
  normal<-sum(feature[!is.na(feature$LabResult),]$LabResult>MinCom&feature[!is.na(feature$LabResult),]$LabResult<MaxCom)/nrow(feature[!is.na(feature$LabResult),])*100
  plausible<-sum(feature[!is.na(feature$LabResult),]$LabResult>MinExt&feature[!is.na(feature$LabResult),]$LabResult<MaxExt)/nrow(feature[!is.na(feature$LabResult),])*100
  
  # transformations to unify all units
  feature$LabEenh<-gsub("/100mL","/dl",feature$LabEenh)
  feature$LabEenh<-gsub("/100 mL","/dl",feature$LabEenh)
  feature$LabEenh<-gsub("liter","L",feature$LabEenh)
  feature$LabEenh<-gsub("dL","dl",feature$LabEenh)
  feature$LabEenh<-gsub("mL","ml",feature$LabEenh)
  feature$LabEenh<-gsub(" ","",feature$LabEenh)
  feature$LabEenh<-gsub("/l","/L",feature$LabEenh) # to not confuse ml and L conversions
  feature$LabEenh<-gsub("micro","u",feature$LabEenh)
  feature$LabEenh<-gsub("mc","u",feature$LabEenh)
  
  
  # obtain list of clean measurement units for this feature
  CorrectUnit=dict$Lab_unit
  OriginalUnit=unique(feature$LabEenh)
  ChangedUnit=rep(NA,length(OriginalUnit))
  
  for (p in 1:length(OriginalUnit)){
    vector<-rep(NA,length(CorrectUnit))
    for (q in 1:length(CorrectUnit)){
      vector[q]<-lev.ratiomatch(OriginalUnit[p],CorrectUnit[q])
    }
    numbers<-(max(vector,na.rm=TRUE)==vector)
    if(max(vector,na.rm=TRUE)<=0.5){
      ChangedUnit[p]=NA
    } else if(length(which(numbers))>1){
      ChangedUnit[p]=CorrectUnit[[which(numbers)[[1]]]]
    } else{
      ChangedUnit[p]=CorrectUnit[[which(numbers)]]
    }
  }  
  feature$LabEenh <- mapvalues(feature$LabEenh, from=OriginalUnit, to=ChangedUnit)
  
  # cleaning stats
  NA_units2<-sum(is.na(feature$LabEenh))
  
  # convert lab results according to medical database
  for(c in 1:nrow(dict)){
    feature$LabResult<-ifelse(feature$LabEenh==dict[[c,3]]&!is.na(feature$LabResult)&!is.na(feature$LabEenh)&!between(feature$LabResult,MinCom,MaxCom),feature$LabResult*dict[[c,4]],feature$LabResult)
  }
  
  # code specific conversions for Hb A1c % to mmol/mol
  if(feature$LabMddcCod=="57183B.B"){
    feature$LabResult<-feature$LabResult-23.5
  }
  completeness3=sum(is.na(feature$LabResult))/OriginalLength*100
  #extreme values
  m<-0
  q<-0
  if(min(feature[!is.na(feature$LabResult), ]$LabResult)<0 & MinCom>=0){
    q<-q+sum(feature[!is.na(feature$LabResult), ]$LabResult<0)
    feature[!is.na(feature$LabResult), ]$LabResult<-ifelse(feature[!is.na(feature$LabResult), ]$LabResult<0,feature[!is.na(feature$LabResult), ]$LabResult*-1,feature[!is.na(feature$LabResult), ]$LabResult)
  }
  if(min(feature[!is.na(feature$LabResult), ]$LabResult)==0 & MinCom>0){
    m<-m+sum(feature[!is.na(feature$LabResult), ]$LabResult==0)
    feature[!is.na(feature$LabResult), ]$LabResult<-ifelse(feature[!is.na(feature$LabResult), ]$LabResult==0,NA,feature[!is.na(feature$LabResult), ]$LabResult)
  }

  n<-0
  while((max(feature$LabResult,na.rm=TRUE)>MaxCom&max(feature$LabResult,na.rm=TRUE)>10*MinCom)|max(feature$LabResult,na.rm=TRUE)>MaxExt){
    n<-n+length(feature[!is.na(feature$LabResult),]$LabResult>MaxCom)
    feature$LabResult<-ifelse((feature$LabResult>MaxCom&feature$LabResult>10*MinCom)|feature$LabResult>MaxExt,feature$LabResult/10,feature$LabResult)
  }
  while((min(feature$LabResult,na.rm = TRUE)<MinCom&min(feature$LabResult,na.rm = TRUE)<MaxCom/10)|min(feature$LabResult,na.rm = TRUE)<MinExt){
    n<-n+length(feature[!is.na(feature$LabResult), ]$LabResult<MinCom)
    feature$LabResult<-ifelse((feature$LabResult<MinCom&feature$LabResult<MaxCom/10)|feature$LabResult<MinExt,feature$LabResult*10,feature$LabResult)
  }
  
  # remove values that are still outside the extreme range
  feature$LabResult<-ifelse(!is.na(feature$LabResult)&feature$LabResult>as.numeric(dict$Max_extreme[1]),NA,feature$LabResult)
  feature$LabResult<-ifelse(!is.na(feature$LabResult)&feature$LabResult<as.numeric(dict$Min_extreme[1]),NA,feature$LabResult)
  
  #NA's after cleaning
  NA_results2<-sum(is.na(feature$LabResult))
  
  #NA's after cleaning
  NA_results3<-sum(is.na(feature$LabResult))
  
  #calculate percentage in common range
  normal2<-sum(feature[!is.na(feature$LabResult),]$LabResult>MinCom&feature[!is.na(feature$LabResult),]$LabResult<MaxCom)/nrow(feature[!is.na(feature$LabResult),])*100
  completeness4=sum(is.na(feature$LabResult))/OriginalLength*100
  TestCom=NA_results3/OriginalLength*100
  #statistics
  #labeenheden<-feature %>% group_by(LabEenh) %>% count() 
  #means<-feature %>% group_by(LabEenh) %>% summarize(means=mean(na.omit(LabResult)),med=median(na.omit(LabResult)),min=min(na.omit(LabResult)),max=max(na.omit(LabResult)))
  
  # collect data-cleaning statistics
  Stats<-data.frame(labcodes[i],"Test"=dict[[1,2]],"percentage_NA_results1"=(NA_results1/nrow(feature))*100,
                    "percentage_NA_units1"=(NA_units1/nrow(feature))*100,
                    "percentage_NA_results2"=(NA_results2/nrow(feature))*100,
                    "percentage_NA_units2"=(NA_units2/nrow(feature))*100,
                    "percentage_NA_results3"=(NA_results3/nrow(feature))*100,
                    normal,normal2,completeness1,completeness2,
                    completeness3, completeness4,TestCom,
                    "nrow"=nrow(feature),m,n,q,plausible)
  Statistics3<-rbind(Statistics3,Stats)
  #colnames(Database)[3]<-dict$name[1]
  #final3<-merge(final3,Database,by=c("Date","id_patient"),all=TRUE)
  print(i)
}
end_time <- Sys.time()
end_time-start_time # 6.35 min
Statistics3

#save(final3,file="final3.RData")
#save(Statistics3,file="Statistics3.Rdata")
