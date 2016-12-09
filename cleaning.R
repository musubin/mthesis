library(dplyr)
# library(sqldf)
library(magrittr)
library(data.table)
# library(foreach)
###

#allfunc <- function(variables){

pref <- function(variables){  
  # library(foreach)
  setwd("C:\\Users\\KT\\Desktop\\panel\\userdata\\csv")
  # setwd("/resources/data/panel/csv")
  files <- list.files(full.names = F, pattern="csv", recursive = FALSE)
  # all.temp <- foreach(i=2:4) %dopar% {}  #%dopar%
  reading <- function(x){
    # library(data.table)
    temp<- fread(x) %>% dplyr::select(which(names(fread(x)) %in% variables))%>% 
      mutate_each(funs(as.numeric), which(sapply(., is.character))) %>%
      dplyr::filter(Q11 == 2|Q16 == 2|Q21 == 2|Q26 == 2|Q31==2)%>% 
      dplyr::mutate(num2=2) %>%
      dplyr::mutate(year2 = as.numeric(ifelse(Q11==2, Q13, ifelse(Q16==2, Q18, ifelse(Q21==2, Q23,
      ifelse(Q26==2,Q28,ifelse(Q31==2,Q33,NA))))))) %>% dplyr::filter(year2!=99) %>% 
      dplyr::filter(Q11 == 3|Q16 == 3|Q21 == 3|Q26 == 3|Q31==3)%>% 
      dplyr::mutate(num3=3) %>%
      dplyr::mutate(year3 = as.numeric(ifelse(Q11==3, Q13, ifelse(Q16==3, Q18, ifelse(Q21==3, Q23,
      ifelse(Q26==3,Q28,ifelse(Q31==3,Q33,NA))))))) %>% dplyr::filter(year3!=99) %>% 
      dplyr::filter(Q11 == 4|Q16 == 4|Q21 == 4|Q26 == 4|Q31==4)%>% 
      dplyr::mutate(num4=4) %>%
      dplyr::mutate(year4 = as.numeric(ifelse(Q11==4, Q13, ifelse(Q16==4, Q18, ifelse(Q21==4, Q23,
      ifelse(Q26==4,Q28,ifelse(Q31==4,Q33,NA))))))) %>% dplyr::filter(year4!=99) 
    
    temp <- temp %>% dplyr::mutate(strata1=as.numeric(Q8-year2)) %>% 
      dplyr::mutate(strata2=as.numeric(year2-year3)) %>% 
      dplyr::mutate(strata3=as.numeric(year3-year4)) %>% 
      dplyr::mutate(Set2=ifelse(PANEL>=14|PANEL-year2>=14,2,1)) %>% 
      dplyr::mutate(Set3=ifelse(PANEL>=14|PANEL-year3>=14,2,1))
    return(temp)
  }
  for (i in files) {
    mypath <- file.path(getwd(), i)
    assign(i, reading(mypath))
  }
  all <- lapply(files, function(x) eval(parse(text = x))) %>% 
    bind_rows() %>% as.data.frame() %>% dplyr::arrange(ID,PANEL) %>% dplyr::group_by(ID) %>% 
    dplyr::filter(PANEL==max(PANEL))
  return(all)
}

# #型変換
# n<-ncol(final)
# ix<-7:n
# final.2<-lapply(final[ix],as.numeric)
# final.2 <- as.data.frame(final.2)
# final[7:59] <-final.2 
# final$temp <- as.numeric(final$temp)


cf <- pref(c("ID","PANEL","Q3","Q8","Q11","Q13","Q16","Q18","Q21","Q23","Q26","Q28","Q31","Q33","Q57","Q60SR","Q61SR",
             "Q66","Q67A","Q142","Q145","Q153","Q213","Q297F","Q299A","Q299F","Q377",#","Q152","Q160","Q191K","Q139","Q323","
             "Q493BH","Q493BM","Q495BH","Q511","Q565","Q880")) #"Q673","Q700","Q297A",
                                
pre2 <- function(variables){  
  # setwd("/resources/data/panel/csv")
  setwd("C:\\Users\\KT\\Desktop\\panel\\userdata\\csv")
  files <- list.files(full.names = F, pattern="csv", recursive = FALSE)
  # all.temp <- foreach(i=2:4) %dopar% {}  #%dopar%
  reading <- function(x){
    temp<- fread(x) %>% dplyr::select(which(names(fread(x)) %in% variables))%>%
      mutate_each(funs(as.numeric), which(sapply(., is.character))) %>%
      filter(Q11 == 2|Q16 == 2|Q21 == 2|Q26 == 2|Q31==2)%>% 
      #mutate(num2=2) %>%
      mutate(year1 = as.numeric(ifelse(Q11==2, Q13, ifelse(Q16==2, Q18, ifelse(Q21==2, Q23,
      ifelse(Q26==2,Q28,ifelse(Q31==2,Q33,NA))))))) %>% filter(year2!=99) %>% 
      filter(Q11 == 3|Q16 == 3|Q21 == 3|Q26 == 3|Q31==3)%>% 
      #mutate(num3=3) %>%
      mutate(year2 = as.numeric(ifelse(Q11==3, Q13, ifelse(Q16==3, Q18, ifelse(Q21==3, Q23,
      ifelse(Q26==3,Q28,ifelse(Q31==3,Q33,NA))))))) %>% filter(year3!=99) %>% 
      mutate(num4=NA,year4=as.numeric(NA))
    temp <- temp %>% mutate(strata1=as.numeric(Q8-year2)) %>% mutate(strata2=as.numeric(year2-year3)) %>%
      mutate(strata3=as.numeric(NA)) %>% 
      mutate(Set2=ifelse(PANEL>=14|PANEL-year2>=14,2,1)) %>% mutate(Set3=ifelse(PANEL>=14|PANEL-year3>=14,2,1))
    return(temp)
  }
  for (i in files) {
    mypath <- file.path(getwd(), i)
    assign(i, reading(mypath))
  }
  all <- lapply(files, function(x) eval(parse(text = x))) %>% 
    bind_rows() %>% as.data.frame() %>% arrange(ID,PANEL) %>% group_by(ID) %>% filter(PANEL==max(PANEL))
  return(all)
}

c3.2 <- pre2(c("ID","PANEL","Q3","Q8","Q11","Q13","Q16","Q18","Q21","Q23","Q26","Q28","Q31","Q33","Q57","Q60SR","Q61SR",
                                 "Q66","Q67A","Q142","Q145","Q153","Q213","Q297F","Q299A","Q299F","Q377",#","Q152","Q160","Q191K","Q139","Q323","
               "Q493BH","Q493BM","Q495BH","Q511","Q565","Q880")) #,"Q673","Q700"

cfid <- cf$ID
cf2 <- c3.2[!(c3.2$ID %in% cfid),]

pre1 <- function(variables){  
  # library(foreach)
  # setwd("/resources/data/panel/csv")
  setwd("C:\\Users\\KT\\Desktop\\panel\\userdata\\csv")
  files <- list.files(full.names = F, pattern="csv", recursive = FALSE)
  # all.temp <- foreach(i=2:4) %dopar% {}  #%dopar%
  reading <- function(x){
    temp<- fread(x) %>% dplyr::select(which(names(fread(x)) %in% variables))%>% 
      mutate_each(funs(as.numeric), which(sapply(., is.character))) %>%
      filter(Q11 == 2|Q16 == 2|Q21 == 2|Q26 == 2|Q31==2)%>% 
      mutate(num2=2) %>%
      mutate(year2 = as.numeric(ifelse(Q11==2, Q13, ifelse(Q16==2, Q18, ifelse(Q21==2, Q23,
                                                                               ifelse(Q26==2,Q28,ifelse(Q31==2,Q33,NA))))))) %>% filter(year2!=99) %>% 
      mutate(num3=NA,year3 = as.numeric(NA),num4=NA, year4=as.numeric(NA))
    temp <- temp %>% mutate(strata1=as.numeric(Q8-year2)) %>% mutate(strata2=as.numeric(NA),strata3=as.numeric(NA)) %>% 
      mutate(Set2=ifelse(PANEL>=14|PANEL-year2>=14,2,1)) %>% mutate(Set3=NA)
    return(temp)
  }
  for (i in files) {
    mypath <- file.path(getwd(), i)
    assign(i, reading(mypath))
  }
  all <- lapply(files, function(x) eval(parse(text = x))) %>% 
    bind_rows() %>% as.data.frame() %>% arrange(ID,PANEL) %>% group_by(ID) %>% filter(PANEL==max(PANEL))
  return(all)
}

cf3.t <- pre1(c("ID","PANEL","Q3","Q8","Q11","Q13","Q16","Q18","Q21","Q23","Q26","Q28","Q31","Q33","Q57","Q60SR","Q61SR",
                                 "Q66","Q67A","Q142","Q145","Q153","Q213","Q297F","Q299A","Q299F","Q377",#,"Q152","Q160","Q191K","Q139","Q323",
                "Q493BH","Q493BM","Q495BH","Q511","Q565","Q880")) #,"Q673","Q700"
cf3id <- rbind(cfid,cf2$ID)
cf3 <- cf3.t[!(cf3.t$ID %in% cf3id),]

nopre <- function(variables){
  # setwd("/resources/data/panel/csv")
  setwd("C:\\Users\\KT\\Desktop\\panel\\userdata\\csv")
  files <- list.files(full.names = F, pattern="csv", recursive = FALSE)
  reading <- function(x){
    library(dplyr)
    temp<- fread(x) %>% dplyr::select(which(names(fread(x)) %in% variables))%>% 
      mutate_each(funs(as.numeric), which(sapply(., is.character))) %>%
      mutate(num2=NA,year2=NA,num3=3,year3 = as.numeric(NA),num4=NA, year4=as.numeric(NA)) %>% 
      mutate(strata1=as.numeric(NA),strata2=as.numeric(NA),strata3=as.numeric(NA)) %>% 
      mutate(Set2=NA) %>% mutate(Set3=NA)
    return(temp)
  }
  for (i in files) {
    mypath <- file.path(getwd(), i)
    assign(i, reading(mypath))
  }
  all <- lapply(files, function(x) eval(parse(text = x))) %>% 
    bind_rows() %>% as.data.frame() %>% arrange(ID,PANEL) %>% group_by(ID) %>% filter(PANEL==max(PANEL))
  return(all)
}
cfnot <- nopre(c("ID","PANEL","Q3","Q8","Q11","Q13","Q16","Q18","Q21","Q23","Q26","Q28","Q31","Q33","Q57","Q60SR","Q61SR",
                 "Q66","Q67A","Q142","Q145","Q153","Q213","Q297F","Q299A","Q299F","Q377",#","Q152","Q160","Q191K","Q139","Q323","
                 "Q493BH","Q493BM","Q495BH","Q511","Q565","Q880")) #"Q673","Q700","Q297A",
cfnoid <- rbind(cf3id,cf3$ID)
cfno <- cfnot[!(cfnot$ID %in% cfnoid),]


cfall <- rbind(cf,cf2,cf3,cfno)
# cfall <- cfall[,c(1:31,46,33:45)] 
cfall <- cfall %>% arrange(ID)

write.csv(cfall,"cfall.csv")

cfall <- cfall %>% ungroup()

cfall.f <- cfall %>% #select(-Q565,-Q673,-Q700) %>% 
  mutate(Q67=ifelse(Q67A==1,1,0)) %>% #mutate(C1=ifelse(Q57==1,1,0))%>% mutate(C2=ifelse(Q57==2,1,0)) %>% mutate(C3=ifelse(Q57==3,1,0))%>%
  mutate(daisotu1=ifelse(Q60SR==6|Q60SR==7,1,0)) %>% mutate(daisotu2=ifelse(Q61SR==6|Q61SR==7,1,0)) %>%
  mutate(nearoya=ifelse(Q377<=4,1,0)) %>% 
  mutate(income=ifelse(Q297F>=9997,NA,Q297F/100)) %>% mutate(ottoincome=ifelse(Q66<=2,0,1)) %>% 
  mutate(honninsyuugyou=ifelse(Q142==1,1,0)) %>% mutate(ottosyuugyou=ifelse(Q213==1,1,0)) %>% 
  mutate(zehi=ifelse(Q511==1,1,0)) %>% mutate(jouken=ifelse(Q511==2,1,0)) %>% 
  mutate(tokai=ifelse(Q3==1,1,0)) %>%  mutate(nearoya=ifelse(Q377<=4,1,0)) %>% 
  mutate(honninhatarakujikan=ifelse(Q493BH==99,NA,Q493BH)) %>% mutate(ottohatarakujikan=ifelse(Q495BH==99,NA,Q495BH)) %>% 
  #mutate(daikigyo=ifelse(Q145==7|Q145==8,1,0)) %>% mutate(koumuin=ifelse(Q145==8,1,0)) %>% 
  mutate(parttime=ifelse(Q142!=1,0,1)) %>% mutate(ikukyu=ifelse(Q880<=3,1,0)) %>% 
  mutate_each(funs(as.numeric), which(sapply(., is.character)))

cfallf <- datacox%>% #select(-Q565,-Q673,-Q700) %>% 
  mutate(Q67=ifelse(Q67A==1,1,0)) %>% #mutate(C1=ifelse(Q57==1,1,0))%>% mutate(C2=ifelse(Q57==2,1,0)) %>% mutate(C3=ifelse(Q57==3,1,0))%>%
  mutate(daisotu1=ifelse(Q60SR==6|Q60SR==7,1,0)) %>% mutate(daisotu2=ifelse(Q61SR==6|Q61SR==7,1,0)) %>%
  mutate(nearoya=ifelse(Q377<=4,1,0)) %>% 
  mutate(income=ifelse(Q297F>=9997,NA,Q297F/100)) %>% mutate(ottoincome=ifelse(Q66<=2,0,1)) %>% 
  mutate(honninsyuugyou=ifelse(Q142==1,1,0)) %>% mutate(ottosyuugyou=ifelse(Q213==1,1,0)) %>% 
  mutate(zehi=ifelse(Q511==1,1,0)) %>% mutate(jouken=ifelse(Q511==2,1,0)) %>% 
  mutate(tokai=ifelse(Q3==1,1,0)) %>%  mutate(nearoya=ifelse(Q377<=4,1,0)) %>% 
  mutate(honninhatarakujikan=ifelse(Q493BH==99,NA,Q493BH)) %>% mutate(ottohatarakujikan=ifelse(Q495BH==99,NA,Q495BH)) %>% 
  #mutate(daikigyo=ifelse(Q145==7|Q145==8,1,0)) %>% mutate(koumuin=ifelse(Q145==8,1,0)) %>% 
  mutate(parttime=ifelse(Q142!=1,0,1)) %>% mutate(ikukyu=ifelse(Q880<=3,1,0)) %>% 
  mutate_each(funs(as.numeric), which(sapply(., is.character)))
  

