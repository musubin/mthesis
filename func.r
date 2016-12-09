library(dplyr)
library(sqldf)
library(magrittr)
library(data.table)
# library(foreach)
###

#allfunc <- function(variables){

pref <- function(variables){  
  # library(foreach)
  setwd("C:\\Users\\KT\\Desktop\\jhps\\userdata\\csv")
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
             "Q66","Q67A","Q142","Q152","Q153","Q160","Q191","Q213","Q297A","Q297F","Q299A","Q299F","Q377",#,"Q139","Q323",
             "Q493BH","Q493BM","Q495BH","Q511","Q565","Q673","Q700")) 
# cf$Q11 <- as.numeric(cf$Q11)

pre2 <- function(variables){  
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
      filter(Q11 == 3|Q16 == 3|Q21 == 3|Q26 == 3|Q31==3)%>% 
      mutate(num3=3) %>%
      mutate(year3 = as.numeric(ifelse(Q11==3, Q13, ifelse(Q16==3, Q18, ifelse(Q21==3, Q23,
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
               "Q66","Q67A","Q142","Q152","Q153","Q160","Q191","Q213","Q297A","Q297F","Q299A","Q299F","Q377",#,"Q139","Q323",
               "Q493BH","Q493BM","Q495BH","Q511","Q565","Q673","Q700")) 

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
                "Q66","Q67A","Q142","Q152","Q153","Q160","Q191","Q213","Q297A","Q297F","Q299A","Q299F","Q377",#,"Q139","Q323",
                "Q493BH","Q493BM","Q495BH","Q511","Q565","Q673","Q700")) 
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
                 "Q66","Q67A","Q142","Q152","Q153","Q160","Q191","Q213","Q297A","Q297F","Q299A","Q299F","Q377",#,"Q139","Q323",
                 "Q493BH","Q493BM","Q495BH","Q511","Q565","Q673","Q700")) 
cfnoid <- rbind(cf3id,cf3$ID)
cfno <- cfnot[!(cfnot$ID %in% cfnoid),]


cfall <- rbind(cf,cf2,cf3,cfno)
cfall <- cfall[,c(1:31,46,33:45)] 
cfall <- cfall %>% arrange(ID)
# }

write.csv(cfall,"cfall.csv")

pre <- function(variables, childnum){  
  setwd("C:\\Users\\KT\\Desktop\\panel\\userdata\\csv")
  files <- list.files(full.names = F, pattern="csv", recursive = FALSE)
  reading <- function(x){

    temp<- fread(x) %>% dplyr::select(which(names(fread(x)) %in% variables)) %>% 
      #temp<- fread(x) %>% select(one_of(variables))%>%
      filter(Q11 == childnum|Q16 == childnum|Q21 == childnum|Q26 == childnum|Q31==childnum)%>% 
      mutate(cnum=childnum) %>%
      mutate(year = ifelse(Q11==childnum, Q13, ifelse(Q16==childnum, Q18, ifelse(Q21==childnum, Q23,
      ifelse(Q26==childnum,Q28,ifelse(Q31==childnum,Q33,NA)))))) %>% filter(year!=99)
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

#allfunc()


pwp <- function(i){
  
  
  
}

### テスト
all.temp <- foreach(i=3:4) %dopar% {
  add.2 <- pre(c("ID","PANEL","Q8","Q11","Q13","Q16","Q18","Q21","Q23","Q26","Q28","Q31","Q33","Q493BH","Q493BM","Q297A","Q297F",
        "Q299A","Q299F","Q565"), 2) 
  add <- pre(c("ID","PANEL","Q8","Q11","Q13","Q16","Q18","Q21","Q23","Q26","Q28","Q31","Q33","Q493BH","Q493BM","Q297A","Q297F",
               "Q299A","Q299F","Q565"), 3) 
  
  # add <- fread(lf[i])
  # data <- rbind(data,add)
}

c2 <- pre(c("ID","PANEL","Q8","Q11","Q13","Q16","Q18","Q21","Q23","Q26","Q28","Q31","Q33","Q493BH","Q493BM","Q297A","Q297F",
           "Q299A","Q299F","Q565"), 2) 
c2$year <- as.integer(c2$year)
c3.2 <- pre2(c("ID","PANEL","Q8","Q11","Q13","Q16","Q18","Q21","Q23","Q26","Q28","Q31","Q33","Q493BH","Q493BM","Q297A","Q297F",
            "Q299A","Q299F","Q565"), 3) 
c3 <-pre(c("ID","PANEL","Q8","Q11","Q13","Q16","Q18","Q21","Q23","Q26","Q28","Q31","Q33","Q493BH","Q493BM","Q297A","Q297F",
           "Q299A","Q299F","Q565"), 3) 
c4 <-pre(c("ID","PANEL","Q8","Q11","Q13","Q16","Q18","Q21","Q23","Q26","Q28","Q31","Q33","Q493BH","Q493BM","Q297A","Q297F",
           "Q299A","Q299F","Q565"), 4) 
c5 <-pre(c("ID","PANEL","Q8","Q11","Q13","Q16","Q18","Q21","Q23","Q26","Q28","Q31","Q33","Q493BH","Q493BM","Q297A","Q297F",
           "Q299A","Q299F","Q565"), 5) 
c6 <-pre(c("ID","PANEL","Q8","Q11","Q13","Q16","Q18","Q21","Q23","Q26","Q28","Q31","Q33","Q493BH","Q493BM","Q297A","Q297F",
           "Q299A","Q299F","Q565"), 6) 
###2to3
c3.1 <- c3 %>% dplyr::select(ID,PANEL,cnum,year) %>% rename(num3=cnum,year3=year)
c3.1$year3 <- as.integer(c3.1$year3)
#c3.1$year.x <- as.integer(c3.1$year.x)
#c3.1$ID <- as.integer(c3.1$ID)
c23 <- c2 %>% inner_join(c3.1,by="ID")
c23$year3 <- as.integer(c23$year3)
c23$year <- as.integer(c23$year)
c23 <- c23 %>% mutate(year23=year-year3)

###3to4
c3.1 <- c3 %>% dplyr::select(ID,PANEL,cnum,year) %>% rename(num3=cnum,year3=year)
c3.1$year3 <- as.integer(c3.1$year3)
#c3.1$year.x <- as.integer(c3.1$year.x)
#c3.1$ID <- as.integer(c3.1$ID)
c23 <- c2 %>% inner_join(c3.1,by="ID")
c23$year3 <- as.integer(c23$year3)
c23$year <- as.integer(c23$year)
c23 <- c23 %>% mutate(year23=year-year3)
c23check <- c23 %>% filter(PANEL.x != PANEL.y)


c23.f <- c23 %>% select(-PANEL.x,-year,-cnum,-year3) %>% rename(cnum=num3,PANEL=PANEL.y,year=year23)
c23.f <- c23.f[,c(1,19,2:18,20,21)]
c2and3 <- rbind(c2,c23.f) 
c2and3 <-c2and3 %>% ungroup() %>% dplyr::arrange(ID)


reading <- function(x,childnum){
  library(dplyr)
  library(data.table)
  temp<- fread(x) %>% dplyr::select(which(names(fread(x)) %in% variables)) %>% 
    #temp<- fread(x) %>% select(one_of(variables))%>%
    filter(Q11 == childnum|Q16 == childnum|Q21 == childnum|Q26 == childnum|Q31==childnum)%>% 
    mutate(cnum=childnum) %>%
    mutate(year = ifelse(Q11==childnum, Q13, ifelse(Q16==childnum, Q18, ifelse(Q21==childnum, Q23,
    ifelse(Q26==childnum,Q28,ifelse(Q31==childnum,Q33,NA)))))) %>% filter(year!=99)
  return(temp)
}

m20 <- reading("p20.csv",2)#,variables)
variables <- c("ID","PANEL","Q8","Q11","Q13","Q16","Q18","Q21","Q23","Q26","Q28","Q31","Q33","Q493BH","Q493BM","Q297A","Q297F",
               "Q299A","Q299F","Q565")

