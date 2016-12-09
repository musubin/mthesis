library(tidyverse)
library(sqldf)
library(data.table)
library(foreach)


setwd("C:\\Users\\KT\\Desktop\\jhps\\userdata\\csv")
lf <- list.files(full.names = F, pattern="csv", recursive = FALSE)
# lf <- lf[-c(3,9,13,16,17,18,20,21)]
lf <- lf[-c(1)]

data <- data.frame()
for(i in 1:length(lf)){
  add <- fread(lf[i]) %>% mutate_each(funs(as.numeric), which(sapply(., is.character)))
  data <- add %>% bind_rows(data)
}

p1 <- fread("p1.csv") %>% mutate_each(funs(as.numeric), which(sapply(., is.character))) 
# p1$Q11 <- as.numeric(p1$Q11)
# data$V1 <- as.numeric(data$V1)

data1 <- data %>%  bind_rows(p1)

###cox用データ
datacox1.2 <- data1 %>%
  mutate_each(funs(as.numeric), which(sapply(., is.character))) %>%
  dplyr::filter(Q11 == 2|Q16 == 2|Q21 == 2|Q26 == 2|Q31==2|Q36==2|Q41==2|Q46==2|Q51==2)%>% 
  dplyr::filter(Q11 == 3|Q16 == 3|Q21 == 3|Q26 == 3|Q31==3|Q36==3|Q41==3|Q46==3|Q51==3)%>% 
  dplyr::filter(Q11 == 4|Q16 == 4|Q21 == 4|Q26 == 4|Q31==4|Q36==4|Q41==4|Q46==4|Q51==4)%>% 
  #dplyr::mutate(num2=2) %>%
  dplyr::mutate(year1 = as.numeric(ifelse(Q11==2, Q13, ifelse(Q16==2, Q18, ifelse(Q21==2, Q23,
 ifelse(Q26==2,Q28,ifelse(Q31==2,Q33,ifelse(Q36==2,Q38,
  ifelse(Q41==2,Q43,ifelse(Q46==2,Q48,ifelse(Q51==2,Q53,NA))))))))))) %>% 
  dplyr::filter(year1!=99) %>% 
  #dplyr::mutate(num3=3) %>%
  dplyr::mutate(year2 = as.numeric(ifelse(Q11==3, Q13, ifelse(Q16==3, Q18, ifelse(Q21==3, Q23,
  ifelse(Q26==3,Q28,ifelse(Q31==3,Q33,ifelse(Q36==3,Q38,
  ifelse(Q41==3,Q43,ifelse(Q46==3,Q48,ifelse(Q51==3,Q53,NA))))))))))) %>% dplyr::filter(year2!=99) %>% 
  #dplyr::mutate(num4=4) %>%
  dplyr::mutate(year3 = as.numeric(ifelse(Q11==4, Q13, ifelse(Q16==4, Q18, ifelse(Q21==4, Q23,
  ifelse(Q26==4,Q28,ifelse(Q31==4,Q33,ifelse(Q36==4,Q38,
  ifelse(Q41==4,Q43,ifelse(Q46==4,Q48,ifelse(Q51==4,Q53,NA))))))))))) %>%  dplyr::filter(year3!=99) %>% 
  dplyr::mutate(strata1=as.numeric(Q8-year1)) %>% 
  dplyr::mutate(strata2=as.numeric(year1-year2)) %>% 
  dplyr::mutate(strata3=as.numeric(year2-year3)) %>% 
  dplyr::mutate(Set1=ifelse(PANEL>=14|PANEL-year1>=14,1,0)) %>% #set2=1stchild
  dplyr::mutate(Set2=ifelse(PANEL>=14|PANEL-year2>=14,1,0)) %>%
  dplyr::mutate(Set3=ifelse(PANEL>=14|PANEL-year3>=14,1,0)) %>%#inner_join(cpi,by=c("PANEL"="chousa")) %>%
  bind_rows() %>% as.data.frame() %>% dplyr::arrange(ID,PANEL) %>% dplyr::group_by(ID) %>% 
  dplyr::filter(PANEL==max(PANEL))


datacox2.2 <- data1 %>%
  mutate_each(funs(as.numeric), which(sapply(., is.character))) %>%
  filter(Q11 == 2|Q16 == 2|Q21 == 2|Q26 == 2|Q31==2|Q36==2|Q41==2|Q46==2|Q51==2)%>% 
  filter(Q11 == 3|Q16 == 3|Q21 == 3|Q26 == 3|Q31==3|Q36==3|Q41==3|Q46==3|Q51==3)%>% 
  filter(Q57==2) %>% 
  # filter(Q11 != 4,Q16 != 4,Q21 != 4,Q26 != 4,Q31!=4,Q36!=4,Q41!=4,Q46!=4,Q51!=4)%>% 
  #mutate(num2=2) %>%
  dplyr::mutate(year1 = as.numeric(ifelse(Q11==2, Q13, ifelse(Q16==2, Q18, ifelse(Q21==2, Q23,
   ifelse(Q26==2,Q28,ifelse(Q31==2,Q33,ifelse(Q36==2,Q38,
   ifelse(Q41==2,Q43,ifelse(Q46==2,Q48,ifelse(Q51==2,Q53,NA))))))))))) %>%  filter(year1!=99) %>% 
  #mutate(num3=3) %>%
  dplyr::mutate(year2 = as.numeric(ifelse(Q11==3, Q13, ifelse(Q16==3, Q18, ifelse(Q21==3, Q23,
  ifelse(Q26==3,Q28,ifelse(Q31==3,Q33,ifelse(Q36==3,Q38,
  ifelse(Q41==3,Q43,ifelse(Q46==3,Q48,ifelse(Q51==3,Q53,NA))))))))))) %>% filter(year2!=99) %>% 
  mutate(year3=as.numeric(NA))%>%#num4=NA,
  mutate(strata1=as.numeric(Q8-year1)) %>% mutate(strata2=as.numeric(year1-year2)) %>%
  mutate(strata3=as.numeric(NA)) %>% 
  mutate(Set1=ifelse(PANEL>=14|PANEL-year1>=14,1,0)) %>% mutate(Set2=ifelse(PANEL>=14|PANEL-year2>=14,1,0)) %>% 
  mutate(Set3=NA) %>% 
  #inner_join(cpi,by=c("PANEL"="chousa")) %>%
  bind_rows() %>% as.data.frame() %>% dplyr::arrange(ID,PANEL) %>% dplyr::group_by(ID) %>% 
  dplyr::filter(PANEL==max(PANEL))

datacox3.2 <- data1 %>%
  mutate_each(funs(as.numeric), which(sapply(., is.character))) %>%
  filter(Q11 == 2|Q16 == 2|Q21 == 2|Q26 == 2|Q31==2|Q36==2|Q41==2|Q46==2|Q51==2)%>% 
  filter(Q57==1) %>% 
  #   filter(Q11 != 3,Q16 != 3,Q21 != 3,Q26 != 3,Q31!=3,Q36!=3,Q31!=3,Q36!=3,Q51!=3)%>% 
  #   filter(Q11 != 4,Q16 != 4,Q21 != 4,Q26 != 4,Q41!=4,Q46!=4,Q41!=4,Q46!=4,Q51!=4)%>% 
  #mutate(num2=2) %>%
  dplyr::mutate(year1 = as.numeric(ifelse(Q11==2, Q13, ifelse(Q16==2, Q18, ifelse(Q21==2, Q23,
  ifelse(Q26==2,Q28,ifelse(Q31==2,Q33,ifelse(Q36==2,Q38,
  ifelse(Q41==2,Q43,ifelse(Q46==2,Q48,ifelse(Q51==2,Q53,NA))))))))))) %>% filter(year1!=99) %>% 
  mutate(year2 = as.numeric(NA), year3=as.numeric(NA))%>% #num3=NA,num4=NA,
  mutate(strata1=as.numeric(Q8-year1)) %>% mutate(strata2=as.numeric(NA),strata3=as.numeric(NA)) %>% 
  mutate(Set1=ifelse(PANEL>=14|PANEL-year1>=14,1,0)) %>% mutate(Set2=NA) %>%  mutate(Set3=NA) %>% #inner_join(cpi,by=c("PANEL"="chousa")) %>%
  bind_rows() %>% as.data.frame() %>% dplyr::arrange(ID,PANEL) %>% dplyr::group_by(ID) %>% 
  dplyr::filter(PANEL==max(PANEL))

datacox4.2 <- data1 %>%
  mutate_each(funs(as.numeric), which(sapply(., is.character))) %>%
  filter(Q57==0) %>% 
  #   filter(Q11 != 2,Q16 != 2,Q21 != 2,Q26 != 2,Q21!=2,Q26!=2,Q21!=2,Q26!=2,Q51!=2)%>% 
  #   filter(Q11 != 3,Q16 != 3,Q21 != 3,Q26 != 3,Q31!=3,Q36!=3,Q31!=3,Q36!=3,Q51!=3)%>% 
  #   filter(Q11 != 4,Q16 != 4,Q21 != 4,Q26 != 4,Q41!=4,Q46!=4,Q41!=4,Q46!=4,Q51!=4)%>% 
  mutate_each(funs(as.numeric), which(sapply(., is.character))) %>%
  mutate(year1=as.numeric(NA),year2 = as.numeric(NA), year3=as.numeric(NA)) %>% #num2=NA,num3=3,num4=NA,
  mutate(strata1=as.numeric(NA),strata2=as.numeric(NA),strata3=as.numeric(NA)) %>% 
  mutate(Set1=NA) %>% mutate(Set2=NA) %>%  mutate(Set3=NA) %>% #inner_join(cpi,by=c("PANEL"="chousa")) %>%
  bind_rows() %>% as.data.frame() %>% dplyr::arrange(ID,PANEL) %>% dplyr::group_by(ID) %>% 
  dplyr::filter(PANEL==max(PANEL))

datacox.2 <- bind_rows(datacox1.2,datacox2.2,datacox3.2,datacox4.2)%>% #Q511,Q565,Q673,Q700,Q493BM
  dplyr::select(ID,PANEL,Q1,Q3,Q8,Q11,Q13,Q16,Q18,Q21,Q23,Q26,Q28,Q31,Q33,Q57,Q60SR,Q61SR, 
                Q66,Q67A,Q142,Q145,Q153,Q213,Q296F,Q297F,Q299A,Q299F,Q377,#,Q152,Q160,Q191K,Q139,Q323,Q511,filter(Q1==1) %>%
                Q493BH,Q493BM,Q495BH,Q511,Q565,Q673,Q700,Q860,Q880,strata1,strata2,strata3,Set1,Set2,Set3) %>% #Q511,Q565,Q673,Q700,Q493BM
  inner_join(cpi,by=c("PANEL"="chousa")) %>% mutate(nenrei=Q8) %>% mutate(nenrei2=Q8^2) %>% 
  mutate(tokai=ifelse(Q3==1,1,0)) %>% mutate(C0=ifelse(Q57==0,1,0)) %>%  mutate(C1=ifelse(Q57==1,1,0))%>% 
  mutate(Q67=ifelse(Q67A==1,1,0)) %>% mutate(C2=ifelse(Q57==2,1,0)) %>% mutate(C3=ifelse(Q57==3,1,0))%>% 
  mutate(daisotu1=ifelse(Q60SR==6|Q60SR==7,1,0)) %>% mutate(daisotu2=ifelse(Q61SR==6|Q61SR==7,1,0)) %>%
  mutate(nearoya=ifelse(Q377<=4,1,0)) %>% 
  # mutate(income2=ifelse(Q297F==9999,NA,(Q297F/100)^2)) %>% mutate(ottoincome2=ifelse(Q296F==9999,NA,(Q296F/100)^2)) %>%
  mutate(income=ifelse(Q297F==9999,NA,(Q297F*cpi/10000))) %>% mutate(income2=ifelse(Q297F==9999,NA,(Q297F*cpi/10000)^2)) %>% 
  mutate(ottoincome=ifelse(Q296F==9999,NA,Q296F*cpi/10000)) %>% mutate(ottoincome2=ifelse(Q296F==9999,NA,(Q296F*cpi/10000)^2)) %>%
  #mutate(ottoincome=ifelse(Q296F==9999,NA,Q296F/100)) %>% mutate(income=ifelse(Q297F==9999,NA,Q297F/100)) %>%
  # mutate(honninsyuugyou=ifelse(Q142==1,1,0)) %>% mutate(ottosyuugyou=ifelse(Q213==1,1,0)) %>%  
  mutate(honninhatarakujikan=ifelse(Q493BH==99,NA,Q493BH)) %>% mutate(ottohatarakujikan=ifelse(Q495BH==99,NA,Q495BH)) %>% 
  mutate(zehi=ifelse(Q511==1,1,0)) %>% mutate(jouken=ifelse(Q511==2,1,0)) %>% mutate(parttime=ifelse(Q142!=1,0,1)) %>%
  mutate(daikigyo=ifelse(Q145==7,1,0)) %>% mutate(koumuin=ifelse(Q145==8,1,0)) %>%
  mutate(ottojiei=ifelse(Q860<=5,1,0)) %>% mutate(ottoservice=ifelse(Q860==12,1,0)) %>% 
  mutate(ikukyu=ifelse(Q880<=3,1,0)) %>%  mutate(PANELf=PANEL-1) %>% #mutate(censor1=ifelse(strata1>=0,1,0)) %>% 
  dplyr::select(-Q57,-Q377,-Q142,-Q66,-Q297F,-Q299A,-Q299F,-Q61SR,-Q60SR,-Q213,-Q3,-Q493BH,-Q495BH,-Q880,-Q67A)%>% 
  bind_rows() %>% as.data.frame() %>% dplyr::arrange(ID,PANEL) %>% dplyr::group_by(ID) %>% 
  dplyr::filter(PANEL==max(PANEL))

datacox.2[is.na(datacox.2)]<-"."


write.csv(datacox.2,"svdata.csv")


cfid <- cf$ID
cf2 <- c3.2[!(c3.2$ID %in% cfid),]