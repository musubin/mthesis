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
datacox1 <- data1 %>%
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
  dplyr::mutate(Set2=ifelse(PANEL>=14|PANEL-year1>=14,2,1)) %>% 
  dplyr::mutate(Set3=ifelse(PANEL>=14|PANEL-year2>=14,2,1))


datacox2 <- data1 %>%
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
  mutate(Set2=ifelse(PANEL>=14|PANEL-year2>=14,2,1)) %>% mutate(Set3=ifelse(PANEL>=14|PANEL-year3>=14,2,1))

datacox3 <- data1 %>%
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
  mutate(Set2=ifelse(PANEL>=14|PANEL-year2>=14,2,1)) %>% mutate(Set3=NA)

datacox4 <- data1 %>%
  mutate_each(funs(as.numeric), which(sapply(., is.character))) %>%
  filter(Q57==0) %>% 
#   filter(Q11 != 2,Q16 != 2,Q21 != 2,Q26 != 2,Q21!=2,Q26!=2,Q21!=2,Q26!=2,Q51!=2)%>% 
#   filter(Q11 != 3,Q16 != 3,Q21 != 3,Q26 != 3,Q31!=3,Q36!=3,Q31!=3,Q36!=3,Q51!=3)%>% 
#   filter(Q11 != 4,Q16 != 4,Q21 != 4,Q26 != 4,Q41!=4,Q46!=4,Q41!=4,Q46!=4,Q51!=4)%>% 
  mutate_each(funs(as.numeric), which(sapply(., is.character))) %>%
  mutate(year1=as.numeric(NA),year2 = as.numeric(NA), year3=as.numeric(NA)) %>% #num2=NA,num3=3,num4=NA,
  mutate(strata1=as.numeric(NA),strata2=as.numeric(NA),strata3=as.numeric(NA)) %>% 
  mutate(Set2=NA) %>% mutate(Set3=NA)

datacox <- bind_rows(datacox1,datacox2,datacox3,datacox4)%>% #Q511,Q565,Q673,Q700,Q493BM
  dplyr::select(ID,PANEL,Q1,Q3,Q8,Q11,Q13,Q16,Q18,Q21,Q23,Q26,Q28,Q31,Q33,Q57,Q60SR,Q61SR,
                Q66,Q67A,Q142,Q145,Q153,Q213,Q296F,Q297F,Q299A,Q299F,Q377,#,Q152,Q160,Q191K,Q139,Q323,Q511,
                Q493BH,Q493BM,Q495BH,Q511,Q565,Q673,Q700,Q860,Q880,strata1,strata2,strata3) %>% #Q511,Q565,Q673,Q700,Q493BM
  filter(Q1==1) %>% mutate(nenrei=Q8) %>% mutate(nenrei2=Q8^2) %>% 
  mutate(tokai=ifelse(Q3==1,1,0)) %>% mutate(C0=ifelse(Q57==0,1,0)) %>%  mutate(C1=ifelse(Q57==1,1,0))%>% 
  mutate(Q67=ifelse(Q67A==1,1,0)) %>% mutate(C2=ifelse(Q57==2,1,0)) %>% mutate(C3=ifelse(Q57==3,1,0))%>% 
  mutate(daisotu1=ifelse(Q60SR==6|Q60SR==7,1,0)) %>% mutate(daisotu2=ifelse(Q61SR==6|Q61SR==7,1,0)) %>%
  mutate(nearoya=ifelse(Q377<=4,1,0)) %>% mutate(income=ifelse(Q297F==9999,NA,Q297F/100)) %>%
  mutate(income2=ifelse(Q297F==9999,NA,(Q297F/100)^2)) %>% mutate(ottoincome2=ifelse(Q296F==9999,NA,(Q296F/100)^2)) %>%
  mutate(ottoincome=ifelse(Q296F==9999,NA,Q296F/100)) %>% mutate(honninsyuugyou=ifelse(Q142==1,1,0)) %>% 
  mutate(ottosyuugyou=ifelse(Q213==1,1,0)) %>%  
  mutate(honninhatarakujikan=ifelse(Q493BH==99,NA,Q493BH)) %>% mutate(ottohatarakujikan=ifelse(Q495BH==99,NA,Q495BH)) %>% 
  mutate(zehi=ifelse(Q511==1,1,0)) %>% mutate(jouken=ifelse(Q511==2,1,0)) %>% mutate(parttime=ifelse(Q142!=1,0,1)) %>%
  mutate(daikigyo=ifelse(Q145==7,1,0)) %>% mutate(koumuin=ifelse(Q145==8,1,0)) %>%
  mutate(ottojiei=ifelse(Q860<=5,1,0)) %>% mutate(ottoservice=ifelse(Q860==12,1,0)) %>% 
  mutate(ikukyu=ifelse(Q880<=3,1,0)) %>%  mutate(PANELf=PANEL-1) %>% 
  mutate(dm95=ifelse(PANELf>=3,1,0)) %>% mutate(dm99=ifelse(PANELf>=7,1,0)) %>%  mutate(dm00=ifelse(PANELf>=8,1,0)) %>% 
  mutate(dm01=ifelse(PANELf>=9,1,0)) %>%  mutate(dm02=ifelse(PANELf>=10,1,0)) %>%  mutate(dm04=ifelse(PANELf>=12,1,0)) %>% 
  mutate(dm05=ifelse(PANELf>=13,1,0)) %>%  mutate(dm06=ifelse(PANELf>=14,1,0)) %>%  mutate(dm07=ifelse(PANELf>=15,1,0)) %>% 
  mutate(dm10=ifelse(PANELf>=18,1,0)) %>%  mutate(dm11=ifelse(PANELf>=19,1,0)) %>%  mutate(dm12=ifelse(PANELf>=20,1,0)) %>% 
  dplyr::select(-Q57,-Q377,-Q142,-Q66,-Q297F,-Q299A,-Q299F,-Q61SR,-Q60SR,-Q213,-Q3,-Q493BH,-Q495BH,-Q880,-Q67A)

# datacox %>% group_by(Q67) %>% 　summarise(count=n())

PANELf <- datacox %>% select(ID,PANEL,Q67) %>% rename(ID1=ID,panelf=PANEL)

# data.f <- PANELf %>% dplyr::inner_join(datas,by=c("PANEL"="PANEL1","ID"="ID"))
data.f <- sqldf("SELECT * FROM PANELf INNER JOIN data1 ON PANELf.panelf=datas.panelf AND PANELf.ID1=datas.ID")

datacox[is.na(datacox)]<-"."

# cox1st <- datacox %>% filter(C1+C2+C3==0|C1+Q67==2) %>% 
cox1sttemp <- datacox %>% filter(C0==1) %>% filter(Q67!=1)#1人も子供がいない
cox1st <- datacox %>% filter(C1==1,Q67==1) %>%  bind_rows(cox1sttemp)#出産時 %>% filter(C2==0,C3==0)
cox2ndtemp <- datacox %>% filter(C1==1) %>% filter(Q67!=1)# %>% filter(C3==0)
cox2nd <- datacox  %>% filter(C2==1,Q67==1) %>%  bind_rows(cox2ndtemp)#出産時%>% filter(C1==1),C3==0
cox3rdtemp <- datacox %>% filter(C2==1) %>% filter(Q67!=1)#C1==1, %>% filter(C3==0)
cox3rd <- datacox %>% filter(Q67==1,C3==1) %>%  bind_rows(cox3rdtemp)#出産時C1==1,%>% filter(C2==1) 

# datacox[is.na(datacox)]<-0

bid1 <- cox1st %>% filter(Q67==1) %>% filter(PANEL<14)
bid1$ID
aid1 <- cox1st %>% filter(Q67==1) %>% filter(PANEL>=14)
aid1$ID
bid2 <- cox2nd %>% filter(Q67==1) %>% filter(PANEL<14)
bid2$ID
aid2 <- cox2nd %>% filter(Q67==1) %>% filter(PANEL>=14)
aid2$ID
bid3 <- cox3rd %>% filter(Q67==1) %>% filter(PANEL<14)
bid3$ID
aid3 <- cox3rd %>% filter(Q67==1) %>% filter(PANEL>=14)
aid3$ID

#%>%  mutate(PANELf=PANEL-1)  mutate(PANELf=PANEL-1) %>% mutate(panel=PANEL)-ID1,-PANELf,-panelf,
#write.csv(datacox,"datacox.csv")
###1st child
setwd("C:\\Users\\KT\\Desktop\\jhps")#\\userdata
before1 <- cox1st %>% filter(PANEL<14)#filter(ID %in% bid1$ID) #%>% mutate(fyear = Q8-year2) %>% mutate(Set=0) %>% mutate(cens=1)
after1 <- cox1st %>% filter(PANEL>=14)#filter(ID %in% aid1$ID) #%>% mutate(fyear = Q8-year2) %>%  mutate(Set=1) %>% mutate(cens=1)

write.csv(before1,"before1.csv")
write.csv(after1,"after1.csv")

before2 <- cox2nd %>% filter(PANEL<14)#filter(ID %in% bid2$ID) #%>% mutate(fyear = Q8-year2) %>% mutate(Set=0) %>% mutate(cens=1)
after2 <- cox2nd %>% filter(PANEL>=14)#filter(ID %in% aid2$ID) #%>% mutate(fyear = Q8-year2) %>%  mutate(Set=1) %>% mutate(cens=1)

write.csv(before2,"before2.csv")
write.csv(after2,"after2.csv")

before3 <- cox3rd %>% filter(PANEL<14)#filter(ID %in% bid3$ID) #%>% mutate(fyear = Q8-year2) %>% mutate(Set=0) %>% mutate(cens=1)
after3 <- cox3rd %>% filter(PANEL>=14)#filter(ID %in% aid3$ID) #%>% mutate(fyear = Q8-year2) %>%  mutate(Set=1) %>% mutate(cens=1)

write.csv(before3,"before3.csv")
write.csv(after3,"after3.csv")

first <- before1 %>% mutate(set=1) %>% bind_rows(after1 %>% mutate(set=2))
second <- before2 %>% mutate(set=1) %>% bind_rows(after2 %>% mutate(set=2))
third <- before3 %>% mutate(set=1) %>% bind_rows(after3 %>% mutate(set=2))

write.csv(first,"first.csv")
