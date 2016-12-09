library(tidyverse)
library(sqldf)
library(data.table)
library(foreach)


setwd("C:\\Users\\KT\\Desktop\\jhps\\userdata\\csv")
lf <- list.files(full.names = F, pattern="csv", recursive = FALSE)
# lf <- lf[2:25]
# lf <- lf[-c(1,4,10,17,21)]
# lf <- lf[-c(3,9,16,20,21)]
# lf <- lf[-c(1:2,5,11,18,22)]
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

setwd("C:\\Users\\KT\\Desktop\\jhps")
cpi <- fread("cpi.csv")

data1 <- data %>%  bind_rows(p1) %>% inner_join(cpi,by=c("PANEL"="chousa"))

# data1 <- data1 

ninzu <- data1 %>% group_by(PANEL) %>% summarise(n=n())
syussansu <- data1 %>% group_by(PANEL,Q67A) %>% summarise(n=n())


datas <- data1 %>% dplyr::select(ID,PANEL,Q1,Q3,Q8,Q11,Q13,Q16,Q18,Q21,Q23,Q26,Q28,Q31,Q33,Q57,Q60SR,Q61SR,
                                Q66,Q67A,Q142,Q145,Q153,Q213,Q296F,Q297F,Q299A,Q299F,Q377,#,Q152,Q160,Q191K,Q139,Q323,Q511,
                                Q493BH,Q493BM,Q495BH,Q511,Q565,Q673,Q700,Q860,Q880,cpi) %>% #Q511,Q565,Q673,Q700,Q493BM
  filter(Q1==1) %>% mutate(nenrei=Q8) %>% mutate(nenrei2=Q8^2) %>% 
  mutate(city=ifelse(Q3==1,1,0)) %>% mutate(C0=ifelse(Q57==0,1,0)) %>%  mutate(C1=ifelse(Q57==1,1,0))%>% 
  mutate(Q67=ifelse(Q67A==1,1,0)) %>% mutate(C2=ifelse(Q57==2,1,0)) %>% mutate(C3=ifelse(Q57==3,1,0))%>% 
  mutate(daisotu1=ifelse(Q60SR==6|Q60SR==7,1,0)) %>% mutate(daisotu2=ifelse(Q61SR==6|Q61SR==7,1,0)) %>%
  mutate(nearoya=ifelse(Q377<=4,1,0)) %>% #mutate(tcpi=cpi/100)%>% 
 mutate(income=ifelse(Q297F==9999,NA,(Q297F*cpi/10000))) %>% mutate(income2=ifelse(Q297F==9999,NA,(Q297F*cpi/10000)^2)) %>% 
 mutate(ottoincome=ifelse(Q296F==9999,NA,Q296F*cpi/10000)) %>% mutate(ottoincome2=ifelse(Q296F==9999,NA,(Q296F*cpi/10000)^2)) %>%
  mutate(honninsyuugyou=ifelse(Q142==1,1,0)) %>%   mutate(ottosyuugyou=ifelse(Q213==1,1,0)) %>%  
  mutate(honninhatarakujikan=ifelse(Q493BH==99,NA,Q493BH)) %>% mutate(ottohatarakujikan=ifelse(Q495BH==99,NA,Q495BH)) %>% 
  mutate(zehi=ifelse(Q511==1,1,0)) %>% mutate(jouken=ifelse(Q511==2,1,0)) %>% mutate(iranai=ifelse(Q511==3,1,0)) %>% 
  mutate(parttime=ifelse(Q142!=1,0,1)) %>% mutate(seigen12=ifelse(ottoincome>=960,1,0)) %>% mutate(seigen06=ifelse(ottoincome>=860,1,0)) %>% 
  mutate(seigen01=ifelse(ottoincome>=574,1,0)) %>% mutate(seigen82=ifelse(ottoincome>=475,1,0)) %>% 
  mutate(daikigyo=ifelse(Q145==7,1,0)) %>% mutate(koumuin=ifelse(Q145==8,1,0)) %>%
  mutate(ottojiei=ifelse(Q860<=5,1,0)) %>% mutate(ottoservice=ifelse(Q860==12,1,0)) %>% 
  mutate(ikukyu=ifelse(Q880<=3,1,0)) %>%  mutate(PANELf=PANEL-1) %>% 
  # mutate(dm95=ifelse(PANELf>=3&PANELf<=6,1,0)) %>% mutate(dm99=ifelse(PANELf==7,1,0)) %>%  mutate(dm00=ifelse(PANELf==8,1,0)) %>% 
  # mutate(dm01=ifelse(PANELf==9,1,0)) %>%  mutate(dm02=ifelse(PANELf==10|PANELf==11,1,0)) %>%  mutate(dm04=ifelse(PANELf==12,1,0)) %>% 
  # mutate(dm05=ifelse(PANELf==13,1,0)) %>%  mutate(dm06=ifelse(PANELf==14,1,0)) %>%  mutate(dm07=ifelse(PANELf>=15&PANELf<=17,1,0)) %>% 
  # mutate(dm10=ifelse(PANELf==18,1,0)) %>%  mutate(dm11=ifelse(PANELf==19,1,0)) %>%  mutate(dm12=ifelse(PANELf>=20,1,0))
  mutate(dm95=ifelse(PANELf>=3,1,0)) %>% mutate(dm99=ifelse(PANELf>=7,1,0)) %>%  mutate(dm00=ifelse(PANELf>=8,1,0)) %>% 
  mutate(dm01=ifelse(PANELf>=9,1,0)) %>%  mutate(dm02=ifelse(PANELf>=10,1,0)) %>%  mutate(dm04=ifelse(PANELf>=12,1,0)) %>% 
  mutate(dm05=ifelse(PANELf>=13,1,0)) %>%  mutate(dm06=ifelse(PANELf>=14,1,0)) %>%  mutate(dm07=ifelse(PANELf>=15,1,0)) %>% 
  mutate(dm10=ifelse(PANELf>=18,1,0)) %>%  mutate(dm11=ifelse(PANELf>=19,1,0)) %>%  mutate(dm12=ifelse(PANELf>=20,1,0)) %>% 
  #dplyr::select(-ID1,-PANELf,-panelf,-Q57,-Q377,-Q142,-Q66,-Q297F,-Q299A,-Q299F,-Q61SR,-Q60SR,-Q66,-Q213,-Q3,-Q493BH,-Q495BH,-Q880)

PANELf <- datas %>% select(ID,PANEL,Q67) %>% rename(ID1=ID,panelf=PANEL)
datas <- datas %>% select(-Q67A,-Q67)

# data.f <- PANELf %>% dplyr::inner_join(datas,by=c("PANEL"="PANEL1","ID"="ID"))
# merge(PANEL1,datas,by=c("PANEL"="PANEL1","ID"="ID"))
data.f <- sqldf("SELECT * FROM PANELf INNER JOIN datas ON PANELf.panelf=datas.PANELf AND PANELf.ID1=datas.ID")

data.f[is.na(data.f)]<-"."
  
datas2 <- data1 %>% dplyr::select(ID,PANEL,Q1,Q3,Q8,Q11,Q13,Q16,Q18,Q21,Q23,Q26,Q28,Q31,Q33,Q57,Q60SR,Q61SR,
                                   Q66,Q67A,Q142,Q145,Q153,Q213,Q296F,Q297F,Q299A,Q299F,Q377,#,Q152,Q160,Q191K,Q139,Q323,Q511,
                                   Q493BH,Q493BM,Q495BH,Q511,Q565,Q673,Q700,Q860,Q880,cpi) %>% #Q511,Q565,Q673,Q700,Q493BM
  filter(Q1==1) %>% mutate(nenrei=Q8) %>% mutate(nenrei2=Q8^2) %>% 
  mutate(city=ifelse(Q3==1,1,0)) %>% mutate(C0=ifelse(Q57==0,1,0)) %>%  mutate(C1=ifelse(Q57==1,1,0))%>% 
  mutate(Q67=ifelse(Q67A==1,1,0)) %>% mutate(C2=ifelse(Q57==2,1,0)) %>% mutate(C3=ifelse(Q57==3,1,0))%>% 
  mutate(daisotu1=ifelse(Q60SR==6|Q60SR==7,1,0)) %>% mutate(daisotu2=ifelse(Q61SR==6|Q61SR==7,1,0)) %>%
  mutate(nearoya=ifelse(Q377<=4,1,0)) %>% #mutate(tcpi=cpi/100)%>% 
  mutate(income=ifelse(Q297F==9999,NA,(Q297F*cpi/10000))) %>% mutate(income2=ifelse(Q297F==9999,NA,(Q297F*cpi/10000)^2)) %>% 
  mutate(ottoincome=ifelse(Q296F==9999,NA,Q296F*cpi/10000)) %>% mutate(ottoincome2=ifelse(Q296F==9999,NA,(Q296F*cpi/10000)^2)) %>%
  mutate(honninsyuugyou=ifelse(Q142==1,1,0)) %>%   mutate(ottosyuugyou=ifelse(Q213==1,1,0)) %>%  
  mutate(honninhatarakujikan=ifelse(Q493BH==99,NA,Q493BH)) %>% mutate(ottohatarakujikan=ifelse(Q495BH==99,NA,Q495BH)) %>% 
  mutate(zehi=ifelse(Q511==1,1,0)) %>% mutate(jouken=ifelse(Q511==2,1,0)) %>% mutate(parttime=ifelse(Q142!=1,0,1)) %>%
  mutate(daikigyo=ifelse(Q145==7,1,0)) %>% mutate(koumuin=ifelse(Q145==8,1,0)) %>%
  mutate(ottojiei=ifelse(Q860<=5,1,0)) %>% mutate(ottoservice=ifelse(Q860==12,1,0)) %>% 
  mutate(ikukyu=ifelse(Q880<=3,1,0)) %>%  mutate(PANELf=PANEL-1) %>% 
  mutate(dm95=ifelse(PANELf>=3&PANELf<=6,1,0)) %>% mutate(dm99=ifelse(PANELf==7,1,0)) %>%  mutate(dm00=ifelse(PANELf==8,1,0)) %>%
  mutate(dm01=ifelse(PANELf==9,1,0)) %>%  mutate(dm02=ifelse(PANELf==10|PANELf==11,1,0)) %>%  mutate(dm04=ifelse(PANELf==12,1,0)) %>%
  mutate(dm05=ifelse(PANELf==13,1,0)) %>%  mutate(dm06=ifelse(PANELf==14,1,0)) %>%  mutate(dm07=ifelse(PANELf>=15&PANELf<=17,1,0)) %>%
  mutate(dm10=ifelse(PANELf==18,1,0)) %>%  mutate(dm11=ifelse(PANELf==19,1,0)) %>%  mutate(dm12=ifelse(PANELf>=20,1,0))

data.f2 <- sqldf("SELECT * FROM PANELf INNER JOIN datas2 ON PANELf.panelf=datas2.PANELf AND PANELf.ID1=datas2.ID")

data.f2[is.na(data.f2)]<-"."

# data.f$Q67 <- as.numeric(data.f$Q67)

###多重代入法によるロジット
d1 <- data.f %>% filter(PANEL<14) %>% dplyr::select(-ID1,-PANELf,-panelf,-Q57,-Q377,-Q142,
      -Q66,-Q297F,-Q299A,-Q299F,-Q61SR,-Q60SR,-Q213,-Q3,-Q493BH,-Q495BH,-Q880)%>% 
  filter(Q67==0|Q67==1) #,-Q67A%>% mutate(set=1) 
d2 <- data.f %>% filter(PANEL>=14) %>% dplyr::select(-ID1,-PANELf,-panelf,-Q57,-Q377,-Q142,
      -Q66,-Q297F,-Q299A,-Q299F,-Q61SR,-Q60SR,-Q66,-Q213,-Q3,-Q493BH,-Q495BH,-Q880)%>% filter(Q67==0|Q67==1)#-ID,-Q67A,%>% mutate(set=2) 
# dmi <- rbind(d1,d2)

data.f <- data.f %>% filter(Q67==0|Q67==1) 
write.csv(data.f,"mlogit.csv")
write.csv(d1,"d1.csv")
write.csv(d2,"d2.csv")

#logit 出生準
firsttemp <- data.f %>% filter(C0==1) %>% filter(Q67!=1) #1人も子供がいない
l1 <- data.f %>% filter(C1==1,Q67==1) %>%  bind_rows(firsttemp)#出産時 %>% filter(C2==0,C3==0)
bl1 <- l1 %>% filter(PANEL<14)
al1 <- l1 %>% filter(PANEL>=14)

secondtemp <- data.f %>% filter(C1==1) %>% filter(Q67!=1)# %>% filter(C3==0)
l2 <- data.f  %>% filter(C2==1,Q67==1) %>%  bind_rows(secondtemp)#出産時%>% filter(C1==1),C3==0
bl2 <- l2 %>% filter(PANEL<14)
al2 <- l2 %>% filter(PANEL>=14)

thirdtemp <- data.f %>% filter(C2==1) %>% filter(Q67!=1)#C1==1, %>% filter(C3==0)
l3 <- data.f %>% filter(Q67==1,C3==1) %>%  bind_rows(thirdtemp)#出産時C1==1,%>% filter(C2==1) 
bl3 <- l3 %>% filter(PANEL<14)
al3 <- l3 %>% filter(PANEL>=14)

blogit <- data.f %>% filter(PANEL<14)
alogit <- data.f %>% filter(PANEL>=14)

setwd("C:\\Users\\KT\\Desktop\\jhps")
write.csv(bl1,"bl1.csv")
write.csv(bl2,"bl2.csv")
write.csv(bl3,"bl3.csv")
write.csv(al1,"al1.csv")
write.csv(al2,"al2.csv")
write.csv(al3,"al3.csv")
write.csv(l1,"l1.csv")
write.csv(l2,"l2.csv")
write.csv(l3,"l3.csv")
write.csv(data.f,"alllogit.csv")
write.csv(data.f,"alllogit2.csv")
write.csv(blogit,"blogit.csv")
write.csv(alogit,"alogit.csv")


###検査
d12 <- data.f %>% filter(PANEL<15) %>% dplyr::select(-ID1,-PANELf,-panelf,-Q57,-Q377,-Q142,
                                                    -Q66,-Q297F,-Q299A,-Q299F,-Q61SR,-Q60SR,-Q213,-Q3,-Q493BH,-Q495BH,-Q880)%>% 
  filter(Q67==0|Q67==1) #,-Q67A%>% mutate(set=1) 
d22 <- data.f %>% filter(PANEL>=15) %>% dplyr::select(-ID1,-PANELf,-panelf,-Q57,-Q377,-Q142,
                                                     -Q66,-Q297F,-Q299A,-Q299F,-Q61SR,-Q60SR,-Q66,-Q213,-Q3,-Q493BH,-Q495BH,-Q880)%>%
  filter(Q67==0|Q67==1)#-ID,-Q67A,%>% mutate(set=2) 
# dmi <- rbind(d1,d2)
setwd("C:\\Users\\KT\\Desktop\\jhps")
write.csv(d12,"d115.csv")
write.csv(d22,"d215.csv")


###検査
d13 <- data.f %>% filter(PANEL<10) %>% dplyr::select(-ID1,-PANELf,-panelf,-Q57,-Q377,-Q142,
                                                     -Q66,-Q297F,-Q299A,-Q299F,-Q61SR,-Q60SR,-Q213,-Q3,-Q493BH,-Q495BH,-Q880)%>% 
  filter(Q67==0|Q67==1) #,-Q67A%>% mutate(set=1) 
d23 <- data.f %>% filter(PANEL>=10) %>% dplyr::select(-ID1,-PANELf,-panelf,-Q57,-Q377,-Q142,
                                                      -Q66,-Q297F,-Q299A,-Q299F,-Q61SR,-Q60SR,-Q66,-Q213,-Q3,-Q493BH,-Q495BH,-Q880)%>%
  filter(Q67==0|Q67==1)#-ID,-Q67A,%>% mutate(set=2) 
# dmi <- rbind(d1,d2)
setwd("C:\\Users\\KT\\Desktop\\jhps")
write.csv(d13,"d110.csv")
write.csv(d23,"d210.csv")


###検査
d14 <- data.f %>% filter(PANEL<5) %>% dplyr::select(-ID1,-PANELf,-panelf,-Q57,-Q377,-Q142,
                                                     -Q66,-Q297F,-Q299A,-Q299F,-Q61SR,-Q60SR,-Q213,-Q3,-Q493BH,-Q495BH,-Q880)%>% 
  filter(Q67==0|Q67==1) #,-Q67A%>% mutate(set=1) 
d24 <- data.f %>% filter(PANEL>=5) %>% dplyr::select(-ID1,-PANELf,-panelf,-Q57,-Q377,-Q142,
                                                      -Q66,-Q297F,-Q299A,-Q299F,-Q61SR,-Q60SR,-Q66,-Q213,-Q3,-Q493BH,-Q495BH,-Q880)%>%
  filter(Q67==0|Q67==1)#-ID,-Q67A,%>% mutate(set=2) 
# dmi <- rbind(d1,d2)
setwd("C:\\Users\\KT\\Desktop\\jhps")
write.csv(d14,"d15.csv")
write.csv(d24,"d25.csv")

###検査
d15 <- data.f %>% filter(PANEL<5) %>% dplyr::select(-ID1,-PANELf,-panelf,-Q57,-Q377,-Q142,
                                                    -Q66,-Q297F,-Q299A,-Q299F,-Q61SR,-Q60SR,-Q213,-Q3,-Q493BH,-Q495BH,-Q880)%>% 
  filter(Q67==0|Q67==1) #,-Q67A%>% mutate(set=1) 
d25 <- data.f %>% filter(PANEL>=5,PANEL<15) %>% dplyr::select(-ID1,-PANELf,-panelf,-Q57,-Q377,-Q142,
                                                     -Q66,-Q297F,-Q299A,-Q299F,-Q61SR,-Q60SR,-Q66,-Q213,-Q3,-Q493BH,-Q495BH,-Q880)%>%
  filter(Q67==0|Q67==1)#-ID,-Q67A,%>% mutate(set=2) 
d35 <- data.f %>% filter(PANEL>=15) %>% dplyr::select(-ID1,-PANELf,-panelf,-Q57,-Q377,-Q142,
                                                     -Q66,-Q297F,-Q299A,-Q299F,-Q61SR,-Q60SR,-Q66,-Q213,-Q3,-Q493BH,-Q495BH,-Q880)%>%
  filter(Q67==0|Q67==1)#-ID,-Q67A,%>% mutate(set=2) 
# dmi <- rbind(d1,d2)
setwd("C:\\Users\\KT\\Desktop\\jhps")
write.csv(d15,"d13.csv")
write.csv(d25,"d23.csv")
write.csv(d35,"d33.csv")
