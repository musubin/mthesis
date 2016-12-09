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


datadm <- data1 %>% dplyr::select(ID,PANEL,Q1,Q3,Q8,Q11,Q13,Q16,Q18,Q21,Q23,Q26,Q28,Q31,Q33,Q57,Q60SR,Q61SR,
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
  # mutate(dm95=ifelse(PANELf>=3&PANELf<=6,1,0)) %>% mutate(dm99=ifelse(PANELf==7,1,0)) %>%  mutate(dm00=ifelse(PANELf==8,1,0)) %>% 
  # mutate(dm01=ifelse(PANELf==9,1,0)) %>%  mutate(dm02=ifelse(PANELf==10|PANELf==11,1,0)) %>%  mutate(dm04=ifelse(PANELf==12,1,0)) %>% 
  # mutate(dm05=ifelse(PANELf==13,1,0)) %>%  mutate(dm06=ifelse(PANELf==14,1,0)) %>%  mutate(dm07=ifelse(PANELf>=15&PANELf<=17,1,0)) %>% 
  # mutate(dm10=ifelse(PANELf==18,1,0)) %>%  mutate(dm11=ifelse(PANELf==19,1,0)) %>%  mutate(dm12=ifelse(PANELf>=20,1,0))
  mutate(dm95=ifelse(PANELf>=3,1,0)) %>% mutate(dm99=ifelse(PANELf>=7,1,0)) %>%  mutate(dm00=ifelse(PANELf>=8,1,0)) %>% 
  mutate(dm01=ifelse(PANELf>=9,1,0)) %>%  mutate(dm02=ifelse(PANELf>=10,1,0)) %>%  mutate(dm04=ifelse(PANELf>=12,1,0)) %>% 
  mutate(dm05=ifelse(PANELf>=13,1,0)) %>%  mutate(dm06=ifelse(PANELf>=14,1,0)) %>%  mutate(dm07=ifelse(PANELf>=15,1,0)) %>% 
  mutate(dm10=ifelse(PANELf>=18,1,0)) %>%  mutate(dm11=ifelse(PANELf>=19,1,0)) %>%  mutate(dm12=ifelse(PANELf>=20,1,0)) %>% 
  mutate(citydm=ifelse(Q3==1,1,0)*dm95) %>% mutate(C0dm=ifelse(Q57==0,1,0)*dm95) %>%  mutate(C1dm=ifelse(Q57==1,1,0)*dm95)%>% 
  mutate(C2dm=ifelse(Q57==2,1,0)*dm95) %>% mutate(C3dm=ifelse(Q57==3,1,0)*dm95)%>% 
  mutate(daisotu1dm=ifelse(Q60SR==6|Q60SR==7,1,0)*dm95) %>% mutate(daisotu2dm=ifelse(Q61SR==6|Q61SR==7,1,0)*dm95) %>%
  mutate(nearoyadm=ifelse(Q377<=4,1,0)) %>% #mutate(tcpi=cpi/100)%>% 
  mutate(incomedm=ifelse(Q297F==9999,NA,(Q297F*cpi/10000))*dm95) %>% mutate(income2dm=ifelse(Q297F==9999,NA,(Q297F*cpi/10000)^2)*dm95) %>% 
  mutate(ottoincomdm=ifelse(Q296F==9999,NA,Q296F*cpi/10000)*dm95) %>% mutate(ottoincome2dm=ifelse(Q296F==9999,NA,(Q296F*cpi/10000)^2)*dm95) %>%
  mutate(honninsyuugyoudm=ifelse(Q142==1,1,0)*dm95) %>% mutate(ottosyuugyoudm=ifelse(Q213==1,1,0)*dm95) %>%  
  mutate(honninhatarakujikandm=ifelse(Q493BH==99,NA,Q493BH)*dm95) %>% mutate(ottohatarakujikandm=ifelse(Q495BH==99,NA,Q495BH)*dm95) %>% 
  mutate(zehidm=ifelse(Q511==1,1,0)*dm95) %>% mutate(joukendm=ifelse(Q511==2,1,0)*dm95) %>% mutate(parttime=ifelse(Q142!=1,0,1)*dm95) %>%
  mutate(daikigyodm=ifelse(Q145==7,1,0)*dm95) %>% mutate(koumuindm=ifelse(Q145==8,1,0)*dm95) %>%
  mutate(ottojieidm=ifelse(Q860<=5,1,0)*dm95) %>% mutate(ottoservicedm=ifelse(Q860==12,1,0)*dm95) %>% 
  mutate(ikukyudm=ifelse(Q880<=3,1,0)*dm95)


PANELf <- datas %>% select(ID,PANEL,Q67) %>% rename(ID1=ID,panelf=PANEL)

datas <- datas %>% select(-Q67A,-Q67)

# data.f <- PANELf %>% dplyr::inner_join(datas,by=c("PANEL"="PANEL1","ID"="ID"))
# merge(PANEL1,datas,by=c("PANEL"="PANEL1","ID"="ID"))
data.f <- sqldf("SELECT * FROM PANELf INNER JOIN datas ON PANELf.panelf=datas.PANELf AND PANELf.ID1=datas.ID")

data.f[is.na(data.f)]<-"."