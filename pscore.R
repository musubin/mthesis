library(dplyr)
library(sqldf)
library(magrittr)
library(data.table)


result.p <- glm(ikukyu~zehi+jouken+daikigyo+koumuin+parttime , data=data.f, 
                family=binomial(link=logit))#Q511+Q565+Q673+Q700+
summary(result.p)

###IPW/DR推定量

ipwe <- function(data, target, treat, ps) {
  y         <- as.matrix(data[target])[,]
  z1        <- as.matrix(data[treat])[,]
  z2        <- 1-z1
  z         <- cbind(z1, z2)
  ipw1      <- (z1/ps)/(length(z1)/sum(z1))
  ipw2      <- (z2/(1-ps))/(length(z2)/sum(z2))
  ipw       <- ipw1+ipw2
  ipw12.reg <- lm(y~z-1, data=data, weights=ipw)
  return(ipw12.reg)
}
ps <- result.p$fitted
ret <- ipwe(data.f, "Q67", "ikukyu", ps)

###TJO
# IPW
y <- data.f$Q67
z1 <- data.f$ikukyu
# y <- lalonde$re78
# z1 <- lalonde$treat
ipwe1 <- sum((z1*y)/ps)/sum(z1/ps)
ipwe0 <- sum(((1-z1)*y)/(1-ps))/sum((1-z1)/(1-ps))
ipwe1 - ipwe0


# DR
# SAMさんの例にならって関数化してあります
dre <- function(data, target, treat, ps, formula) {
  n       <- nrow(data)
  y       <- data[target]
  data1   <- data[data[treat]==1,]
  data0   <- data[data[treat]==0,]
  model1  <- lm(formula=formula, data=data1)
  model0  <- lm(formula=formula, data=data0)
  fitted1 <- predict(model1, data)
  fitted0 <- predict(model0, data)
  dre1    <- (1/n)*sum(y+((z1-ps)/ps)*(y-fitted1))
  dre0    <- (1/n)*sum(((1-z1)*y)/(1-ps)+(1-(1-z1)/(1-ps))*fitted0)
  return(c(dre1, dre0))
}
ret <- dre(data.1, "Q67", "ikukyu", ps,
           Q67 ~ #+Q153+Q297F+Q299F+Q299AQ323+Q152+Q160+Q139+Q191+Q377+Q57+Q60SR+Q61SR+
             tokai+daisotu1+daisotu2+C1+C2+C3+nearoya+income+ottoincome+#Q60SR+Q61SR+Q57+Q66+
             honninsyuugyou+ottosyuugyou+honninhatarakujikan+ottohatarakujikan+zehi+jouken+ikukyu+parttime)
# re78~age+educ+black+hisp+married+nodegr)
ret
ret[1] - ret[2]

###selectしてから
# setwd("/resources/data/panel/csv")
setwd("C:\\Users\\KT\\Desktop\\panel\\userdata\\csv")
lf <- list.files(full.names = F, pattern="csv", recursive = FALSE)
# lf <- lf[2:25]
# lf <- lf[-c(1,4,10,17,21)]
# lf <- lf[c(5:9,11:13,15:16)]
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

data.s <- data1 %>% dplyr::select(ID,PANEL,Q57,Q60SR,Q61SR,Q67A,
                         Q66,Q142,Q145,Q153,Q213,Q297F,Q299A,Q299F,Q377,#,Q160,Q152,Q67A,Q191K,Q139,Q323,
                         Q493BH,Q495BH,Q511,Q880) %>% #Q511,Q565,Q673,Q700,Q493BM
  mutate(Q67=ifelse(Q67A==1,1,0)) %>% mutate(C1=ifelse(Q57==1,1,0))%>% 
  mutate(C2=ifelse(Q57==2,1,0)) %>% mutate(C3=ifelse(Q57==3,1,0))%>% mutate(daisotu1=ifelse(Q60SR==6|Q60SR==7,1,0)) %>% 
  mutate(daisotu2=ifelse(Q61SR==6|Q61SR==7,1,0)) %>% mutate(nearoya=ifelse(Q377<=4,1,0)) %>% 
  mutate(ottoincome=ifelse(Q66<=2,0,1)) %>% mutate(honninsyuugyou=ifelse(Q142==1,1,0)) %>% 
  mutate(ottosyuugyou=ifelse(Q213==1,1,0)) %>% mutate(income=ifelse(Q297F==9999,NA,Q297F/100)) %>% 
  mutate(zehi=ifelse(Q511==1,1,0)) %>% mutate(jouken=ifelse(Q511==2,1,0)) %>% 
  mutate(daikigyo=ifelse(Q145==7,1,0)) %>% mutate(koumuin=ifelse(Q145==8,1,0)) %>% 
  mutate(parttime=ifelse(Q142!=1,0,1)) %>% mutate(ikukyu=ifelse(Q880<=3,1,0))

# data.s <- na.omit(datas)


PANELf <- datas %>% select(ID,PANEL,Q67) %>% rename(ID1=ID,panelf=PANEL)

datas <- datas %>% select(-Q67A,-Q67) %>% filter(Q142==1)#有業者のみに絞る

# data.f <- PANELf %>% dplyr::inner_join(datas,by=c("PANEL"="PANEL1","ID"="ID"))
# merge(PANEL1,datas,by=c("PANEL"="PANEL1","ID"="ID"))
data.f <- sqldf("SELECT * FROM PANELf INNER JOIN datas ON PANELf.panelf=datas.panelf AND PANELf.ID1=datas.ID")


###05年前後での分割
data.1 <- data.f %>% filter(PANEL>=14) %>% mutate(set=1) 

data.2 <- data.f %>% filter(PANEL<14) %>% mutate(set=2)

result.1 <- glm(Q67 ~#+Q153+Q297F+Q299F+Q299AQ323+Q152+Q160+Q139+Q191+Q377+Q57+Q60SR+Q61SR+
                  tokai+daisotu1+daisotu2+C1+C2+C3+nearoya+income+ottoincome+#Q60SR+Q61SR+Q57+Q66+
                  honninsyuugyou+ottosyuugyou+honninhatarakujikan+ottohatarakujikan+zehi+jouken+ikukyu+parttime, data=data.1, family=binomial(link=logit))#Q511+Q565+Q673+Q700+
summary(result.1)
result.2 <-  glm(Q67 ~ #+Q153+Q297F+Q299F+Q299AQ323+Q152+Q160+Q139+Q191+Q377+Q57+Q60SR+Q61SR+
                   tokai+daisotu1+daisotu2+C1+C2+C3+nearoya+income+ottoincome+#Q60SR+Q61SR+Q57+Q66+
                   honninsyuugyou+ottosyuugyou+honninhatarakujikan+ottohatarakujikan+zehi+jouken+ikukyu+parttime, data=data.2, family=binomial(link=logit))#Q511+Q565+Q673+Q700+
summary(result.2)



# data.1 <- datas %>% filter(PANEL>=14) %>% mutate(set=1)
# data.2 <- datas %>% filter(PANEL<14) %>% mutate(set=2)

# pro.f <- na.omit(data.s)



 ######functions######
 expit = function(x){
 	rr = exp(x)/(1+exp(x))
	return(rr)
 }

logit = function(x){
	rr = log(x/(1-x))
	return(rr)
}
 
IPW = function(n,p,d,y){
 	numerator = sum(d*y/p)
 	denominator = sum(d/p)
 	r = numerator/denominator
 	return(r)
 }
#############
set.seed(20160619)


