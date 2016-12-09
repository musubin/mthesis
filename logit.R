library(dplyr)
library(sqldf)
library(magrittr)
library(data.table)
library(foreach)
library(readr)

###多重代入法によるロジット
d1 <- data.f %>% filter(PANEL<14) %>% dplyr::select(-ID1,-PANELf,-panelf,-Q57,-Q377,-Q142,
                                                    -Q66,-Q297F,-Q299A,-Q299F,-Q61SR,-Q60SR,-Q66,-Q213,-Q3,-Q493BH,-Q495BH,-Q880)%>% 
  filter(Q67==0|Q67==1) #,-Q67A%>% mutate(set=1) 
d2 <- data.f %>% filter(PANEL>=14) %>% dplyr::select(-ID1,-PANELf,-panelf,-Q57,-Q377,-Q142,
  -Q66,-Q297F,-Q299A,-Q299F,-Q61SR,-Q60SR,-Q66,-Q213,-Q3,-Q493BH,-Q495BH,-Q880)%>% filter(Q67==0|Q67==1)#-ID,-Q67A,%>% mutate(set=2) 
dmi <- rbind(d1,d2)

write.csv(d1,"d1.csv")
write.csv(d2,"d2.csv")

###05年前後での分割
data.1 <- datas %>% filter(PANEL>=14) %>% mutate(set=1) 
data.2 <- datas %>% filter(PANEL<14) %>% mutate(set=2)

result.1 <- glm(Q67 ~ #+Q153+Q297F+Q299F+Q299AQ323+Q152+Q160+Q139+Q191+Q377+Q57+Q60SR+Q61SR+
              tokai+daisotu1+daisotu2+C1+C2+C3+nearoya+income+ottoincome+#Q60SR+Q61SR+Q57+Q66+
                honninsyuugyou+ottosyuugyou+honninhatarakujikan+ottohatarakujikan+zehi+jouken+ikukyu+parttime, 
              data=data.1, family=binomial(link=logit))#Q511+Q565+Q673+Q700+
summary(result.1)
result.2 <-  glm(Q67 ~ #+Q153+Q297F+Q299F+Q299AQ323+Q152+Q160+Q139+Q191+Q377+Q57+Q60SR+Q61SR+
                   tokai+daisotu1+daisotu2+C1+C2+C3+nearoya+income+ottoincome+#Q60SR+Q61SR+Q57+Q66+
                   honninsyuugyou+ottosyuugyou+honninhatarakujikan+ottohatarakujikan+zehi+jouken+ikukyu+parttime, 
                 data=data.2, family=binomial(link=logit))#Q511+Q565+Q673+Q700+
summary(result.2)


library(mice)
library(stargazer)
# imp_data<-mice::mice(nhanes, seed =1, m =20)
# fit <-with(imp_data, lm(chl~age +bmi))
# summary(pool(fit))
set.seed(334)

tmp1 <- pool(with(mice(d1),
glm(Q67 ~ #+Q153+Q297F+Q299F+Q299AQ323+Q152+Q160+Q139+Q191+Q377+Q57+Q60SR+Q61SR+Q66+
      tokai+daisotu1+daisotu2+C1+C2+C3+nearoya+income+ottoincome+#Q60SR+Q61SR+Q57+
      honninsyuugyou+ottosyuugyou+honninhatarakujikan+ottohatarakujikan+zehi+jouken+ikukyu+parttime, 
    family=binomial(link=logit))))
summary(tmp1)

tmp2 <- pool(with(mice(d2),
        glm(Q67 ~ #+Q153+Q297F+Q299F+Q299AQ323+Q152+Q160+Q139+Q191+Q377+Q57+Q60SR+Q61SR+Q66+
              tokai+daisotu1+daisotu2+C1+C2+C3+nearoya+income+ottoincome+#Q60SR+Q61SR+Q57+
              honninsyuugyou+ottosyuugyou+honninhatarakujikan+ottohatarakujikan+zehi+jouken+ikukyu+parttime, 
                      family=binomial(link=logit))))
summary(tmp2)



stargazer(tmp1,tmp2, title="多重代入法によるLogistic回帰", omit.stat=c("f","ser"), align=T, no.space=T)

##傾向スコア
setwd("/resources/data/panel/csv")
lf.2 <- list.files(full.names = F, pattern="csv", recursive = FALSE)
# lf <- lf[2:25]
# lf.2 <- lf.2[c(5:13,15:17)]

propensity <- data.frame()
for(i in 1:length(lf.2)){
  add <- fread(lf.2[i]) %>% select(ID,PANEL,Q142,Q145,Q297F,Q511,Q880)
  propensity <- add %>% bind_rows(propensity,add)
}

###変数処理
pro.f <- propensity %>% mutate(income=ifelse(Q297F==9999,NA,Q297F/100)) %>% 
  mutate(zehi=ifelse(Q511==1,1,0)) %>% mutate(jouken=ifelse(Q511==2,1,0)) %>% 
  mutate(daikigyo=ifelse(Q145==7,1,0)) %>% mutate(koumuin=ifelse(Q145==8,1,0)) %>% 
  mutate(parttime=ifelse(Q142!=1,0,1)) %>% mutate(ikukyu=ifelse(Q880<=3,1,0))

# data.1 <- datas %>% filter(PANEL>=14) %>% mutate(set=1)
# data.2 <- datas %>% filter(PANEL<14) %>% mutate(set=2)

pro.f <- na.omit(pro.f)
result.p <- glm(ikukyu~zehi+jouken+daikigyo+koumuin+parttime , data=pro.f, 
                family=binomial(link=logit))#Q511+Q565+Q673+Q700+
summary(result.p)

#c統計量
ret <- lrm(ikukyu~zehi+jouken+daikigyo+koumuin+parttime, data=pro.f)

ret$stats[6]

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
ret <- ipwe(data.1, "result.1$Q67", "ikukyu", ps)
ret <- ipwe(data.1, "re78", "treat", ps)

dre <- function(data, target, treat, ps, formula) {
  n       <- nrow(data)
  y       <- data[target]
  data1   <- data[data[treat]==1,]
  data0   <- data[data[treat]==0,]
  model1  <- lm(formula=formula, data=data1)
  model0  <- lm(formula=formula, data=data0)
  fitted1 <- predict(model1, data)
  fitted0 <- predict(model0, data)
  dre1    <- (1/n)*sum(y+((z-ps)/ps)*(y-fitted1))
  dre0    <- (1/n)*sum(((1-z)*y)/(1-ps)+(1-(1-z)/(1-ps))*fitted0)
  return(c(dre1, dre0))
}

ret <- dre(lalonde, "re78", "treat", ps,
           re78~age+educ+black+hisp+married+nodegr+re74+re75+u74+u75)

# download.file("https://cran.r-project.org/src/contrib/Archive/CBPS/CBPS_0.8.tar.gz",destfile="CBPS_0.8.tar.gz")
# library(CBPS)
# ps <- result.p$fitted
# ipwe.diff <- CBPS::IPW(outcome=result.1$Q67, treat=ikukyu, data=data.1, pscore=ps,
#                  k = length(coef(result.1)))
# dre.diff <- DR(Q67 ~ Q57+Q60SR+Q61SR+
#                  Q66+Q142+Q153+Q213+Q297A+Q297F+Q299A+Q299F+Q377+#Q323+Q152+Q160+Q139+Q191+
#                  Q493BH+Q493BM+Q495BH,
#                model="logit", data=pro.f, treat=ikukyu, pscore=ps)

# dre.diff <- DR(re78~age+educ+black+hisp+married+nodegr+re74+re75+u74+u75,
#                model="lm", data=lalonde, treat=treat, pscore=ps)

#################
name_list <- list()
df_list <- list()
i <- 1
for(year in files){
  # df <- readr::read_csv(year)
  df <- readr::read_csv(year,col_types = cols("Q11" = col_character(),
                                              "Q12" = col_character()))
  df <- df %>% select(-X1)
  name_list[[i]] <- names(df)
  df_list[[i]] <- df
  i = i + 1
}
df_all <- bind_rows(df_list)
common <- Reduce(function(a,b) dplyr::intersect(a,b), name_list)
df_common <- df_all %>%
  dplyr::select_(.dots = common)


# for(year in files){
#   
#   # *********** a.csvを生成 *************
#   df <- data_frame(year = year,
#                    Q_common1 = "Q_common1",
#                    Q_common2 = "Q_common2",
#                    c = paste0("Q", year)) %>%
#     setNames(c("year", "Q_common1", "Q_common2", paste0("Q", year)))
#   write.csv(df, year)
#   # *************************************
#   
#   df <- fread(year)
#   
#   name_list[[i]] <- names(df)
#   df_list[[i]] <- df
#   i <- i+1
# }
# df_all <- bind_rows(df_list)
# common <- Reduce(function(a,b) dplyr::intersect(a,b), name_list)
# df_common <- df_all %>%
#   dplyr::select_(.dots = common)

#全データ使用
# setwd("/resources/data/panel/csv")
setwd("C:\\Users\\KT\\Desktop\\panel\\userdata")
files <- list.files(full.names = F, pattern="csv", recursive = FALSE)

###カラムチェック
ans <- list()
for (i in 1:25){ 
  ans[[i]] <- "Q152" %in% name_list[[i]]
}

for (i in 1:25){ 
  ans[[i]] <- "Q67A" %in% name_list[[i]]
}