library(dplyr)
library(sqldf)
library(magrittr)
library(data.table)
library(RSQLite)
library(survival)
library(MASS)
library(ggplot2)

d.survfit <- survfit(Surv(strata1,cens)~Set2,data=cfall)

fortify.survfit <- function(survfit.data) {
  data.frame(time = survfit.data$time,
             n.risk = survfit.data$n.risk,
             n.event = survfit.data$n.event,
             n.censor = survfit.data$n.censor,
             surv = survfit.data$surv,
             std.err = survfit.data$std.err,
             upper = survfit.data$upper,
             lower = survfit.data$lower,
             strata = rep(names(survfit.data$strata), survfit.data$strata))
}


###kaplan plot
ggplot(data = surv.data) +
  geom_line(aes_string(x = 'time', y = 'surv', colour = 'strata')) +
  geom_ribbon(aes_string(x = 'time', ymin = 'lower', ymax = 'upper', fill = 'strata'), alpha = 0.5) + 
  scale_y_continuous(labels = scales::percent)
##dummy

dummy <- function(i){
  i <- i %>% mutate(wgakureki=as.numeric(ifelse(Q60SR==6||Q60SR==7,1,0))) %>% 
    mutate(hgakureki=as.numeric(ifelse(Q61SR==6||Q61SR==7,1,0))) %>% 
    mutate(oyadoukyo=as.numeric(ifelse(Q377>=5,1,0))) %>% 
    mutate(tokai=as.numeric(ifelse(Q3==1,1,0))) %>% 
    mutate(kodomohosii=as.numeric(Q511==1||Q511==2,1,0))
}


###打ち切りかそうでないか
# cfall <- cfall %>% filter(PANEL==21)%>% mutate(cens=1)
# cfall <- cfall %>% filter(Q8 <= 45) %>% filter(PANEL!=21)%>% mutate(cens=0)
cfall <- cfall %>% mutate(cens=ifelse(Q8 <= 45 & PANEL!=21,0,1))


###05年より前か後か1st
# cfall <- cfall %>% as.numeric(year) %>% as.numeric(PANEL)
#cfall <- cfall %>% mutate(Set=ifelse(year2==NA,NA,ifelse(PANEL>=14|PANEL-year2>=14,2,1)))
cf3 <- cf3 %>% mutate(Set=ifelse(PANEL>=14|PANEL-year2>=14,2,1))

# before <- cfall %>% filter(year > PANEL-14) %>% mutate(Set=1) 

# after <- cfall %>% filter(PANEL>=14,PANEL-year>=14) %>% mutate(Set=2) 

#datafunc
# d.survfit <- survival::survfit(survival::Surv(time, status) ~ sex,data = lung)

# ggplot(data = d.survfit) +
#   geom_line(aes_string(x = 'time', y = 'surv', colour = 'strata')) +
#   geom_ribbon(aes_string(x = 'time', ymin = 'lower', ymax = 'upper', fill = 'strata'), alpha = 0.5) + 
#   scale_y_continuous(labels = scales::percent)

# data1st
surv.data <- survfit(Surv(strata1,cens)~Set2,data=cfall)
plot(surv.data,lty=1:2,main="1st Child",xlab="year",conf.int = F,xlim=c(10,50))
legend(locator(1),c("before","after"),lty=c(1,2))#maususitei
#test
survdiff(Surv(strata1)~Set2,data=cfall)
#cox
# cox1 <- coxph(Surv(strata1,cens)~Set2+Q493BH+Q297A,data=cfall)
cox1 <- coxph(Surv(strata1,cens)~Set2+Q57+Q60+Q61+
  Q66+Q142+Q152+Q153+Q160+Q213+Q297A+Q297F+Q299A+Q299F+Q377+#+Q139+Q191
  Q493BH+Q493BM+Q495BH+Q511+Q673+Q700,data=cfall)#+Q565
summary(cox1)




# data2nd
surv.data <- survfit(Surv(strata1,cens)~Set2,data=cfall)
plot(surv.data,lty=1:2,main="2nd Child",xlab="year",conf.int = F,xlim=c(10,50))
legend(locator(1),c("before","after"),lty=c(1,2))#maususitei
#test
survdiff(Surv(strata1)~Set2,data=cfall)
#cox
cox1 <- coxph(Surv(strata1,cens)~Set2+Q493BH+Q297A,data=cfall)
summary(cox1)





#geom_histogram()


histgg <- function(i){
  ggplot(data=cfall, aes(x=paste0("strata",i)))+
    geom_histogram(fill="blue", alpha=0.5)+
    #stat_count(width = 0.5)+
    #geom_histogram(binwidth=0.1)+
    labs(title=paste0("child",i))
    #ggsave(paste0("hist",i,".jpg"))
  #geom_histogram(aes(x=x), binwidth=0.1, alpha=0.5)+xlim(0,10)+
  #geom_text(aes(x=x),label=sprintf("%2.1f", x), vjust=1.4) + 
  #coord_flip()
}

histgg(3)

ggplot(data=cfall, aes(x=strata1))+
  geom_histogram(fill="blue", alpha=0.8)+
  labs(title="Child1")+
  ggsave("hist1.jpg")

ggplot(data=cfall, aes(x=strata2))+
  geom_histogram(fill="blue", binwidth=1, alpha=0.8)+
  #geom_histogram()+,binwidth=0.2
  labs(title="child2")+
  ggsave("hist2.jpg")

ggplot(data=cfall, aes(x=strata3))+
  geom_histogram(fill="blue", binwidth=1, alpha=0.8)+
  #geom_histogram()+,binwidth=0.2
  labs(title="child3")+
  ggsave("hist3.jpg")

data = diamonds

# 1. fill=blueとしているのにぜんぜん違う色になり、redなどにしても変わらない。
# aes()内のでfillに変数を指定すると、指定した変数に応じて色分けして、となります。
# この場合、「"blue"ごとに色分けして」という指示ですが、blueという名前のカラムはないので...という感じです。

# 以下で色が変えられます。
ggplot(data = diamonds, aes(x = x)) +
  geom_histogram(fill = "blue")

# 2. ラベル（各階級の頻度数）が表示されない
# 3. ヒストグラムの階級幅をアルゴリズム（Scottなど）で選びたいがggplotになるとどうすればよいかわからない。

# 2. に関しては
# 元のコードのgeom_text(aes(x=x),label=sprintf("%2.1f", x))の
# "label = "部で取得しているのは、度数ではなく、diamonds$xです。
# エラーがでない状態にしても意図された通りには表示されません。

diamonds %>%
  head(100) %>% # 全部書くと重いので一部だけ取り出す
  ggplot(data = ., aes(x = x)) +
  geom_histogram(fill = "blue", binwidth = 0.1, alpha = 0.5) +
  geom_text(aes(x = x, y = y, label = sprintf("%2.1f", x)))

# geom_histogramで書けるのでしょうか...。
# 一応、以下でいけますが...。

# binをとってくる
x_bins <-
  cfall$strata1 %>%
  hist %>% # 任意のアルゴリズムを指定 3. を解決
  .[["breaks"]] %>%
  stats::filter(., c(1/2, 1/2)) %>% # 各binの中央をとってくる
  na.omit %>%
  as.vector # TimeSeriesを単なるベクトルに戻す

# 度数をとってくる
x_counts <-
  cfall$strata1 %>%
  hist %>%
  .[["counts"]]

data.frame(x = x_bins, y = x_counts) %>%
  ggplot(data = ., aes(x = x, y = y)) +
  geom_bar(stat = "identity", fill = "pink") +
  geom_text(aes(x = x, y = y + 300, label = sprintf("%2.0f", y)))

val <- diff(range(cfall$strata1))/nclass.scott(cfall$strata1)
ggplot(cfall, aes(x)) + geom_histogram(fill = "blue", binwidth=val)


#######################

####bad####
ggplot(data) + 
  geom_bar(aes(x=variable, y=all, fill=ustanova), position="dodge", stat="identity") +
  geom_text(aes(x=variable, y=all, label=sprintf("%2.1f", all))) + 
  coord_flip()

####good####
ggplot(cfall$strata1) + 
  geom_bar(aes(x=variable, y=all, fill=ustanova), stat="identity", position="dodge") +
  geom_text(aes(x=variable, y=all, group=ustanova, label=sprintf("%2.1f", all), hjust=1.4), 
            position=position_dodge(width=0.9)) 
#xとy交代
coord_flip()

# 基本的には，どんどんgeom_XXXを+でつないでいけばよいだけ
## data=XXXと書くことで，2個目以降のグラフに違うデータフレームを使うことも可能

hist(data$x,breaks="Scott",label=T,main="diamond$x")
hist(data$x,breaks=nclass.Sturges(data$x),label=T)
nclass.Sturges(data$x)

library(data.table)
setwd("C:\\Users\\KT\\Desktop\\R")
new <- fread("new.csv",header=TRUE)
plot(new[,2],new[,3],type="n")
text(new[,2],new[,3],new.label)

colnames(new) <- c("pre","income08","mitsudo08")
library(maptools)
new <- as.data.frame(new)
plot(new[,2],new[,3],type="n")
pointLabel(x=new[,2], y=new[,3], labels=new$pre)

dq1 %>% 
  SparkR::select("depth","mag","stations") %>%
  SparkR::filter(dq1$mag > 5.5) %>%
  SparkR::collect()