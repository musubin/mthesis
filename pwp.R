library(tidyverse)
library(sqldf)
library(data.table)
library(foreach)

library(survival)


# cfall.p <- cfall.f[,c(1:3,5:41,4,42:61)]

pwp <- cfall.p %>% 
  #melt(id.var=c("ID"), value.name = "end") %>%         # long形式に展開、値はend列として使用
  tidyr::gather("id","end",strata1:Q8) %>% 
  arrange(ID, end) %>% group_by(ID) %>%                # id & end値を基準にソート、idでグループ化
  do(mutate(., start = lag(.$end, n=1, default=0),     # group毎にendをひとつずらしてstartを作成
            count = 1:nrow(.))) %>%  ungroup() %>%               　# またgroup内で何行目かをcountに使用
  # na.omit() %>%                         　# naを含む行の削除、グループ解除
  mutate(event = ifelse(id=="Q8", 0, 1))　# nowを使用しているか否かでevent値を算出
  # select(-variable) 

pwp <- pwp %>% mutate(diff=end-start) %>% mutate(group=ifelse()) %>% 
  group_by(ID,start)

before.2 <- cfall.f %>% filter(year2 > PANEL-14) %>% mutate(fyear = Q8-year2) %>% mutate(Set=1) %>% mutate(cens=1)
after.2 <- cfall.f %>% filter(PANEL>=14,PANEL-year2>=14) %>% mutate(fyear = Q8-year2) %>% 
  mutate(Set=2) %>% mutate(cens=1)

pwp.f <- coxph(Surv(start,end,event) ~ GROUP + cluster(ID) + strata(diff), data=pwp)

pwp.b <- coxph(Surv(start,end,event) ~ GROUP + cluster(ID) + strata(diff), data=pwp)
pwp.a <- coxph(Surv(start,end,event) ~ GROUP + cluster(ID) + strata(diff), data=pwp)
