library(VIM)
matrixplot(dmi)
m <-aggr(dmi)
m
library(Amelia)
library(Zelig)
set.seed(334)

#amelia()関数をまずは使います
#引数は以下のとおり
#x = data.frame
#m = 何個のデータセットを作るかの指定
#dat1.outという変数に，多重代入した結果を入れます


dmi.1 <-amelia(x = d1,m = 5)

#中身はsummaryで確認します
summary(dmi.1)

lmi.1 <- zelig(Q67 ~ #+Q153+Q66+Q297F+Q299F+Q299AQ323+Q152+Q160+Q139+Q191+Q377+Q57+Q60SR+Q61SR+
        tokai+daisotu1+daisotu2+C1+C2+C3+nearoya+ottoincome+honninsyuugyou+ottosyuugyou+honninhatarakujikan+ottohatarakujikan,
      data = dmi.1, model = "logit")# ,robust = TRUE)
summary(lmi.1)

dmi.2 <-amelia(x = d2,m = 5)

#中身はsummaryで確認します
summary(dmi.2)

lmi2 <- zelig(Q67 ~ #+Q153+Q66+Q297F+Q299F+Q299AQ323+Q152+Q160+Q139+Q191+Q377+Q57+Q60SR+Q61SR+
                 tokai+daisotu1+daisotu2+C1+C2+C3+nearoya+ottoincome+honninsyuugyou+ottosyuugyou+honninhatarakujikan+ottohatarakujikan,
               data = dmi.2, model = "logit")# ,robust = TRUE)

extract.zelig(lmi2)

( xtable.descript <- xtable(descriptive.tab,caption="xtableによる記述統計表", label="descriptive.xtable", align=c("l",rep("r",5) ), 
                            digits=c(0,0,3,3,3,3), display=c("s","d",rep("f",4) ) ) )
print(xtable.descript, file="xtable.descript.tex")

require(stargazer)
require(texreg)
stargazer(lmi.1,lmi2, title="多重代入法によるLogistic回帰", omit.stat=c("f","ser"), align=T, no.space=T)
models <-list(lmi.1,lmi2) 
texreg(l=models,file="texreg.tex", caption="texregを用いた場合", 
       digits=3, booktabs=T, dcolumn=T, center=T, use.packages=F, caption.above=T, custom.model.names=c("OLS 1", "OLS 2"))
htmlreg(l=models,file="htmlreg.html", caption="htmlregを用いた場合", 
        digits=3, booktabs=T, dcolumn=T, center=T, use.packages=F, html.tag=T, head.tag=T, body.tag=T)
screenreg(l=models)



if(!require(mi)){
  install.packages("mi", dependencies=TRUE)
}
library(mi)

# データの読み込みと不要変数の削除
# train <- read.table('~/Documents/workspace/R/kaggle/data/titanic/train.csv', header=T, sep=',')
setwd("/resources/data/panel/csv")
train <- fread("cfall.csv")
train.mi <- train
# train.mi$name <- NULL
# train.mi$ticket <- NULL
# train.mi$cabin <- NULL

# NAデータのプロット
mp.plot(train.mi, clustered=FALSE) 

# データの情報取得と確認
train.info <- mi.info(train.mi)
train.info$imp.formula

# 欠損値補完の前処理と確認
train.pre <- mi.preprocess(train.mi)
attr(train.pre, 'mi.info')

# 欠損値補完とデータの取得
train.imp <- mi(train.pre, R.hat=1.8)
train.dat.all <- mi.completed(train.imp)

train.dat <- mi.data.frame(train.imp, m=1)

library(mice)
imp_data<-mice::mice(nhanes, seed =1, m =20)
fit <-with(imp_data, lm(chl~age +bmi))
summary(pool(fit))

tmp <- pool(with(mice(datas),model))

# 各imputed data にlmを当てはめて、推計値と標準偏差を引っ張る。
model_Estimate<-function(x){
  coef(summary(lm(data =x, formula =chl~age +bmi)))[, "Estimate"]
}
model_inSD<-function(x){
  coef(summary(lm(data =x, formula =chl~age +bmi)))[, "Std. Error"]
}
# complete 関数からimputed data をリストにいったんまとめる。
comp_list<-list()
for(i in 1:20){
  comp_list[[i]]<-complete(imp_data, i)
}
# EstimateとSDのマトリックスを吐く
Estimate_Matrix<-sapply(comp_list, model_Estimate)
SD_matrix<-sapply(comp_list, model_inSD)
# Rubin's Ruleを適応し統合
M <-20
estimated_beta<-mean(Estimate_Matrix["age", ])
estimated_beta_V<-mean(SD_matrix["age", ]^2)
estimated_beta_V_2 <-(1+(1/M))*(sum(((Estimate_Matrix["age", ]-estimated_beta)^2)/(M -1)))
# age の推計量とSDを表示
c(estimated_beta, (estimated_beta_V+estimated_beta_V_2)^(1/2))

