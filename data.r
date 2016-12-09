library(dplyr)
library(sqldf)
library(magrittr)
library(data.table)
library(RSQLite)
library(survival)
library(MASS)
library(knitr)
setwd("C:\\Users\\KT\\Desktop\\panel\\zemi17")
Sys.setenv(RSTUDIO_PDFLATEX = "lualatex")
# knit2html("zemi16.Rnw")
knit2pdf("zemi17.Rnw",encoding = "UTF8",compiler = "lualatex")

library(psych)
library(stargazer)
stargazer(describeBy(before.2), out="descriptb.tex", title="2005年前の記述統計量", align=F, style="qje")#, omit="var"

describeBy(before.2)#, group=NULL,mat=FALSE,type=3,digits=15,…)
describeBy(after.2)
describeBy(d1)
describeBy(d2)


# % !Rnw weave = knitr
# % !TeX program = XeLaTeX

#' <<include=FALSE>>=
#'   library(knitr)
#' opts_knit$set(latex.options.graphicx = "dvipdfmx")
#' opts_knit$set(latex.options.color = "dvipdfmx")
#' opts_chunk$set(fig.path = "figure/ja-fig-platex-win-utf8-", fig.height=5, fig.width=5, fig.pos="!h", fig.align="center", echo=FALSE)
#' library(grid)
#' @ 
  
#' \if0{
#'   <<echo=FALSE>>=
#'     c1st <- rbind(cf,cf2,cf3)
#'     basic(c1st$strata1)
#'     
#'     c2nd <- rbind(cf,cf2)
#'     basic(c2nd$strata2)
#'     
#'     @
#'       \fi 


basic<-function(xx){
  N<-length(xx)
  Mean <- mean(xx)
  Min <- min(xx)
  Max <- max(xx)
  var1<-function(a) {var(a)*(length(a)-1)/length(a)} #標本分散
  Sd<-sqrt(var1(xx))                                #標本標準偏差
  t<-(xx-Mean)/Sd                                #標準化
  Sk<-mean(t^3)                                    #歪度 
  Ku<-mean(t^4)                                    #尖度 
  # hist(xx, breaks = "Scott",xlim=c(0,50))#main=paste0("Histogram of",xx),
  ds1 <-  cbind(N,Mean,Sd,Min,Max,Sk,Ku)           #まとめて
  return(ds1) 
}

#basic(c2$fyear)
c1st <- rbind(cf,cf2,cf3)
basic(c1st$strata1)

c2nd <- rbind(cf,cf2)
basic(c2nd$strata2)

basic(cf$strata3)

m <- mean(c23$year23)
v <- var1(c23$year23)
s <- sqrt(var1(c23$year23))


c23$year23 <- as.numeric(c23$year23)
class(c23$year23)
class(s)
class(v)
class(m)