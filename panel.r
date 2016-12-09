library(tidyverse)
library(sqldf)
library(data.table)
library(foreach)


setwd("C:\\Users\\KT\\Desktop\\panel\\userdata")#\\csv\\p1")
dir <- list.dirs("csv", full.names = F, recursive = FALSE)
# dir2 <- substr(dir,5,15)
#all2 <- list.files("csv", full.names = TRUE,recursive = T) %>% lapply(fread) %>% bind_rows()

all <- function(i){
  setwd(paste0("C:\\Users\\KT\\Desktop\\panel\\userdata\\csv\\"))#,dir2[[i]]))
  #rm(result)
  gc()
  result <- list()
  result[[paste0(dir[[i]])]] <-list.files(dir[[i]], full.names = TRUE) %>% lapply(fread) %>% bind_cols()
  # result[[paste0(dir2[[i]])]] <-list.files(dir2[[i]], full.names = TRUE) %>% lapply(fread) %>% inner_join(by="ID")
  result <- as.data.frame(result) %>% select(-contains("ID.",ignore.case=TRUE)) #%>% select(-1)
  # result <- gsub("^p*\.","",result)
  # result <- gsub("p.*?\.","",result)
  clname <- colnames(result)
  clname <- gsub("p.*?\\.","",clname)
  clname <- gsub("\\.","_",clname)
  # clname <- select(-panel.1)
  names(result) <- clname
  # result[is.na(result)]<-"NaN"
  write.csv(result,paste0(dir[[i]],".csv"))
}

all(23)

lapply(1:24, all)

#prepro <- function(drange,variables,childnum){
prepro <- function(variables,childnum){  
  setwd("C:\\Users\\KT\\Desktop\\panel\\userdata")#\\csv\\p1")
  ld.names <- list.dirs("csv", full.names = F, recursive = FALSE) #%>% as.list()
  ld.files <- list.files("csv", full.names = F, pattern="csv",recursive = FALSE) #%>% as.list()

  reading <- function(x){
    setwd("C:\\Users\\KT\\Desktop\\panel\\userdata\\csv")
    # child<- fread(ld.files[[x]]) %>% select_(variables)%>% 
    child<- fread(x) %>% select(one_of(variables))%>% 
    filter(Q11 == childnum|Q16 == childnum|Q21 == childnum|Q26 == childnum)%>% 
    mutate(num=childnum) %>%
    mutate(year = ifelse(Q11==childnum, Q13, ifelse(Q16==childnum, Q18, ifelse(Q21==childnum, Q23,
    ifelse(Q26==childnum,Q28,ifelse(Q31==childnum,Q33,NA))))))
    #mypath <- file.path(getwd(), paste("H", i, ".csv", sep = "")) 
    #assign(paste0("p", i), child)
    return(child)
  

 for (i in ld.names) {
  setwd("C:\\Users\\KT\\Desktop\\panel\\userdata\\csv")
  mypath <- file.path(getwd(), paste0(i,".csv"))
  # print(length(mypath))
  assign(i, reading(mypath))
 }
    }
  # all.7 <- rbind(eval(parse(text=paste0(ld.names,collapse=",")))) #%>% as.data.frame()
  all.7 <- rbind(eval(parse(text=ld.names))) %>% arrange(ID,PANEL) %>%  group_by(ID) %>%  filter(PANEL==max(PANEL))
  #all.7 <- do.call(rbind, lapply(drange,reading))
  return(all.7)
#   all <- lapply(ld.files, function(x) eval(parse(text = x))) %>% 
#     bind_rows() %>% as.data.frame()
#   return(all)
}
child2 <- prepro(c("ID","REC","PANEL","Q8","Q11","Q13","Q16","Q18","Q21","Q23","Q26","Q28","Q31","Q33","Q493BH","Q493BM","Q297A","A297F",
               "Q299A","Q299F"), 2)

vrb <- quote(c("ID","PANEL","Q3","Q8","Q11","Q13","Q16","Q18","Q21","Q23","Q26","Q28","Q31","Q33","Q57","Q60","Q61",
                "Q66","Q139","Q142","Q152","Q153","Q160","Q191","Q213","Q297A","Q297F","Q299A","Q299F","Q323","Q377",
                "Q493BH","Q493BM","Q495BH","Q511","Q565","Q673","Q700","Q880","Q888A","Q888B","Q888C",
                "Q1065","Q1126","Q1129")) 

lapply(1:25,vrb)

sqldf("SELECT * FROM all where Q11")

write.csv(p20,"m20.csv")
write.csv(m21,"m21.csv")
# for(j in 8:10){
#    all(j)
# }
# 
# lapply(1, all)
# 
# all(1)

##experiment##
  setwd(paste0("C:\\Users\\KT\\Desktop\\panel\\userdata\\csv\\"))#,dir2[[1]]))
  result[[paste0("p",16)]] <-list.files(dir2[[16]], full.names = TRUE,recursive = T) %>% lapply(fread) %>% bind_cols()
result <- as.data.frame(result)
write.csv(result,"16.csv")
####
# ディレクトリ内のすべてのファイルを読み取ってマージする
read_csv_in_dir <- function(dir_name) {
  result <- list()
  result <- list.files(paste0("p",dir_name), full.names = TRUE) %>% lapply(fread) %>% bind_rows()
  assign(paste("csv_",dir_name) , result)
  #list.files(dir_name, full.names = TRUE) %>% lapply(read.csv) %>% bind_rows()
}
  
# それをすべてのディレクトリに対して適用する
list.files("csv", full.names = TRUE) %>% lapply(read_csv_in_dir)


# setwd("C:\\Users\\KT\\Desktop\\panel\\userdata\\csv\\p1")
# 
# p1_1 <- fread("p1_1.csv")

# paste0("i","J") <- 1
# 
# for(i in 1:20)
#  {
#   setwd("C:\\Users\\KT\\Desktop\\panel\\userdata\\csv\\p_+"i"")
#   for(j in 1:5){
#   p+"i"+_+"j" <- fread("p+"i"+_+"j"+.csv")
#   }
# }
#   setwd("C:\\Users\\KT\\Desktop\\panel\\userdata\\csv\\p_j")
#   paste("C:\\Users\\KT\\Desktop\\panel\\userdata\\csv\\p_",j)

setwd("C:\\Users\\KT\\Desktop\\panel\\userdata\\csv")
test <- fread("p1.csv")
clname <- colnames(test)
clname <- gsub("p.*?\\.","",clname)
names(test) <- clname

df_tidy <- newdata %>%
  tidyr::gather(key = item, value = value, select = -c(V1)) %>% 
  dplyr::mutate(year = tidyr::extract_numeric(item) + 2000,
                item = gsub("[^[:alpha:]]+", "", item))
df_tidy <- df_tidy %>%
  spread(item, value) %>%
  rename(都道府県 = V1, 年 = year, 収入 = income, 求人数 = kyuujin, 流入者数 = `in`, 流出者数 = out)

####trash####
imp <- function(lst){
  for (j in 1:25){
    void <- list()
    void[[j]] <- void[[j]] %>% select(vrb)
  }
}


setwd("C:\\Users\\KT\\Desktop\\panel\\userdata\\csv\\")
#p7-p21
p7 <- fread("p7.csv") %>% select(ID,REC,PANEL,Q8,Q11,Q13,Q16,Q18,Q21,Q23,Q26,Q28,Q31,Q33,Q493BH,Q493BM,Q297A,Q297F,
                                 Q299A,Q299F,Q565,Q888A,Q888B,Q888C)%>% filter(Q11 == 2|Q16 == 2|Q21 == 2|Q26 == 2)%>% 
  mutate(num=2) %>% mutate(year = ifelse(Q11==2, Q13, ifelse(Q16==2, Q18, ifelse(Q21==2, Q23,ifelse(Q26==2,Q28,ifelse(Q31==2,Q33,NA))))))
