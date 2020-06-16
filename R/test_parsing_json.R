#######################
###load data
#######################
rm(list=ls())
options(max.print=999999)
library(pacman)
p_load(here)
p_load(openxlsx)
p_load(tidyverse)
p_load(gridExtra)
p_load(jsonlite)

library(bueri)
list.files()%>%str_subset(".txt")
z0<-readLines("speedtest_output1.txt")
test<-z0%>%map(fromJSON,flatten=T)

test_df<-test%>%reduce(rbind)

test_df[1,"server"]
test_df[1,"client"]

test_df[["server"]]

test%>%map(length)
df1<-test%>%map(~unlist(.[setdiff(names(.),c("server","client"))]))%>%reduce(rbind,deparse.level = 0)
df2<-test%>%map(~.$server)%>%reduce(rbind,deparse.level = 0)
df3<-test%>%map(~.$client)%>%reduce(rbind,deparse.level = 0)

list_df<-list(df1,df2,df3)

full_df<-list_df%>%reduce(cbind)%>%data.frame()

full_df[,1]
full_df$download
full_df[,1:3]

#######################
###load data
#######################
rm(list=ls())
options(max.print=999999)
library(pacman)
p_load(here)
p_load(openxlsx)
p_load(tidyverse)
p_load(gridExtra)
p_load(jsonlite)

library(bueri)
list.files()%>%str_subset(".txt")
z0<-stream_in(file("speedtest_output1.txt"))

z0$download*0.125/100000
