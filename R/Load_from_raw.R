##sudo ping -i .001 -w 900 www.google.com  | while read pong; do echo "$(date +"%T.%N"): $pong"; done > first_ping_test5.txt
#######################
###load data
#######################
rm(list=ls())
options(max.print=999999)
library(pacman)
p_load(here)
p_load(openxlsx)
p_load(tidyverse)
library(bueri)
oldir<-getwd()
output_path<-"../outputs"

####see available xlsx files to load
list.files("../data/raw")%>%str_subset(".txt")
###asign the apropiate name file. without xlsx extencion
file_nm<-"first_ping_test6.txt"
###load file

z0<-readLines( paste0("../data/raw/",file_nm))
head(z0)
tail(z0)

z0<-z0[-c(1,(length(z0)-3):length(z0))]
z0%>%str_split_fixed(":",5)->z1
head(z1)
tail(z1)
z1[,4]%>%unique
z1<-z1[,-4]

unformated_ts<-z1[,4]%>%str_split_fixed(" ",5)
head(unformated_ts)
tail(unformated_ts)
unformated_ts<-unformated_ts[,-c(1,3,5)]
unformated_ts[,2]%>%str_remove("time=")->unformated_ts[,2]
unformated_ts[,1]%>%str_remove("icmp_seq=")->unformated_ts[,1]
class(unformated_ts) <- "numeric"


z2<-data.frame(hour=as.numeric(z1[,1]),minute=as.numeric(z1[,2]),seconds=as.numeric(z1[,3]),
               icmp_seq=unformated_ts[,1],ping=unformated_ts[,2])
z2$time_sc<-z2$hour*3600+z2$minute*60+z2$seconds
z2$full_time<-apply(z2[,1:3],1,paste,collapse=":")
z2$time_sc_rel<-z2$time_sc-min(z2$time_sc)
head(z2)
tail(z2)
treshld_píng<-mean(z2$ping)+2*sd(z2$ping)
pdf(file=paste0(output_path,"/","ping_series_2.pdf"),width=17,height=5)
z2%>%ggplot(aes(x=time_sc_rel,y=ping))+geom_line()+
  scale_x_continuous(name ="minutos",breaks = (0:15)*60,labels =0:15)+theme_bw()+
  geom_hline(yintercept=treshld_píng,linetype="dashed", color = "red")
dev.off()

summary(z2)

z2[z2$ping>treshld_píng,]%>%nrow()%>%{.*100/nrow(z2)}
z2[z2$ping>treshld_píng,]%>%nrow()%>%{.*100/(max(z2$time_sc_rel)/60)}
