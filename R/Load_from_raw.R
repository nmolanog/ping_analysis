##sudo timeout 1800s ping -i .001 www.google.com  | while read pong; do echo "$(date +"%T.%N"): $pong"; done > "test_$(date +"%Y_%m_%d__%H%M").txt"
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
library(bueri)
oldir<-getwd()
output_path<-"../outputs"

####see available xlsx files to load
list.files("../data/raw")%>%str_subset(".txt")
###asign the apropiate name file. without xlsx extencion
test_name<-"test_2020_06_13__1304"
file_nm<-paste0(test_name,".txt")
###load file

z0<-readLines( paste0("../data/raw/",file_nm))
main_title<-z0[1]
#main_end<-z0[length(z0)-1]
head(z0)
tail(z0)

z_info<-z0[c(1,length(z0)-1)]
#z0<-z0[-c(1,(length(z0)-3):length(z0))]
z0<-z0[-1]
z0%>%str_split_fixed(":",5)->z1
head(z1)
tail(z1)
z1[,4]%>%unique

#z1[z1[,4] %>%str_detect("Unreachable"),]
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
treshld_píng<-round(mean(z2$ping,na.rm = T)+2*sd(z2$ping,na.rm = T),2)
my_breaks <- c(seq(0, max(z2$ping,na.rm = T), length.out = 5),treshld_píng)
my_labels <- as.character(my_breaks)
last_min<-(z2$time_sc_rel/60)%>%max%>%ceiling()
z2$min<-(z2$time_sc_rel/60)%>%cut(breaks=c(-Inf,1:last_min),labels = 0:(last_min-1))

ping_anom_count<-z2[z2$ping>treshld_píng,"min"]%>%table%>%{data.frame(.)}
colnames(ping_anom_count)<-c("minuto","anomalos")

p1<-z2%>%ggplot(aes(x=time_sc_rel,y=ping))+geom_line()+
scale_x_continuous(name ="minutos",breaks = (0:last_min)*60,labels =0:last_min)+theme_bw()+
geom_hline(yintercept=treshld_píng,linetype="dashed", color = "red")+
scale_y_continuous(limits = c(0, max(z2$ping)), breaks = my_breaks, labels = my_labels,
                   name = "ping (ms)")+ ggtitle('ping en tiempo real')
p2<-z2%>%ggplot(aes(x=ping))+geom_density()+theme_bw()+ ggtitle('distribucion del ping')

p3<-tableGrob(data.frame(stat=c("min","Q1","median","mean","Q3","max","sd","NA"),
                        value=c(round(min(z2$ping,na.rm = T),3),
                                round(quantile(z2$ping,0.25,na.rm = T),3),
                                round(quantile(z2$ping,0.5,na.rm = T),3),
                                round(mean(z2$ping,na.rm = T),3),
                                round(quantile(z2$ping,0.75,na.rm = T),3),
                                round(max(z2$ping,na.rm = T),3),
                                round(sd(z2$ping,na.rm = T),3),
                                sum(is.na(z2$ping)))),rows =NULL) 

p4<-ping_anom_count%>%ggplot(aes(x=minuto, y=anomalos)) +
  geom_bar(stat="identity")+theme_bw()+ ggtitle('numero de paquetes con ping mayor al humbral')

pdf(file=paste0(output_path,"/",test_name,".pdf"),width=12,height=6)
grid.arrange(
  grobs = list(p1,p2,p4,p3),
  widths = c(1,1,1),
  layout_matrix = rbind(c(1,1,1),
                        c(2,3,4)),
  top=z_info[1]
)
dev.off()


summary(z2)

z2[z2$ping>treshld_píng,]%>%nrow()%>%{.*100/nrow(z2)}
z2[z2$ping>treshld_píng,]%>%nrow()%>%{.*100/(max(z2$time_sc_rel)/60)}

