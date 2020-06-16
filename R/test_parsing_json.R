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
z0<-stream_in(file("speedtest_2020_06_16__1611.txt"))

z0$download*0.125/100000
