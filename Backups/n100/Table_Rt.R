rm(list=ls(all=TRUE))
library(xtable)

load("Table_2_Rt_n100.Rdata")
xtable(Table_2_Rt_Power_n100,digits=3)
Table_2_Rt_Time_n100