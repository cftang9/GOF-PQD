rm(list=ls(all=TRUE))
library(xtable)

load("Table_2_CA_n100.Rdata")
xtable(Table_2_CA_Power_n100,digits=3)
Table_2_CA_Time_n100