rm(list=ls(all=TRUE))
library(xtable)

load("Table_2_FGM_n100.Rdata")
xtable(Table_2_FGM_Power_n100,digits=3)
Table_2_FGM_Time_n100